output$termcriteriadiv <- renderUI({
    method <- input$terminationcriteria
    if (is.null(method)) {
        return(NULL)
    }

    textInput("termcriteriavalue", "Value of termination criteria", value=method)
})

output$simendstates <- renderUI({
    res <- simoutput()

    if (is.null(res))
        return(NULL)

    isolate({

        withProgress(message="Summarising simulation...", value=0.3, {

            # Returns a K x N matrix where N = # sims and K = # states indicating number of individuals ending in each state
            num_states <- length(states())
            sim_end_states <- sapply(res, function(sim) {

                 if (input$terminationcriteria == "Time limit") {
                     sim <- filter(sim, time < as.numeric(input$termcriteriavalue))
                 }
                 end_states <- sim %>%
                             group_by(id) %>%
                             summarise(end_state = state[which.max(time)])
                 end_states_table <- table(end_states$end_state)

                 # This checks for states with zero occupancy
                 corrected_missing <- sapply(seq(num_states), function(state) {
                     if (is.na(end_states_table[as.character(state-1)])) 0 else end_states_table[as.character(state-1)] # Convert from 0 based index
                 })
                 corrected_missing
            })

            mean_num_in_states <- rowMeans(sim_end_states)

            item_list <- list(h4("Average state occupancy"),
                              renderTable(data.frame(state=states(), num=mean_num_in_states))
            )
            do.call(tagList, item_list)
        })
    })
})

output$savebuttons <- renderUI({
    res <- simoutput()

    if (is.null(res))
        return(NULL)

    fluidRow(
        hr(),
        textInput("simulationname", "Simulation name"),
        downloadButton("savemodel", "Save model"),
        downloadButton("saveresults", "Save results")
    )
})

output$savemodel <- downloadHandler(
    filename = function() {
        paste0(input$simulationname, '_model.json')
    },
    content = function(file) {
        output <- list(
           'transitions' = lapply(reactiveValuesToList(transitions), function(t) {
                   list('source' = states()[t$from],
                        'target' = states()[t$to],
                        'distribution' = t$dist,
                        'parameters' = t$params)
                        }),
           'simulation_parameters' = list('termination_criteria'=input$terminationcriteria,
                                          'termination_value'=input$termcriteriavalue,
                                          'entry_rate'=input$entryrate)
        )
        write(toJSON(output), file)
    }
)

# This method currently only produces wide formatted CSVs, where each state is either visited or not.
# This doesn't allow for recurrent visits to states, even though my DES model allows this to happen.
# I should really change the output of the DES to better reflect this, rather than forcing it into the
# wide mstate CSV method.
#
# Maybe just return a long data frame with a row referring to a transitions with the following columns:
#   - sim number
#   - individual number
#   - time
#   - start state
#   - end state
#
# I could then write wrappers to convert it to mstate style wide tables, or long mstate transition tables
# But essentially, since there are so many possibilities of what the user would want to do with the
# simulation results, it's probably better to provide it as raw as possible and let them do the manual
# tweaking to obtain the required format for their specific analysis
output$saveresults <- downloadHandler(
    filename = function() {
        paste0(input$simulationname, '_results.csv')
    },

    content = function(file) {
        res <- simoutput()
        n_sims <- length(res)

        withProgress(message="Collating results", value=0, max=n_sims, {

            this_states <- states()

            num_states <- length(this_states)
            sinks <- sink_states()
            state_time_cols <- paste(this_states, 'time', sep='.')

            full_output_raw <- lapply(seq(n_sims), function(i) {
                incProgress(1, detail=paste("simulation", i))

                sim <- res[[i]]
                with_times <- sim %>%
                                mutate(state_lab = this_states[state+1]) %>%
                                select(-state) %>%
                                spread(key=state_lab, value=time)

                # Censor all times greater than max sim time if using a time limit
                if (input$terminationcriteria == "Time limit") {
                with_times <- with_times %>%
                                gather_("state", "time", this_states) %>%
                                mutate(time=ifelse(time >= as.numeric(input$termcriteriavalue), NA, time)) %>%
                                spread(state, time)
                }

                # Add status for censoring
                with_status <- with_times %>%
                    gather_("state", "time", this_states) %>%
                    mutate(status=as.numeric(!is.na(time))) %>%
                    gather_("variable", "value", c('time', 'status')) %>%
                    unite(temp, state, variable, sep='.') %>%
                    spread(temp, value)

                # Determine censoring times for any unobserved state entries. These are individual-specific
                apply(with_status, 1, function(row) {
                    sink_reached <- sapply(sinks, function(state) row[[paste(state, 'status', sep='.')]] == 1)
                    if (any(sink_reached)) {
                        # Should only be only sink reached, but have max just in case
                        censor_time <- max(sapply(sinks[sink_reached], function(state) row[[paste(state, 'time', sep='.')]]))
                    } else {
                        if (input$terminationcriteria == 'Time limit') {
                            censor_time <- as.numeric(input$termcriteriavalue)
                        } else {
                            censor_time <- max(sim$time)
                        }
                    }

                    # Set any censored state entries to the appropriate time
                    row[is.na(row)] <- censor_time
                    row
                })
            })

            full_output <- lapply(full_output_raw, function(df) as.data.frame(t(df)))

            full_output_comb <- as.data.frame(data.table::rbindlist(full_output, idcol='sim_num', use.names=TRUE))
            write.csv(full_output_comb, file, quote=F, row.names = F)
        })
    }
)

simoutput <- eventReactive(input$runmultiplesimbutton, {
    n_sims <- input$simslider
    platform <- .Platform$OS.type

    # Put this in function somewhere?
    # Guard inputs
    entry_rate <- tryCatch(as.numeric(input$entryrate),
                           warning=function(w) {
                                 message("Error: Please provide a numeric value for entry rate.")
                                 return(NULL)
                          })
    termination_value <- tryCatch(as.numeric(input$termcriteriavalue),
                                  warning=function(w) {
                                     message("Error: Please provide a numeric value for termination criteria.")
                                     return(NULL)
                             })
    termination_method <- input$terminationcriteria
    if (is.null(entry_rate) || is.null(termination_value))
        return()

    # Raw number of individuals
    num_inds <- calculate_number_individuals(entry_rate, termination_method, termination_value)
    censor_time <- if (termination_method == "Time limit") termination_value else NULL

    # Setup transition matrix
    trans_mat <- Q()
    trans_mat[is.na(trans_mat)] <- 0  # C++ can't handle NA

    withProgress(message="Running simulations", value=0, max=n_sims, {
        #print(system.time({
            if (platform == "unix" && n_sims > 1) {
                end_states <- mclapply(seq(n_sims), function(i) {
                        incProgress(1, detail=paste(i))
                        run_simulation_cpp(trans_mat, num_inds, entry_rate, censor_time,
                                           reactiveValuesToList(attributes), reactiveValuesToList(transitions))
                    },
                    mc.cores=4)
            } else {
                end_states <- lapply(seq(n_sims), function(i) {
                        incProgress(1, detail=paste(i))
                        run_simulation_cpp(trans_mat, num_inds, entry_rate, censor_time,
                                           reactiveValuesToList(attributes), reactiveValuesToList(transitions))
                    })
            }
        #}))
    })

    end_states
})