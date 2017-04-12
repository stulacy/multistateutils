run_simulation_cpp <- function() {

    trans_mat <- Q()
    trans_mat[is.na(trans_mat)] <- 0  # C++ can't handle NA

    # Obtain entry times and attributes for incident individuals
    initial_times <- setup_eventlist_cpp()
    n_inds <- length(initial_times)
    raw_attrs <- data.frame(lapply(reactiveValuesToList(attributes), function(x) x$draw(n_inds)))
    new_data <- bind_rows(apply(raw_attrs, 1, convert_stringdata_to_numeric))

    # Obtain parameters for each distribution for each individual
    transition_list <- lapply(reactiveValuesToList(transitions), function(t) {
        list(name=DISTS[[t$dist]]$flex,
             params=as.matrix(calculate_parameters(t$params, new_data))
        )
    })

    # Line that runs the simulation
    raw_mat <- desCpp(transition_list, trans_mat, initial_times)

    history <- data.frame(raw_mat)
    colnames(history) <- c('id', 'state', 'time')
    history$id <- as.factor(history$id + 1) # Convert back to 1-based index
    history$state <- as.integer(history$state)

    # Add patient attribute information to the results
    raw_attrs$id <- as.factor(seq(n_inds))
    total_results <- inner_join(history, raw_attrs, by='id')
    total_results
}

ERROR_MARGIN <- 1.25

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

    # Returns a K x N matrix where N = # sims and K = # states indicating number of individuals ending in each state
    num_states <- length(states())
    sim_end_states <- sapply(res, function(sim) {
             end_states <- sim %>% group_by(id) %>% summarise(end_state = state[which.max(time)])
             end_states_table <- table(end_states$end_state)
             foo <- sapply(seq(num_states), function(state) {
                 if (is.na(end_states_table[as.character(state)])) 0 else end_states_table[as.character(state)]
             })
             foo
    })
    mean_num_in_states <- rowMeans(sim_end_states)

    item_list <- list(h4("Average state occupancy"),
                      renderTable(data.frame(state=states(), num=mean_num_in_states))
    )
    do.call(tagList, item_list)
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

    # TODO Get this working with rcpp implementation!
    content = function(file) {
        res <- simoutput()
        this_states <- states()

        num_states <- length(this_states)
        sinks <- sink_states()
        state_time_cols <- paste(this_states, 'time', sep='.')

        full_output_raw <- lapply(res, function(sim) {
            with_times <- sim %>%
                            mutate(state_lab = this_states[state+1]) %>%
                            select(-state) %>%
                            spread(key=state_lab, value=time)

            # Censor all times greater than max sim time if using a time limit
            if (input$terminationcriteria == "Time limit") {
            with_times <- with_times %>%
                            gather_("state", "time", this_states) %>%
                            mutate(time=ifelse(time >= input$termcriteriavalue, NA, time)) %>%
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
                        censor_time <- input$termcriteriavalue
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

        browser()

        full_output_comb <- as.data.frame(data.table::rbindlist(full_output, idcol='sim_num', use.names=TRUE))
        write.csv(full_output_comb, file, quote=F, row.names = F)
    }
)

setup_eventlist_cpp <- function() {
    entryrate <- tryCatch(entryrate <- as.numeric(input$entryrate),
                          warning=function(w) {
                                 message("Error: Please provide a numeric value for entry rate.")
                                 return(NULL)
                          })
    termcriteria <- tryCatch(as.numeric(input$termcriteriavalue),
                             warning=function(w) {
                                 message("Error: Please provide a numeric value for termination criteria.")
                                 return(NULL)
                             })

    if (is.null(entryrate) || is.null(termcriteria))
        return()

    if (input$terminationcriteria == "Time limit") {
        # If specify time limit then number of individuals is rate * time limit, plus an error margin
        initial_n <- ERROR_MARGIN * (entryrate * termcriteria)
    } else if (input$terminationcriteria == "Number of individuals") {
        # Otherwise can specify number of individuals directly
        initial_n <- termcriteria
    } else {
        message(paste0("Error: Unknown termination criteria option '", input$terminationcriteria, "'."))
    }

    entry_times <- cumsum(rexp(initial_n, entryrate))
    # Remove values which are above maximum time if using one
    if (input$terminationcriteria == "Time limit") {
        entry_times <- entry_times[entry_times < termcriteria]
    }
    n_inds <- length(entry_times)

    if (n_inds == 0)
        return(NULL)

    entry_times
}

simoutput <- eventReactive(input$runmultiplesimbutton, {
    n_sims <- input$simslider
    withProgress(message="Simulating...", value=0, {
        print(system.time({
            platform <- .Platform$OS.type

            # TODO Changed from CPP to R implementation
            if (platform == "unix" && n_sims > 1) {
                end_states <- mclapply(seq(n_sims), function(i) run_simulation_cpp(),
                                       mc.cores=4)
            } else {
                end_states <- lapply(seq(n_sims), function(i) run_simulation_cpp())
            }
        }))
    })
    end_states
})