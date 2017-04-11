curr_time_reactive <- reactiveValues(curr_time=0, next_time=0)

run_simulation_cpp <- function() {

    trans_mat <- Q()
    trans_mat[is.na(trans_mat)] <- 0  # C++ can't handle NA

    # Obtain entry times and attributes for incident individuals
    initial_times <- setup_eventlist_cpp()
    n_inds <- length(initial_times)
    raw_attrs <- data.frame(lapply(reactiveValuesToList(attributes), function(x) x$draw(n_inds)))
    new_data <- bind_rows(apply(raw_attrs, 1, convert_stringdata_to_numeric))

    # Obtain parameters for each distriubtion for each individual
    transition_list <- lapply(reactiveValuesToList(transitions), function(t) {
        list(name=DISTS[[t$dist]]$flex,
             params=as.matrix(calculate_parameters(t$params, new_data))
        )
    })

    raw_mat <- desCpp(transition_list, trans_mat, initial_times)
    history <- data.frame(raw_mat)
    colnames(history) <- c('id', 'state', 'time')
    history$id <- as.integer(history$id)
    history$state <- as.integer(history$state)
    history
}

create_event <- function(id, time, attributes) {
    obj <- list(history=data.frame(state=NULL, entry_time=NULL),
                curr_state = -1,
                next_state = 1,
                id = id,
                time = time,
                attributes = attributes
                #next_transition = NULL # These get filled out later on
                )
    class(obj) <- c(class(obj), 'event')
    obj
}
ERROR_MARGIN <- 1.25

output$termcriteriadiv <- renderUI({
    method <- input$terminationcriteria
    if (is.null(method)) {
        return(NULL)
    }

    textInput("termcriteriavalue", "Value of termination criteria", value=method)
})

output$timedisplay <- renderTable({
    res <- simoutput()

    if (is.null(res))
        return(NULL)

    # TODO Get this summary correctly printing again!

    #num_states <- length(states())
    #num_in_states <- sapply(res, function(sim) {
    #    colSums(sapply(seq(3), function(state) {
    #        sapply(sim, function(ind) ind$curr_state == state)
    #    } ))
    #})
    #mean_num_in_states <- rowMeans(num_in_states)
#
    #data.frame(state=seq(num_states), num=mean_num_in_states)
    res
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

# TODO This method currently only produces wide formatted CSVs, where each state is either visited or not.
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
        num_states <- length(states())
        full_output <- lapply(seq_along(res), function(sim_num) {
            if (input$terminationcriteria == 'Time limit') {
                censor_time <- input$termcriteriavalue
            } else if (input$terminationcriteria == 'Number of individuals') {
               censor_time <- max(sapply(res[[sim_num]], function(e) e$time))
            } else {
                message(paste0("Error: Unknown termination criteria '", input$terminationcriteria, "'."))
                return()
            }
            as.data.frame(t(sapply(seq_along(res[[sim_num]]), function(event_num) {
                # Determine patient individual attributes
                event <- res[[sim_num]][[event_num]]
                attrs <- c(event_num, unlist(event$attributes))

                in_absorbtive_state <- event$curr_state == event$next_state

                # Determine state transition times and censoring values
                event_occured <- sapply(seq(num_states), function(s) s %in% event$history[, 1])
                states_occured <- seq(num_states)[event_occured]
                status <- as.numeric(event_occured)
                times <- rep(NA, num_states)
                times[states_occured] <- sapply(states_occured, function(s) event$history[event$history[, 1] == s, 2])
                times[-states_occured] <- if (in_absorbtive_state) max(times, na.rm=T) else censor_time
                state_vals <- as.vector(sapply(seq(num_states), function(s) c(times[s], status[s])))
                output <- c(attrs, state_vals)
                output
                })))
            })
        full_output <- as.data.frame(data.table::rbindlist(full_output, idcol='sim_num', use.names=TRUE))
        num_cols <- ncol(full_output)
        state_names <- c(sapply(states(), function(s) c(paste0(s, '.time'), paste0(s, '.status'))))

        colnames(full_output)[(num_cols - num_states*2 + 1) : num_cols] <- state_names
        colnames(full_output)[2] <- 'event_id'

        # Format attributes into long format
        cat_attrs <- names(attributes)[sapply(names(attributes), function(a) attributes[[a]]$type == 'Categorical')]
        cont_attrs <- names(attributes)[sapply(names(attributes), function(a) attributes[[a]]$type == 'Continuous')]
        cat_levels <- setNames(lapply(cat_attrs, function(a) paste(a, attributes[[a]]$levels, sep='.')), cat_attrs)

        # Obtain factor value of categorical attributes
        cat_long <- apply(full_output, 1, function(row) {
                        setNames(lapply(cat_attrs, function(a) attributes[[a]]$levels[row[cat_levels[[a]]]==1]),
                                 cat_attrs)
                    })

        full_output <- full_output[, !(names(full_output) %in% unlist(cat_levels))] # Drop wide categorical variables
        full_output <- cbind(full_output, bind_rows(cat_long)) # Add long categorical variables

        # Reorder columns
        full_output <- full_output[, c('sim_num', 'event_id', cont_attrs, cat_attrs, state_names)]
        write.csv(full_output, file, quote=F, row.names = F)
    }
)

# TODO See if force is required here
create_next_transition <- function(starting_state, attrs) {
    force(starting_state)
    force(attrs)

    # Subset transitions where have specific starting state and draw method
    trans <- reactiveValuesToList(transitions)
    trans <- trans[sapply(trans, function(t) t$from == starting_state && !is.null(t$draw))]
    force(trans)

    function() {
        if (length(trans) == 0)
            return(NULL)

        times <- sapply(trans, function(t) t$draw(1, newdata=attrs))
        # Obtain next event as next time
        next_time <- which.min(times)
        # Return the next state and its time
        c(times[next_time], trans[[next_time]]$to)
    }

}

determine_next_transition <- function(current_state, id, params) {
    # Determines the next transition for a given id in a given state
    # This is calculated as the minimum of the next possible event times

    # TODO Have this calculated at the start of the simulation. All possible transitions
    # from a given state
    trans <- reactiveValuesToList(transitions)
    trans <- trans[vapply(trans, function(t) t$from == current_state && !is.null(t$draw), logical(1))]

    if (length(trans) == 0)
        return(NULL)

    times <- vapply(names(trans), function(t_) trans[[t_]]$draw(1, t(params[[t_]][id, ])), numeric(1))

    #times <- sapply(trans, function(t) t$draw(1, params=this_params))
    # Obtain next event as next time
    next_time <- which.min(times)
    # Return the next state and its time
    c(times[next_time], trans[[next_time]]$to)

}

# Function that runs when an event occurs. Creates the subsequent event and returns it
termination_time <- function(max_time) {
    function(eventlist) {
        # If have empty list or the next value is greater than the limit then terminate
        !((length(eventlist) >= 1) && (eventlist[[1]]$time <= max_time))
    }
}

termination_individuals <- function(eventlist) {
    length(eventlist) < 1
}

run_simulation <- function() {
    # Create initial event_list
    event_list <- setup_eventlist()

    newdata <- bind_rows(lapply(event_list, function(e) e$attributes))

    # Create transition probabilities
    all_parameters <- lapply(reactiveValuesToList(transitions), function(t) {
        calculate_parameters(t$params, newdata)
    })


    if (length(event_list) == 0) {
        message("Error in configuring event list. Please confirm all parameters are correct.")
        return()
    }
    absorbant_list <- list()


    # Setup termination criteria
    if (input$terminationcriteria == "Time limit") {
        end_simulation <- termination_time(as.numeric(input$termcriteriavalue))
    } else if (input$terminationcriteria == "Number of individuals") {
        end_simulation <- termination_individuals
    } else {
        message(paste0("Error: Unknown termination criteria option '", input$terminationcriteria, "'."))
        return()
    }

    # Peak at top of list and see time of next event
    while (!end_simulation(event_list)) {
        # Processes event on top of event_list and updates both lists
        new_state <- run_timestep(event_list, absorbant_list, all_parameters)
        event_list <- new_state[[1]]
        absorbant_list <- new_state[[2]]
    }
    append(event_list, absorbant_list)
}

setup_eventlist <- function() {
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

    # Create attributes for all individuals
    attrs <- data.frame(lapply(reactiveValuesToList(attributes), function(x) x$draw(n_inds)))
    attrs_numeric <- bind_rows(apply(attrs, 1, convert_stringdata_to_numeric))

    # Create Event objects containing:
    #   index
    #   time of entry into state 1
    #   attributes
    new_events <- lapply(seq(n_inds), function(i) {
        create_event(i, entry_times[i], attrs_numeric[i, ])
    })

    # Create functions to determine the next event for each of these
    #for (i in seq_along(new_events)) {
    #    new_events[[i]]$next_transition <- create_next_transition(new_events[[i]]$next_state, new_events[[i]]$attributes)
    #}
    new_events
}

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

process_event <- function(event, params) {
    # Create new event as copy of old
    new_event <- event
    # update current state
    new_event$curr_state <- event$next_state
    # Update history
    new_event$history <- rbind(new_event$history, c(event$next_state, event$time))

    # determine next state and time
    # TODO CHANGED
    next_trans <- determine_next_transition(new_event$curr_state, new_event$id, params)
   # next_trans <- event$next_transition()

    # If in an absorbant state, return state as it is
    if (is.null(next_trans)) {
        return(new_event)
    }

    # update next state and time (time will be current time + time to next event)
    new_event$time <- next_trans[1] + event$time
    new_event$next_state <- next_trans[2]
    # set next_transition
    new_event$next_transition <- create_next_transition(new_event$next_state, new_event$attributes)

    new_event

}


run_timestep <- function(event_list, absorbant_list, transition_params) {
    nevent_list <- event_list
    nabsorbant_list <- absorbant_list

    # Pop the current event from top of list
    curr_event <- nevent_list[[1]]
    nevent_list[[1]] <- NULL
    curr_time <- curr_event$time

    # Process the current event to obtain its next transition
    new_event <- process_event(curr_event, transition_params)

    # If not in an absorbant state and have a next transition then add to queue
    if (new_event$time > curr_time) {
        # Insert new_event into appropriate place
        times <- vapply(nevent_list, function(e) e$time, numeric(1))
        # -1 as append below is the index AFTER which to insert value at
        new_index <- if (length(times) == 0 || new_event$time < min(times)) {
                        0
                     } else if (new_event$time > max(times)) {
                        length(times) + 1
                     } else {
                        min(which(times - new_event$time > 0)) - 1
                     }
        nevent_list <- append(nevent_list, list(new_event), new_index)
    } else {
        nabsorbant_list <- append(nabsorbant_list, list(new_event), length(nabsorbant_list)+1)
    }

    # Update times for display
    curr_time_reactive$curr_time <- curr_event$time
    curr_time_reactive$next_time <- if (length(nevent_list) > 0) nevent_list[[1]]$time else NA

    # Return updated event lists
    list(nevent_list, nabsorbant_list)
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