# Determines state occupancy for the given individuals and transition model.
#
# Returns a data table with state occupancy times for each individual,
# and also simulation if \code{ci} are requested.
state_occupancy <- function(models, trans_mat, newdata, tcovs, start_times, start_states, 
                            ci, M, agelimit, agecol, agescale) {
    
    # Add oldage state to transmat if needed
    if (is.numeric(agelimit)) {
        states <- colnames(trans_mat)
        sinks <- get_sink_states(trans_mat)
        num_non_sinks <- sum(!states %in% sinks)
        num_trans <- max(trans_mat, na.rm=T)
        trans_mat <- cbind(trans_mat, c(seq(num_trans+1, num_trans+num_non_sinks), NA))
        trans_mat <- rbind(trans_mat, NA)
        
        # Find new state name
        if (! "oldage" %in% states) {
            name <- "oldage"
        } else {
            name <- "oldage_msmutils"
        }
        colnames(trans_mat) <- c(states, name)
        rownames(trans_mat) <- c(states, name)
        
        # Add oldage model to list of transition models 
        oldage_mod <- list(col=agecol, scale=agescale,
                           limit=agelimit)
        class(oldage_mod) <- c('oldage')
        oldage_mod$concat.formula <- list('dummy')
        attr(oldage_mod$concat.formula, "covnames") <- agecol
        for (i in seq(num_non_sinks)) {
            models[[length(models)+1]] <- oldage_mod
        }
    }
    
    # Obtain attributes as a matrix
    attr_mat <- form_model_matrix(newdata, models)
    
    # Ensure that the transition matrix doesn't have NA values, replacing these with 0
    trans_mat[is.na(trans_mat)] <- 0

    # Find the column indices for time-dependent variables
    if (!is.null(tcovs))
        tcovs <- match(tcovs, colnames(attr_mat)) - 1  # 0-index for c++

    res <- if (!ci) {
        # Convert models to list of transitions as required
        transition_list <- lapply(models, obtain_model_coef, attr_mat)
        run_sim(transition_list, attr_mat, trans_mat, tcovs, start_times, start_states)
    } else {
        # obtain_model_coef returns a list with each transition as the highest level item,
        # then followed by the M simulations. We want the opposite.
        transition_list <- lapply(models, obtain_model_coef, attr_mat, M=M)
        transitions_per_sim <- lapply(1:M, function(sim) {
            lapply(seq_along(models), function(m) {
                transition_list[[m]][[sim]]
            })
        })
        res <- lapply(1:M, function(m) {
            run_sim(transitions_per_sim[[m]], attr_mat, trans_mat, tcovs, start_times, start_states)
        })
        data.table::rbindlist(res, idcol='simulation')
    }
    res
}

run_sim <- function(transitions, newdata_mat, trans_mat, tcovs, start_times, start_states) {
    
    # CMD CHECK
    state <- NULL
    
    if (is.null(tcovs))
        tcovs <- -1
    
    res <- data.table::as.data.table(desCpp(transitions, trans_mat, newdata_mat, 
                                     start_times,
                                     start_states - 1,   # C++ is 0-indexed
                                     tcovs))
    data.table::setnames(res, c('id', 'state', 'time'))
    state_names <- colnames(trans_mat)
    res[, state := state_names[state+1]]  # Convert state into human readable names!
    setattr(res$state, "levels", state_names)
    res
}