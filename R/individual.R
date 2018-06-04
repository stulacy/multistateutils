# Runs an individual level simulation.
#
# Returns a data.table with state occupancy times for a single individual.
individual_simulation <- function(transitions, newdata_mat, trans_mat, N, tcovs) {

    # Form new data into matrix
    mat_exp <- t(newdata_mat)[rep(1, N), ]
    initial_times <- rep(0, nrow(mat_exp))

    # Split people to evenly start in non-sink states.
    # Assign 1 person per sink state to get probability of 1
    is_sink <- apply(trans_mat, 1, function(col) all(col == 0))
    sink_states <- unname(which(is_sink) - 1)  # 0-index for c++
    non_sink <- setdiff(seq(ncol(trans_mat))-1, sink_states)

    # Form vector of starting states, one per individual uniformly distributed
    start_states_long <- sample(non_sink, N-length(sink_states), replace=T)
    start_states_long <- c(start_states_long, sink_states)

    if (is.null(tcovs))
        tcovs <- -1

    desCpp(transitions, trans_mat, mat_exp, initial_times, start_states_long, tcovs)
}

# Runs simulations for multiple individuals.
#
# Combines the data.tables for multiple individuals / simulations into
# a single table, with appropriate indexing.
#
# param covar_values Data frame comprising the covariate values so that neat labels can be formed
#   for the output probability data frame.
#
# @return A data.table with state occupancies and key column 'individual'.
multiple_simulations <- function(transitions, trans_mat, newdata_mat, N, tcovs, covar_values) {
    # Calculate state occupancy for each covariate pattern
    # Run simulation for each individual
    res <- lapply(1:nrow(newdata_mat), function(i) {
        individual_simulation(transitions, newdata_mat[i, ], trans_mat, N, tcovs)
    })

    # Obtain neat labels for results detailing individual attributes
    names(res) <- sapply(1:nrow(newdata_mat), function(i) {
        paste(paste(colnames(covar_values), covar_values[i, ], sep='='), collapse=',')
    })

    resDT <- data.table::rbindlist(lapply(res, data.table::as.data.table), idcol='individual')
    data.table::setnames(resDT, c('individual', 'id', 'state', 'time'))
}

# Determines state occupancy for individual level simulations.
#
# Returns a data table with state occupancy times for each individual,
# and also simulation if \code{ci} are requested.
state_occupancy <- function(models, trans_mat, newdata, N, tcovs, ci, M) {
    # Obtain attributes as a matrix
    attr_mat <- form_model_matrix(newdata, models)

    # Obtain covariate values of new individuals so that can form neat labels
    # of the output probabilities
    label_df <- data.frame(lapply(newdata, as.character), stringsAsFactors=FALSE)

    # Ensure that the transition matrix doesn't have NA values, replacing these with 0
    trans_mat[is.na(trans_mat)] <- 0

    # Find the column indices for time-dependent variables
    if (!is.null(tcovs))
        tcovs <- match(tcovs, colnames(attr_mat)) - 1  # 0-index for c++

    if (!ci) {
        # Convert models to list of transitions as required
        transition_list <- lapply(models, obtain_model_coef, attr_mat)
        multiple_simulations(transition_list, trans_mat, attr_mat, N, tcovs, label_df)
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
            multiple_simulations(transitions_per_sim[[m]], trans_mat, attr_mat, N, tcovs, label_df)
        })
        data.table::rbindlist(res, idcol='simulation')
    }
}
