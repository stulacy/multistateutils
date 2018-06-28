# Determines state occupancy for the given individuals and transition model.
#
# Returns a data table with state occupancy times for each individual,
# and also simulation if \code{ci} are requested.
state_occupancy <- function(models, trans_mat, newdata, tcovs, start_times, start_states, ci, M) {
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

    res <- if (!ci) {
        # Convert models to list of transitions as required
        transition_list <- lapply(models, obtain_model_coef, attr_mat)
        run_sim(transition_list, attr_mat, trans_mat, tcovs, start_times, start_states)
        #multiple_simulations(transition_list, trans_mat, attr_mat, tcovs, label_df)
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
            #multiple_simulations(transitions_per_sim[[m]], trans_mat, attr_mat, tcovs, label_df)
            run_sim(transitions_per_sim[[m]], attr_mat, trans_mat, tcovs, start_times, start_states)
        })
        data.table::rbindlist(res, idcol='simulation')
    }
    
    # Obtain unique identifier for each individual from their covariate values 
    # This just creates an N x p matrix with cols varp=foo, varp+1=bar, ...
    raw_names <- sapply(seq(ncol(newdata)), function(i) paste(colnames(newdata)[i], 
                                                              newdata[, i], 
                                                              sep='='))
    ind_id <- apply(raw_names, 1 , paste, collapse=",")
    res[, individual := ind_id[id + 1]]
    # Now link back to this by the ID column, which is zero-indexed
    # Then want to combine these delimited by commas into a single string
    res
}

run_sim <- function(transitions, newdata_mat, trans_mat, tcovs, start_times, start_states) {
    if (is.null(tcovs))
        tcovs <- -1

    res <- data.table::as.data.table(desCpp(transitions, trans_mat, newdata_mat, 
                                     start_times, start_states, tcovs))
    data.table::setnames(res, c('id', 'state', 'time'))
    res
}