#' Runs an individual level simulation.
#'
#' Returns a data frame with state occupancy times for a single individual.
#'
#' @param transitions A list of transitions as required by \code{desCpp}.
#' @param newdata_mat New data already in data matrix format as a single row
#' @param trans_mat Transition matrix
#' @param N Number of times to replicate this individual.
#' @param tcovs Indices of covariates that are time-dependent and need to have the time
#'   since simulation start added onto them at each intermediate state entry. Default is NULL.
#' @return A data frame with entry times for each state that is entered.
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