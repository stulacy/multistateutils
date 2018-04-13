#' Runs an individual level simulation
#'
#' @param transitions A list of transitions as required by \code{desCpp}.
#' @param newdata_mat New data already in data matrix format as a single row
#' @param trans_mat Transition matrix
#' @param N Number of times to replicate this individual.
#' @return Usual output from DES, with state entry times
individual_simulation <- function(transitions, newdata_mat, trans_mat, N) {
# TODO Do I want to keep this function or just make a separate one for obtaining transition
# probabilities and length of stay? Could have this work for a single individual and then have
# a higher level function like 'pmatrix.simfs' that loops through it for each new individual
# and extracts probabilities at each timepoint. I think the latter and keep this internal

    # Form new data into matrix
    mat_exp <- t(newdata_mat)[rep(1, N), ]
    initial_times <- rep(0, nrow(mat_exp))

    # Determine how many starting states there are
    is_start <- apply(trans_mat, 2, function(col) all(col == 0))
    start_states <- unname(which(is_start) - 1)  # 0-index for c++

    # Turn this into vector of starting states, one per individual uniformly distributed
    start_states_long <- sample(start_states, N, replace=T)

    desCpp(transitions, trans_mat, mat_exp, initial_times, start_states_long)
}

#' Estimates transition probabilities and length of stay
#'
#' Estimates various measures of an individual's passage through a multi-state model
#' by discrete event simulation.
#'
#' @family cohort_simulation
#' @param times Times at which to estimate transition probabilities. If not provided then doesn't estimate
#'   transition probabilities, just length of stay.
#' @param states States at which to estimate length of stay. Can either be integers, or the
#'   names used by \code{trans_mat}. If not provided then defaults to all states.
#' @param start_states Conditional states at which to estimate length of stay. Can either be integers, or the
#'   names used by \code{trans_mat}. If not provided then defaults to all states.
#' @param start_times Conditional time for transition probability.
#' @param N Number of times to repeat the individual
#' @param M Number of times to run the simulations in order to obtain confidence estimates
#' @export
#' @return A list for each individual with items for length of stay and transition probabilities.
predict_transitions <- function(models, newdata, trans_mat, times=NULL, states=NULL, start_times=0, start_states=NULL, N=10000, M=1000) {

    if (ncol(trans_mat) != nrow(trans_mat)) {
        stop(paste0("Error: trans_mat has differing number of rows and columns (",
                    nrow(trans_mat), " and ",
                    ncol(tran_mat), ")."))
    }

    # Obtain attributes as a matrix
    attr_mat <- form_model_matrix(newdata, models)

    # Convert models to list of transitions as required
    transition_list <- lapply(models, obtain_model_coef, attr_mat)

    # Ensure that the transition matrix doesn't have NA values, replacing these with 0
    trans_mat[is.na(trans_mat)] <- 0

    # Run simulation for each individual
    res <- lapply(1:nrow(newdata), function(i) {
        individual_simulation(transition_list, attr_mat[i, ], trans_mat, N)
    })

    # Obtain neat labels for results detailing individual attributes
    label_df <- data.frame(lapply(newdata, as.character), stringsAsFactors=FALSE)
    names(res) <- sapply(1:nrow(newdata), function(i) {
        paste(paste(colnames(label_df), label_df[i, ], sep='='), collapse=',')
    })

    res

    # TODO Work out best data structure to hold all results

    #if (!is.null(times)) {
    #    # TODO Calculate state transition probabilities at times
    #}
#
    #if (is.null(states)) {
    #    states <- 1:ncol(trans_mat)
    #}
#
    ## Convert state names to indices
    #if (is.character(states)) {
    #    states <- match(states, colnames(trans_mat))
    #}

    # TODO Calculate length of stay for each state in 'states'

    # TODO Add in repeating whole thing M times to obtain standard errors

}