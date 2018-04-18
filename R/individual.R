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


#' Calculates transition probabilities for multiple individuals.
#'
#' Uses an already formatted set of arguments to run the individual level
#' simulation for each new individual (whereas \code{individual_simulation} only accepts 1 person)
#' and derives transition probabilities from the resulting state occupancies.
#' @inheritParams individual_simulation
#' @param times Times at which to estimate transition probabilities. If not provided then doesn't estimate
#'   transition probabilities, just length of stay.
#' @param covar_values Data frame comprising the covariate values so that neat labels can be formed
#'   for the output probability data frame..
#'
#' @return A data frame in long format with transition probabilities for each individual,
#' for each starting time, and for each ending time.
calculate_transition_probabilities <- function(newdata_mat, transitions, trans_mat, N, tcovs,
                                               start_times, end_times, covar_values) {

    # Calculate state occupancy for each covariate pattern
    # Run simulation for each individual
    res <- lapply(1:nrow(newdata_mat), function(i) {
        individual_simulation(transitions, newdata_mat[i, ], trans_mat, N, tcovs)
    })

    # Obtain neat labels for results detailing individual attributes
    names(res) <- sapply(1:nrow(newdata_mat), function(i) {
        paste(paste(colnames(covar_values), covar_values[i, ], sep='='), collapse=',')
    })

    state_names <- colnames(trans_mat)
    nstates <- nrow(trans_mat)

    resDT <- data.table::rbindlist(lapply(res, data.table::as.data.table), idcol='individual')
    data.table::setnames(resDT, c('individual', 'id', 'state', 'time'))

    # Find state was in at start time
    # Obtain state that a person is in at starting times
    start_states <- resDT[, .(start_state = state[findInterval(start_times, time)]),by=.(individual, id)]
    start_states[, start_time := start_times ]

    # Obtain state that a person is in at all times want to calculate probabilities for
    end_states <- resDT[, .(end_state = state[findInterval(end_times, time)]), by=.(individual, id)]
    end_states[, end_time := end_times ]

    # Join the two tables together
    combined <- merge(start_states, end_states, on=c('individual', 'id'), allow.cartesian=TRUE)

    # Calculate transition probabilities
    counts <- data.table::dcast(combined, individual + start_time + end_time + start_state ~ end_state,
                                value.var='end_time', length)
    end_state_names <- colnames(counts)[5:ncol(counts)]  # First 4 columns are individual, start_time, end_time, start_state
    # Calculate the number in the starting state at specified starting time
    counts[, num_start:=sum(.SD), .SDcols=end_state_names, by=.(start_time, end_time, start_state, individual)]

    # Calculate proportions
    proportions <- counts[, lapply(.SD, function(x) x / num_start),
                          .SDcols=end_state_names,
                          by=.(individual, start_time, end_time, start_state)]
    proportions_df <- as.data.frame(proportions)

    # Split individual into columns
    covars <- colnames(covar_values)
    proportions_df <- proportions_df %>%
        tidyr::separate(individual, sep=',', into=covars)

    proportions_df[covars] <- lapply(proportions_df[covars],
                                     function(x) gsub("[[:alnum:]]+=", "", x))

    proportions_df <- proportions_df %>%
                            dplyr::mutate(start_state = factor(start_state,
                                                               levels=seq_along(state_names)-1,
                                                               labels=state_names))
    colnames(proportions_df)[ (ncol(proportions_df) - nstates + 1) : ncol(proportions_df) ] <- state_names
    proportions_df
}


#' Estimates transition probabilities and length of stay
#'
#' Estimates various measures of an individual's passage through a multi-state model
#' by discrete event simulation.
#'
#' @param models List of \code{flexsurvreg} objects.
#' @param newdata Data frame with covariates of individual to simulate times for. Must contain all fields
#'   required by models.
#' @param trans_mat Transition matrix, such as that used in \code{mstate}.
#' @param times Times at which to estimate transition probabilities. If not provided then doesn't estimate
#'   transition probabilities, just length of stay.
#' @param start_times Conditional time for transition probability.
#' @param tcovs As in \code{flexsurv::pmatrix.simfs}, this is the names of covariates that need to be
#'   incremented by the simulation clock at each transition, such as age when modelled as age at state entry.
#' @param N Number of times to repeat the individual
#' @param M Number of times to run the simulations in order to obtain confidence interval estimates.
#' @param ci Whether to calculate confidence intervals. See \code{flexsurv::pmatrix.simfs} for details.
#' @export
#' @return A list for each individual with items for length of stay and transition probabilities.
#'
#' @importFrom magrittr '%>%'
#' @import data.table
predict_transitions <- function(models, newdata, trans_mat, times,
                                start_times=0, tcovs=NULL, N=1e5, M=1e3, ci=FALSE) {

    if (ncol(trans_mat) != nrow(trans_mat)) {
        stop(paste0("Error: trans_mat has differing number of rows and columns (",
                    nrow(trans_mat), " and ",
                    ncol(trans_mat), ")."))
    }

    if (any(sapply(start_times, function(s) s > times)))
        stop("Error: 'start_times' must be earlier than any value in 'times'.")

    # TODO More guards! Check nature of trans_mat, check that covariates required
    # by all models are in newdata

    # Required by CRAN checks
    id <- NULL
    state <- NULL
    time <- NULL
    start_time <- NULL
    end_time <- NULL
    individal <- NULL
    num_start <- NULL
    start_state <- NULL
    individual <- NULL

    ###### TODO! Make this one function to prepare inputs, so that can reuse it for LoS

    # Obtain attributes as a matrix
    attr_mat <- form_model_matrix(newdata, models)

    # Obtain covariate values of new individuals so that can form neat labels
    # of the output probabilities
    label_df <- data.frame(lapply(newdata, as.character), stringsAsFactors=FALSE)

    # Convert models to list of transitions as required
    transition_list <- lapply(models, obtain_model_coef, attr_mat)

    # Ensure that the transition matrix doesn't have NA values, replacing these with 0
    trans_mat[is.na(trans_mat)] <- 0

    # Find the column indices for time-dependent variables
    if (!is.null(tcovs))
        tcovs <- match(tcovs, colnames(attr_mat)) - 1  # 0-index for c++

    ###### TODO! End prep function here


    ###### TODO! Start function here to run simulation and return transition probabilities
    probs <- calculate_transition_probabilities(attr_mat, transition_list, trans_mat, N, tcovs,
                                                start_times, times, label_df)
    probs

    # TODO Add in repeating whole thing M times to obtain standard errors. Could be easier to just
    # use flexsurv's normbootn function
    #normboot.flexsurvreg(models[[1]], B=3, transform = T, raw=T)

}