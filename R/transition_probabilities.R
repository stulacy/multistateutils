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
#' @param ci_margin Confidence interval range to use if \code{ci} is set to \code{TRUE}.
#' @return A list for each individual with items for length of stay and transition probabilities.
#'
#' @importFrom magrittr '%>%'
#' @import data.table
#' @export
predict_transitions <- function(models, newdata, trans_mat, times,
                                start_times=0, tcovs=NULL, N=1e5, M=1e3, ci=FALSE,
                                ci_margin=0.95) {

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

    # Ensure that the transition matrix doesn't have NA values, replacing these with 0
    trans_mat[is.na(trans_mat)] <- 0

    # Find the column indices for time-dependent variables
    if (!is.null(tcovs))
        tcovs <- match(tcovs, colnames(attr_mat)) - 1  # 0-index for c++

    ###### TODO! End prep function here

    out <- if (!ci) {
        # Convert models to list of transitions as required
        transition_list <- lapply(models, obtain_model_coef, attr_mat)

        calculate_transition_probabilities(attr_mat, transition_list, trans_mat, N, tcovs,
                                           start_times, times, label_df)
    } else {
        # This function returns a list with each transition as the highest level item,
        # then followed by the M simulations. We want the opposite.
        transition_list <- lapply(models, obtain_model_coef, attr_mat, M=M)
        transitions_per_sim <- lapply(1:M, function(sim) {
            lapply(seq_along(models), function(m) {
                transition_list[[m]][[sim]]
            })
        })
        sims <- lapply(1:M, function(i) {
              calculate_transition_probabilities(attr_mat, transitions_per_sim[[i]], trans_mat, N, tcovs,
                                                 start_times, times, label_df)
        })
        # Make CIs
        # Combine all tables together with a simulation index
        foo <- data.table::rbindlist(lapply(sims, data.table::as.data.table), idcol="simulation")
        # Form the unique indices and grab state names we're going to need for these summaries
        keys <- c(colnames(newdata), 'start_time', 'end_time', 'start_state')
        states <- colnames(trans_mat)

        # Calculate CI limits
        ci_tail <- (1 - ci_margin) / 2
        ci_upper <- 1 - ci_tail
        ci_lower <- ci_tail

        # Obtain summaries
        means <- foo[, lapply(.SD, mean), .SDcols=states, by=keys]
        upper <- foo[, lapply(.SD, quantile, ci_upper), .SDcols=states, by=keys]
        lower <- foo[, lapply(.SD, quantile, ci_lower), .SDcols=states, by=keys]

        # Join together
        merge1 <- merge(means, lower, by=keys, suffixes=c('_est', paste0('_', ci_lower*100, '%')))
        merge2 <- merge(merge1, upper, by=keys)
        colnames(merge2)[match(states, colnames(merge2))] <- paste0(states, paste0('_', ci_upper*100, '%'))
        as.data.frame(merge2)
    }



}