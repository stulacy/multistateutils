# Calculates transition probabilities for multiple individuals.
#
# Calculates transition probabilities for specified parameters from state
# occupancies.
# occupancy State occupancy data.table as returned by \code{state_occupancy}.
# state_names Character vector containing the names of the states.
# end_times Times at which to estimate transition probabilities.
#
# return A data frame in long format with transition probabilities for each individual,
# for each starting time, and for each ending time.
calculate_transition_probabilities <- function(occupancy, start_times, end_times, state_names, ci) {

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


    nstates <- length(state_names)
    keys <- c('individual', 'id')
    full_keys <- c('individual', 'start_time', 'end_time', 'start_state')
    # Formula used for counting number of individuals in each end state from every
    # independent variable combination
    LHS <- c('individual', 'start_time', 'end_time', 'start_state')

    if (ci) {
        keys <- c('simulation', keys)
        full_keys <- c('simulation', full_keys)
        LHS <- c('simulation', LHS)
    }

    count_form <- stats::as.formula(paste(paste(LHS, collapse='+'),
                                          'end_state', sep='~'))


    # Find state was in at start time
    # Obtain state that a person is in at starting times
    start_states <- occupancy[, .(start_state = state[findInterval(start_times, time)]),by=keys]
    start_states[, start_time := start_times ]

    # Obtain state that a person is in at all times want to calculate probabilities for
    end_states <- occupancy[, .(end_state = state[findInterval(end_times, time)]), by=keys]
    end_states[, end_time := end_times ]

    # Join the two tables together
    combined <- merge(start_states, end_states, on=keys, allow.cartesian=TRUE)

    # Calculate transition probabilities
    counts <- data.table::dcast(combined, count_form, value.var='end_time', length)
    end_state_names <- colnames(counts)[(ncol(counts)-nstates+1):ncol(counts)]

    # Calculate the number in the starting state at specified starting time
    counts[, num_start:=sum(.SD), .SDcols=end_state_names, by=full_keys]

    # Calculate proportions
    proportions <- counts[, lapply(.SD, function(x) x / num_start),
                          .SDcols=end_state_names,
                          by=full_keys]

    # Add state names for starting state column and as column names for ending states
    start_ind <- ncol(proportions) - nstates + 1
    data.table::setnames(proportions, c(names(proportions)[1:(start_ind-1)],
                                        state_names))
    proportions$start_state <- factor(proportions$start_state,
                                      levels=seq_along(state_names)-1,
                                      labels=state_names)
    proportions
}


#' Estimates transition probabilities
#'
#' Estimates transition probabilities of an individual's passage 
#' through a multi-state model
#' by discrete event simulation.
#'
#' @param models List of \code{flexsurvreg} objects.
#' @param newdata Data frame with covariates of individual to simulate times for. Must contain all fields
#'   required by models.
#' @param trans_mat Transition matrix, such as that used in \code{mstate}.
#' @param times Times at which to estimate transition probabilities. 
#' @param start_times Conditional time for transition probability.
#' @param tcovs As in \code{flexsurv::pmatrix.simfs}, this is the names of covariates that need to be
#'   incremented by the simulation clock at each transition, such as age when modelled as age at state entry.
#' @param N Number of times to repeat the individual
#' @param M Number of times to run the simulations in order to obtain confidence interval estimates.
#' @param ci Whether to calculate confidence intervals. See \code{flexsurv::pmatrix.simfs} for details.
#' @param ci_margin Confidence interval range to use if \code{ci} is set to \code{TRUE}.
#' @return A data frame with estimates of transition probabilities.
#' 
#' @examples 
#' 
#' library(multistateutils)
#' library(mstate)
#' library(flexsurv)
#' 
#' # Convert data to long
#' data(ebmt3)
#' tmat <- trans.illdeath()
#' long <- msprep(time=c(NA, 'prtime', 'rfstime'), 
#'                status=c(NA, 'prstat', 'rfsstat'), 
#'                data=ebmt3, 
#'                trans=tmat, 
#'                keep=c('age', 'dissub'))
#'                
#' # Fit parametric models
#' models <- lapply(1:3, function(i) {
#'     flexsurvreg(Surv(time, status) ~ age + dissub, data=long, dist='weibull')
#' })
#' 
#' # New individual to estimate transition probabilities for
#' newdata <- data.frame(age="20-40", dissub="AML")
#' 
#' # Estimate transition probabilties at 1 year
#' predict_transitions(models, newdata, tmat, times=365)
#' 
#' # Estimate transition probabilties at 1 year given we know they're alive after 6 months
#' predict_transitions(models, newdata, tmat, times=365, start_times = 365/2)
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
    # by all models are in newdata. Although want state-occupancy specific guards to
    # be in 'state_occupancy'

    # Calculate state occupancies
    occupancy <- state_occupancy(models, trans_mat, newdata, N, tcovs, ci, M)

    # Estimate transition probabilities, this will add 'simulation' as a key if used
    probs <- calculate_transition_probabilities(occupancy, start_times, times, 
                                                colnames(trans_mat), ci)

    if (ci) {
        # Make CIs
        # Form the unique indices and grab state names we're going to need for these summaries
        keys <- c('individual', 'start_time', 'end_time', 'start_state')
        states <- colnames(trans_mat)

        # Calculate CI limits
        ci_tail <- (1 - ci_margin) / 2
        ci_upper <- 1 - ci_tail
        ci_lower <- ci_tail

        # Obtain summaries
        means <- probs[, lapply(.SD, base::mean), .SDcols=states, by=keys]
        upper <- probs[, lapply(.SD, stats::quantile, ci_upper), .SDcols=states, by=keys]
        lower <- probs[, lapply(.SD, stats::quantile, ci_lower), .SDcols=states, by=keys]

        # Join together
        merge1 <- merge(means, lower, by=keys, suffixes=c('_est', paste0('_', ci_lower*100, '%')))
        merge2 <- merge(merge1, upper, by=keys)

        # Provide column names for upper CI
        colnames(merge2)[match(states, colnames(merge2))] <- paste0(states, paste0('_', ci_upper*100, '%'))

        probs  <- merge2
    }

    # Add in columns for each covariate name to replace the single 'individual' column
    separate_covariates(probs, colnames(newdata))

}