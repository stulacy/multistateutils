# Calculates transition probabilities for multiple individuals.
#
# Calculates transition probabilities for specified parameters from state
# occupancies.
# occupancy State occupancy data.table as returned by \code{state_occupancy}.
# end_times Times at which to estimate transition probabilities.
# ci Boolean whether to calculate confidence intervals or not.
#
# return A data frame in long format with transition probabilities for each individual,
# for each starting time, and for each ending time.
calculate_transition_probabilities <- function(occupancy, start_times, end_times, ci) {

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

    # Big assumption that every state is visited...
    # But couldn't find a cleaner way of adding in old age without meaning
    # that every new function has to manually update the transition matrix in their functions.
    # Whereas this current method means that they dont have to worry about it as its done only in
    # run_simulation.
    state_names <- levels(occupancy$state)

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
    start_states[, start_time := rep(start_times, nrow(start_states)/length(start_times)) ]

    # Obtain state that a person is in at all times want to calculate probabilities for
    end_states <- occupancy[, .(end_state = state[findInterval(end_times, time)]), by=keys]
    end_states[, end_time := rep(end_times, nrow(end_states)/length(end_times)) ]

    # Join the two tables together
    combined <- merge(start_states, end_states, on=keys, allow.cartesian=TRUE)

    # Calculate transition probabilities
    counts <- data.table::dcast(combined, count_form, value.var='end_time', length)
    end_state_names <- colnames(counts)[seq(length(full_keys)+1, ncol(counts))]

    # Calculate the number in the starting state at specified starting time
    counts[, num_start:=sum(.SD), .SDcols=end_state_names, by=full_keys]

    # Calculate proportions
    proportions <- counts[, lapply(.SD, function(x) x / num_start),
                          .SDcols=end_state_names,
                          by=full_keys]

    # Set startstate column to be ordered in state order, and likewise destination
    # state order
    start_states_used <- intersect(state_names, unique(proportions$start_state))
    end_states_unused <- setdiff(state_names, end_state_names)
    if (length(end_states_unused) > 0) {
        proportions[, (end_states_unused) := 0.0]
    }
    proportions[, start_state := factor(start_state, levels=start_states_used)]
    data.table::setcolorder(proportions, c(colnames(proportions)[seq(ncol(proportions)-nstates)],
                                           state_names))

    setorder(proportions, individual, start_time, end_time, start_state)
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
#' @param agelimit Whether to automatically assign people to an 'early death' state.
#'   This is useful as otherwise individuals can be assigned unrealistic time-to-events due to the
#'   nature of sampling times from a random number distribution.
#'   If this value is \code{FALSE} then no limit is applied, otherwise provide the time-limit
#'   to be used. This limit must be in the same time-scale as the time-to-event models.
#' @param agescale Any multiplication to be applied to the age covariate to put it onto the same
#'   time-scale as the simulation. This is often useful as time-to-event may be measured on a day-based
#'   time-scale while age is typically measured in years.
#' @param agecol The name of the column in \code{newdata} that holds an individual's age.
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
                                ci_margin=0.95,
                                agelimit=FALSE, agecol='age', agescale=365.25) {

    # CMD CHECK
    id <- NULL

    if (ncol(trans_mat) != nrow(trans_mat)) {
        stop(paste0("Error: trans_mat has differing number of rows and columns (",
                    nrow(trans_mat), " and ",
                    ncol(trans_mat), ")."))
    }

    if (any(sapply(start_times, function(s) s > times)))
        stop("Error: 'start_times' must be earlier than any value in 'times'.")

    validate_oldage(agelimit, agecol, newdata)

    # Replicate new individuals, starting states, and times N times
    newdata_ext <- dplyr::slice(newdata, rep(seq(nrow(newdata)), each=N))
    start_states <- obtain_individual_starting_states(trans_mat, nrow(newdata), N)
    initial_times <- rep(0, nrow(newdata_ext))

    # Calculate state occupancies
    occupancy <- state_occupancy(models, trans_mat, newdata_ext, tcovs, initial_times,
                                 start_states, ci, M, agelimit, agecol, agescale)

    # Add in individual IDs
    individual_key <- data.table::data.table(id=seq(nrow(newdata_ext))-1,
                                             individual=rep(seq(nrow(newdata)), each=N)-1)
    occupancy <- individual_key[occupancy, on='id']

    # Estimate transition probabilities, this will add 'simulation' as a key if used
    probs <- calculate_transition_probabilities(occupancy, start_times, times, ci)

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
    newd_key <- data.table::as.data.table(clean_newdata(newdata, models, agelimit, agecol))
    clean <- newd_key[probs, on=c('id'='individual')]
    clean[, id:=NULL]
    as.data.frame(clean)
}