# Calculates length of stay for multiple individuals.
#
# Uses an already formatted set of arguments to run the individual level
# simulation for each new individual (whereas \code{individual_simulation} only accepts 1 person)
# and derives transition probabilities from the resulting state occupancies.
#
# occupancy State occupancy data.table as returned by \code{state_occupancy}.
# state_names Character vector containing the names of the states.
# end_times Times at which to estimate transition probabilities. If not provided then doesn't estimate
#   transition probabilities, just length of stay.
#
# return A data frame in long format with transition probabilities for each individual,
# for each starting time, and for each ending time.
# TODO Could make this different to totls.simfs by allowing user to specify starting time as well?
# I.e. this currently just assumes state entry at time 0
calculate_los <- function(occupancy, start_states, times, state_names, ci, start_time=0) {

    # Required by CRAN checks
    state <- NULL
    time <- NULL
    duration <- NULL
    
    occupancy[, state := state + 1]  # convert to 1-based index
    
    nstates <- length(state_names)
    keys <- c('individual', 'id')
    los_keys <- c('individual', 'state')
    if (ci) {
        keys <- c('simulation', keys) 
        los_keys <- c('simulation', los_keys)
    }
    
    los <- data.table::rbindlist(lapply(stats::setNames(times, times), function(t) {
        data.table::rbindlist(lapply(stats::setNames(start_states, start_states), function(s) {
            # Filter to people in this starting state and remove state entries
            # that are after t
            this_state <- merge(occupancy[state == s & time == start_time, keys, with=F], 
                                occupancy[time <= t], 
                                on=keys)
            
            # Add dummy state which replicates last known state at t and add back into main table
            early_exit <- copy(this_state[this_state[, .I[which.max(time)], by=keys]$V1][time < t])
            early_exit[ , time:= t ]
            full <- rbindlist(list(this_state, early_exit))[order(time)]
            
            # Obtain time differences between each state transition
            time_spent <- full[, .(state=get_state_entries(state), duration=diff(time)), 
                               by=keys]
            
            # Sum up time spent in each state in case of multiple entries to same state
            time_spent[, duration := sum(duration), by=c(keys, 'state')]
            num_in_starting_state <- length(unique(time_spent$id))
            time_spent[, .(los=sum(duration)/num_in_starting_state), by=los_keys]
        }), idcol='start_state')
    }), idcol='t')
    setorder(los, 't', 'start_state', 'individual', 'state')
    los
}

#' Estimates length of stay
#'
#' Estimates length of stay in each state of an individual's passage 
#' through a multi-state model
#' by discrete event simulation.
#'
#' @inheritParams predict_transitions
#' @param start Starting state. Either number or character name in \code{trans_mat}.
#' @param times Times at which to estimate length of stay.
#' @return A data frame containing length of stay estimates.
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
#' # Estimate length of stay in each state after a year, given starting in state 1
#' length_of_stay(models, 
#'                newdata=newdata,
#'                tmat, times=365.25,
#'                start=1)
#'
#' @importFrom magrittr '%>%'
#' @import data.table
#' @export
length_of_stay <- function(models, newdata, trans_mat, times, start=1,
                           tcovs=NULL, N=1e5, M=1e3, ci=FALSE,
                           ci_margin=0.95) {
    
    # Required by CRAN checks
    state <- NULL

    if (ncol(trans_mat) != nrow(trans_mat)) {
        stop(paste0("Error: trans_mat has differing number of rows and columns (",
                    nrow(trans_mat), " and ",
                    ncol(trans_mat), ")."))
    }
    
    start <- validate_starting_state(start, trans_mat)

    # TODO More guards! Check nature of trans_mat, check that covariates required
    # by all models are in newdata. Although want state-occupancy specific guards to
    # be in 'state_occupancy'

    # Calculate state occupancies
    occupancy <- state_occupancy(models, trans_mat, newdata, N, tcovs, ci, M)

    # Estimate transition probabilities, this will add 'simulation' as a key if used
    los <- calculate_los(occupancy, start, times, colnames(trans_mat), ci)
    
    # Use state names rather than indices
    los$state <- factor(los$state, levels=seq(ncol(trans_mat)), labels=colnames(trans_mat))
    los$start_state <- factor(los$start_state, levels=seq(ncol(trans_mat)), labels=colnames(trans_mat))
    
    if (ci) {
        # Form the unique indices and grab state names we're going to need for these summaries
        keys <- c('t', 'start_state', 'individual')
        keys_with_state <- c(keys, 'state')
        states <- colnames(trans_mat)

        # Calculate CI limits
        ci_tail <- (1 - ci_margin) / 2
        ci_upper <- 1 - ci_tail
        ci_lower <- ci_tail
        
        # Calculate summary values across simulations
        los <- los[, .(los=mean(los), 
                           upper=stats::quantile(los, ci_upper),
                           lower=stats::quantile(los, ci_lower)), 
                       by=keys_with_state]
        
        # Form wide tables with the estimate and CIs
        form <- stats::as.formula(paste(paste(keys, collapse='+'), 'state', sep='~'))
        los_wide_mean <- dcast(los, form, value.var='los')
        los[, state := sprintf("%s_%0.1f", state, ci_upper*100)]
        los_wide_upper <- dcast(los, form, value.var='upper')
        los[, state := gsub("_[0-9\\.]+$", sprintf("_%0.1f", ci_lower*100), state)]
        los_wide_lower <- dcast(los, form, value.var='lower')
        
        # Combine into one
        los_wide <- merge(merge(los_wide_mean, los_wide_lower, by=keys), 
                          los_wide_upper, 
                          by=keys)
    } else {
        los_wide <- dcast(los, t + start_state + individual ~ state, value.var='los')
    }
    
    los_wide[, t := as.numeric(t)]
    setorder(los_wide, 't', 'start_state', 'individual')
    
    # Add in columns for each covariate name to replace the single 'individual' column
    clean <- separate_covariates(los_wide, colnames(newdata))
    clean
}