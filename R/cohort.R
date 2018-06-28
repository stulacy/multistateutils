# Runs a cohort discrete event simulation
#
# @param models Models in the form of a list of \code{flexsurvreg} objects.
# @param newdata Data frame with individual attributes
# @param trans_mat Transition matrix
# @param start_times Entry times of individuals specified in \code{newdata}.
#  Can either be a single time that everyone enters at, or have as many values
#   as rows in \code{newdata}. If NULL then everyone enter at t=0
# @return A data frame with state entry times for each individual.
# @export
# @importFrom Rcpp evalCpp
cohort_simulation <- function(models, newdata, trans_mat, start_time=0, start_state=1,
                              time_limit=NULL,
                              tcovs=NULL, M=1e3, ci=FALSE, ci_margin=0.95) {

    N <- nrow(newdata)
    
    if (length(start_time) == 1) 
        start_time <- rep(start_time, N)
    
    if (length(start_state) == 1) 
        start_state <- rep(start_state, N)
    
    # Guards
    if (length(start_time) != N) 
        stop("Error: start_time must have either as many values as rows in newdata, or 1.")
    if (length(start_state) != N) 
        stop("Error: start_time must have either as many values as rows in newdata, or 1.")
    # check start times all positive
    if (!all(start_time > 0)) 
        stop("Error: must have positive start_time.")
    start_state <- sapply(start_state, validate_starting_state, trans_mat)
        
    occupancy <- state_occupancy(models, trans_mat, newdata, tcovs, start_time, 
                                 start_state, ci, M)
    
    # TODO what post-processing is required for cohort? Want to apply time-limit for starters
    occupancy
}