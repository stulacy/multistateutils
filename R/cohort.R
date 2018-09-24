#' Runs a cohort discrete event simulation
#'
#' @inheritParams predict_transitions
#' @param start_time Entry times of individuals specified in \code{newdata}.
#'  Can either be a single time that everyone enters at, or have as many values
#'   as rows in \code{newdata}. Defaults to everyone starting at time 0.
#' @param start_state The starting state of the individuals specified in \code{newdata}.
#'  States can be represented by an integer (the row/column number of the state in \code{trans_mat}),
#'  or as a string giving the name of the state in \code{trans_mat}.
#'  Can either be a single value when everyone starts in the same state, or have as many values
#'   as rows in \code{newdata}. Defaults to everyone starting in state 1.
#' @param time_limit The maximum time to run the simulation for. If not provided then
#'   the simulation runs until all the individuals have obtained a sink state.
#' @return A data frame with state entry times for each individual.
#' @examples
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
#' sim <- cohort_simulation(models, ebmt3, tmat)
#' @export
cohort_simulation <- function(models, newdata, trans_mat, start_time=0, start_state=1,
                              time_limit=NULL,
                              tcovs=NULL, M=1e3, ci=FALSE, ci_margin=0.95,
                              agelimit=FALSE, agecol='age', agescale=365.25) {

    # Required by CRAN checks
    state <- NULL
    id <- NULL
    time <- NULL

    if (ncol(trans_mat) != nrow(trans_mat)) {
        stop(paste0("Error: trans_mat has differing number of rows and columns (",
                    nrow(trans_mat), " and ",
                    ncol(trans_mat), ")."))
    }

    N <- nrow(newdata)

    validate_oldage(agelimit, agecol, newdata)

    newdata <- clean_newdata(newdata, models, agelimit, agecol)

    if (length(start_time) == 1)
        start_time <- rep(start_time, N)

    if (length(start_state) == 1)
        start_state <- rep(start_state, N)

    # Guards
    if (length(start_time) != N)
        stop("Error: start_time must have either as many values as rows in newdata, or 1.")
    if (length(start_state) != N)
        stop("Error: start_time must have either as many values as rows in newdata, or 1.")
    start_state <- sapply(start_state, validate_starting_state, trans_mat)
    # check start times all positive
    if (!all(start_time >= 0))
        stop("Error: must have start_time >= 0.")
    if (!is.null(time_limit)) {
        if (!is.numeric(time_limit))
            stop("Error: time_limit must be a positive number.")
        if (time_limit <= 0)
            stop("Error: time_limit must be a positive number.")

        incident_before_timelimit <- start_time <= time_limit
        newdata <- newdata[incident_before_timelimit, ]
        start_time <- start_time[incident_before_timelimit]
        start_state <- start_state[incident_before_timelimit]
    }

    occupancy <- state_occupancy(models, trans_mat, newdata, tcovs, start_time,
                                 start_state, ci, M, agelimit, agecol, agescale)

    if (!is.null(time_limit))
        occupancy <- occupancy[time <= time_limit]

    # Add covariates
    clean <- data.table::as.data.table(newdata)[occupancy, on='id']
    setcolorder(clean, c('id', setdiff(names(clean), 'id')))
    as.data.frame(clean)
}