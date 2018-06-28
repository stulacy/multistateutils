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
cohort_simulation <- function(models, newdata, trans_mat, start_times=NULL) {

    # TODO Should I also provide a higher level function that handles things like
    # time / #individual limit, censoring time, limiting death at 100?
    # Or should this function handle it?
    # Or should it even be provided at all, or just let user do it?

    transitions <- models

    mat <- stats::model.matrix(~., newdata)

    if (!is.null(start_times) & length(start_times) != nrow(newdata) & length(start_times) != 1) {
        stop(paste("Error: Please either provide a single value, or as many values as",
                   "there are rows in newdata for start_times."))
    }

    entry_time <- NULL
    if (!is.null(start_times) & length(start_times) == 1) {
        entry_time <- start_times
    } else if (is.null(start_times)) {
        entry_time <- 0
    }

    if (!is.null(entry_time)) {
        start_times <- rep(entry_time, nrow(mat))
    }

    desCpp(transitions, trans_mat, mat, start_times)
}