# Runs cohort simulation, i.e. that of a whole group of people, either limited by
# a set duration or by the number of people

#' Runs a cohort discrete event simulation
#' 
#' @param models Models in the form of a list of \code{flexsurvreg} objects.
#' @param newdata Data frame with individual attributes
#' @param trans_mat Transition matrix
#' @param start_times Entry times of individuals specified in \code{newdata}. If NULL
#'   then everyone enter at t=0
#' @export
#' 
#' @useDynLib rdes
#' @importFrom Rcpp evalCpp
cohort_simulation <- function(models, newdata, trans_mat, start_times=NULL) {
    desCpp(transitions, transmat, individual_attributes, initial_times) 
}