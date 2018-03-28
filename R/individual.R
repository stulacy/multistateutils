# Runs individual simulation, i.e. runs a large number of copies of the
# same person throw the simulation.

#' Runs an individual level simulation
#' 
#' @family cohort_simulation
#' @param N Number of times to replicate this individual.
#' 
#' @export
individual_simulation <- function(models, newdata, trans_mat, N=NULL) {
# TODO Do I want to keep this function or just make a separate one for obtaining transition 
# probabilities and length of stay?
    desCpp(transitions, transmat, individual_attributes, initial_times) 
}