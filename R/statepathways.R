#' Displays the predicted flow for a given individual through the system
#' 
#' Produces an interactive HTML widget that displays a Sankey diagram showing a 
#' predicted patient pathway through the multi-state model.
#' 
#' @param models A list of \code{flexsurv} survival models for
#'   each transition in \code{trans_mat}.
#' @param trans_mat A transition matrix.
#' @param newdata A data frame containing the attributes of the person
#'   to display the predicted state flow for. As the diagram can only 
#'   be displayed for a single individual it will ignore any rows
#'   after the first.
#' @param times The time-points at which to estimate transition
#'  probabilities.
#' @return The HTML widget.
#' @export
state_pathway_flow <- function(models, trans_mat, newdata, times) {
    
}