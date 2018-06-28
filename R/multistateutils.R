#' \code{multistateutils} package.
#'
#' Provides utility functions for already fitted parametric multi-state models,
#' such as estimating transition probabilities and length of stay, as well as
#' means of visualising these predictions.
#'
#' @docType package
#' @useDynLib multistateutils
#' @name multistateutils
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))