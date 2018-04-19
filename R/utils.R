#' Function to obtain the transition list per transition model as required.
#'
#' @param mod A parametric survival model saved as a \code{flexsurvreg} object.
#' @param attrs Attributes of a new individual as a data matrix.
#' @return A list with the following elements:
#' \itemize{
#'     \item name The distribution name as a string, as used by \code{flexsurv}.
#'     \item coefs A nested list, with each child representing a distribution parameter and
#'       being a list itself, containing 2 items. The first item is the (zero-based) indices in
#'       \code{attrs} of the model coefficients, and the second contains the coefficients themselves.
#' }
obtain_model_coef <- function(mod, attrs, M=1) {
    dist <- mod$dlist$name
    dist <- gsub("\\.[a-zA-Z]+", "", dist)
    attr_names <- colnames(attrs)
    param_names <- DISTS[[dist]]

    if (M == 1) {
        coefs_as_list(stats::coef(mod), param_names, mod$mx, attr_names, dist)
    } else {
        raw_coefs <- flexsurv::normboot.flexsurvreg(mod, M, transform=TRUE, raw=TRUE)
        lapply(1:M, function(i) {
            coefs_as_list(raw_coefs[i, ], param_names, mod$mx, attr_names, dist)
        })
    }
}

#' Utility function to obtain covariates as a list for a given distribution, parameterised
#' by these arguments.
#'
#' This function should never be directly called, it is a helper function to reduce code repetition
#' when making bootstrapped models.
#'
#' @param all_coefs A vector of all the coefficients from a model, including distribution coefficients,
#'  which are located at the start of the vector.
#' @param param_names Distribution parameter names as a character vector.
#' @param mx The \code{mx} attribute of a \code{flexsurvreg} object, containing indices
#'   of covariates in the coefficient list that act on each distribution parameter.
#' @param attr_names Attribute names of the entire attribute matrix used for this model, as a character.
#'   The model may only use a subset of these attributes in each its function, but it needs to know the
#'   indices of the covariates it does use in the wider attribute matrix.
#' @param dist The distribution name as a string as used by \code{flexsurvreg}.
#'
#' @return The transition list, as described by \code{obtain_model_coef}.
coefs_as_list <- function(all_coefs, param_names, mx, attr_names, dist) {

    # for each distribution parameter, get the associated coefficients and find their index in the overall attribute matrix
    coefs <- lapply(param_names, function(param) {

        # Grab coefficients from this covariate
        covar_effs <- all_coefs[-seq_along(param_names)][mx[[param]]]

        # Add in distribution effect
        full_covar_effs <- unname(c(all_coefs[param], covar_effs))

        # Now calculate the indices of the attribute matrix used so that future
        # predictions can be made
        covar_names <- names(covar_effs)
        # Remove brackets around coefficient names added by ancilliary params
        covar_names <- gsub("[a-zA-Z]+\\(", "", covar_names)
        covar_names <- gsub("\\)", "", covar_names)

        indices <- c(1, match(covar_names, attr_names)) - 1  # Prepending with 1 adds intercept, and -1 converts to zero-index

        list(indices, full_covar_effs)
    })
    list(name=dist, coefs=coefs)
}

#' Creates data matrix from data frame and models.
#'
#' @param dataframe The data to be converted in a standard
#'   \code{data.frame} object.
#' @inheritParams predict_transitions
#'
#' @return A \code{data.matrix} with the data.
form_model_matrix <- function(dataframe, models) {
    # Obtain the coefficients used in all models
    cov_names <- unique(unlist(sapply(models, function(x) {
        attr(x$concat.formula, "covnames")
    })))

    # Obtain transitions as list as required above. Need data in matrix form first. Will generate this for the entire dataset, containing
    # all covariates that are used in any transition.
    newform <- stats::as.formula(paste("~", paste(cov_names, collapse='+')))
    stats::model.matrix(newform, dataframe)
}

#' Separates individuals who are indexed by a single column into their constituent covariates.
#'
#' Requires a data.table or data.frame that has a column called 'individual' that holds identifying
#' information about that individual combined into a single column for use as a key. The format
#' of this column is <covar_name>=<covar_value>,<covar2_name>=<covar2_value>...
#'
#' @param dt Input data source. Can be \code{data.table} or \code{data.frame}.
#' @param covariates The covariate names that these individuals have, as a character string.
#'   In the description above these are the <covar_name> values.
#'
#' @return A copy of the data with the 'individual' column removed and replaced by a column
#'   for each covariate in \code{covariates} as a \code{data.frame}.
separate_covariates <- function(dt, covariates) {
    proportions_df <- dt %>%
                        tidyr::separate(individual, sep=',', into=covariates) %>%
                        as.data.frame()  # Coerce to data.frame if not already

    proportions_df[covariates] <- lapply(proportions_df[covariates],
                                     function(x) gsub("[[:alnum:]]+=", "", x))

    proportions_df
}

DISTS <- list("weibull"=c("scale", "shape"),
              "gamma"=c("rate", "shape"),
              "exp"=c('rate'),
              "lnorm"=c("meanlog", "sdlog"),
              "llogis"=c('scale', 'shape'),
              "gompertz"=c('rate', 'shape')
              )
