# Function to obtain the transition list per transition model as required.
#
# param mod A parametric survival model saved as a flexsurvreg object.
# param attrs Attributes of a new individual as a data matrix.
# param M Number of bootstrap simulations, set to 1 if no bootstrapping is used.
# return A list with the following elements:
#   name The distribution name as a string, as used by flexsurv.
#   coefs A nested list, with each child representing a distribution parameter and
#     being a list itself, containing 2 items. The first item is the (zero-based) indices in
#     attrs of the model coefficients, and the second contains the coefficients themselves.
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

# Utility function to obtain covariates as a list for a given distribution, parameterised
# by these arguments.
#
# This function should never be directly called, it is a helper function to reduce code repetition
# when making bootstrapped models.
#
# param all_coefs A vector of all the coefficients from a model, including distribution coefficients,
#  which are located at the start of the vector.
# param param_names Distribution parameter names as a character vector.
# param mx The mx attribute of a flexsurvreg object, containing indices
#   of covariates in the coefficient list that act on each distribution parameter.
# param attr_names Attribute names of the entire attribute matrix used for this model, as a character.
#   The model may only use a subset of these attributes in each its function, but it needs to know the
#   indices of the covariates it does use in the wider attribute matrix.
# param dist The distribution name as a string as used by flexsurvreg.
#
# return The transition list, as described by obtain_model_coef.
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

# Creates data matrix from data frame and models.
form_model_matrix <- function(dataframe, models) {
    # Obtain the coefficients used in all models
    cov_names <- unique(unlist(lapply(models, function(x) {
        attr(x$concat.formula, "covnames")
    })))

    # Throw error if not all covariates are in newdata
    if (! all(cov_names %in% colnames(dataframe))) {
        stop(paste("Error: covariates missing from newdata that were used in model fitting: ", cov_names[!cov_names %in% colnames(dataframe)]))
    }

    # Remove variables from newdata that aren't in models
    extraneous_vars <- setdiff(colnames(dataframe), cov_names)
    if (length(extraneous_vars > 0))
        dataframe <- dataframe[-match(extraneous_vars, colnames(dataframe))]

    # Determine covariate levels so can set levels of the
    all_levels <- stats::setNames(lapply(cov_names, function(cov) {
        lapply(models, function(mod) levels(mod$data$m[[cov]]))
    }), cov_names)

    # Remove NULLs
    all_levels <- lapply(all_levels, function(cov) cov[!sapply(cov, is.null)])

    # Remove empty lists (continuous variables or otherwise not defined as factors)
    all_levels <- all_levels[!sapply(all_levels, function(x) length(x) == 0)]

    # Then check all unique
    unique_cov <- sapply(all_levels, function(cov) {
        all(sapply(cov, identical, cov[[1]]))
    })

    if (!all(unique_cov)) {
        stop(paste("Error: can't determine factor levels from models, inconsistent levels on different transition models. Check variables", names(unique_cov)[!unique_cov]))
    }

    # If have the same levels on all transitions then can simply set these as the levels
    for (cov in names(all_levels)) {
        dataframe[[cov]] <- factor(dataframe[[cov]], levels=all_levels[[cov]][[1]])  # Have proven identical above so can just use first item
    }

    # Obtain transitions as list as required above. Need data in matrix form first. Will generate this for the entire dataset, containing
    # all covariates that are used in any transition.
    newform <- stats::as.formula(paste("~", paste(cov_names, collapse='+')))
    stats::model.matrix(newform, dataframe)
}

# Separates individuals who are indexed by a single column into their constituent covariates.
#
# Requires a data.table or data.frame that has a column called 'individual' that holds identifying
# information about that individual combined into a single column for use as a key. The format
# of this column is <covar_name>=<covar_value>,<covar2_name>=<covar2_value>...
#
# param covariates The covariate names that these individuals have, as a character string.
#   In the description above these are the <covar_name> values.
#
# return A copy of the data with the 'individual' column removed and replaced by a column
#   for each covariate in covariates as a data.frame.
separate_covariates <- function(dt, covariates) {
    # CMD CHECK
    individual <- NULL

    proportions_df <- dt %>%
                        tidyr::separate(individual, sep=',', into=covariates) %>%
                        as.data.frame()  # Coerce to data.frame if not already

    proportions_df[covariates] <- lapply(proportions_df[covariates],
                                     function(x) gsub("[[:alnum:]]+=", "", x))

    proportions_df
}

# Used to get the states ids that have entered in LoS estimations 
get_state_entries <- function(x) {
    x[-length(x)]
}

get_sink_states <- function(tmat) {
    rownames(tmat)[apply(tmat, 1, function(row) all(is.na(row)))]
}

DISTS <- list("weibull"=c("scale", "shape"),
              "gamma"=c("rate", "shape"),
              "exp"=c('rate'),
              "lnorm"=c("meanlog", "sdlog"),
              "llogis"=c('scale', 'shape'),
              "gompertz"=c('rate', 'shape')
              )


# Validates the starting state whether input as numeric or character
validate_starting_state <- function(start, trans_mat) {
    if (is.character(start)) {
        start_int <- match(start, colnames(trans_mat))
        if (any(is.na(start_int)))
            stop(paste0("Error: starting state '",
                        start[is.na(start_int)], 
                        "' not found in trans_mat."))
        start <- start_int
    } else {
        # See if integer is valid one
        if (any(!start %in% seq_along(rownames(trans_mat))))
            stop(paste0("Error: starting state '", 
                        start[!start %in% seq_along(rownames(trans_mat))], 
                        "' not found in trans_mat."))
    }
    start
}

# Function to obtain the list of possible states that can be visited
# from a given starting state, for a state structure defined by
# a transition-matrix
# start Starting state as integer index in transition matrix
# trans_mat Transition matrix where null indicates no transition.
get_visited_states <- function(start, trans_mat) {
    # Obtain the states that this starting state directly feeds into
    vis <- unname(which(!is.na(trans_mat[start, ])))
    if (length(vis) == 0) {
        start
    } else {
        # Recursively build up a set of indices of visited states
        unique(unlist(c(start, sapply(vis, get_visited_states, trans_mat))))
    }
}