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
    if (class(mod) == 'oldage') {
        list(name='oldage',
             coefs=list(list(which(colnames(attrs) == mod$col)-1,
                        mod$scale),
                   list(0,
                        mod$limit
                        )))

    } else if (class(mod) == 'flexsurvreg') {
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
    } else {
        stop(paste0("Error: Unknown model class '",
                    class(mod),
                    "'. Only flexsurvreg or oldage are currently supported."))
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
    cov_names <- get_covariates(models)

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

# Used to get the states ids that have entered in LoS estimations
get_state_entries <- function(x) {
    x[-length(x)]
}

get_sink_states <- function(tmat) {
    if (class(tmat) != 'matrix')
        stop("Error: must provide a square transition matrix.")
    if (ncol(tmat) != nrow(tmat))
        stop("Error: must provide a square transition matrix.")

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
    } else if (is.numeric(start)) {
        if (any(start %% 1 != 0))
            stop("Error: start_state must be an integer or the name of a state.")

        # See if integer is valid one
        if (any(!start %in% seq_along(rownames(trans_mat))))
            stop(paste0("Error: if providing an integer for start_state ensure that it is in the range 1:",
                        nrow(trans_mat), "."))
    } else {
        stop("Error: start_state must be an integer or the name of a state.")
    }
    start
}

# Function to obtain the list of possible states that can be visited
# from a given starting state, for a state structure defined by
# a transition-matrix
# start Starting state as integer index in transition matrix
# trans_mat Transition matrix where null indicates no transition.
get_visited_states <- function(start, trans_mat) {

    # Don't really want to guard start as it passing NULL is the
    # termination criteria of this recursive function
    if (ncol(trans_mat) != nrow(trans_mat))
        stop("Error: must provide a square transition matrix.")
    if (!(start >= 1 && start <= ncol(trans_mat)))
        stop(paste0("Error: start must be an integer in range 1:", ncol(trans_mat), "."))

    # Obtain the states that this starting state directly feeds into
    vis <- unname(which(!is.na(trans_mat[start, ])))
    if (length(vis) == 0) {
        start
    } else {
        # Recursively build up a set of indices of visited states
        unique(unlist(c(start, sapply(vis, get_visited_states, trans_mat))))
    }
}

# For individual simulation, such as estimating transition probabilities or
# length of stay, we want to uniformly distribute them amongst the possible
# starting states.
# This function determines these.
obtain_individual_starting_states <- function(trans_mat, ninds, nreps) {

    if (!is.numeric(ninds) || !is.numeric(nreps))
        stop("Error: ninds and nreps must be positive integers.")

    if (ninds %% 1 != 0 || nreps %% 1 != 0)
        stop("Error: ninds and nreps must be positive integers.")

    if (ninds <= 0 || nreps <= 0)
        stop("Error: ninds and nreps must be positive integers.")

    # Split people to evenly start in non-sink states.
    # Assign 1 person per sink state to get probability of 1
    is_sink <- apply(trans_mat, 1, function(col) all(is.na(col)))
    sink_states <- unname(which(is_sink))
    non_sink <- setdiff(seq(ncol(trans_mat)), sink_states)

    # Form vector of starting states, one per individual uniformly distributed
    start_states_long <- sample(non_sink, nreps-length(sink_states), replace=T)
    rep(c(start_states_long, sink_states), ninds)
}

# Obtains covariates that are used by these models
get_covariates <- function(models) {
    if (!"list" %in% class(models))
        stop("Error: must provide a list of flexsurvreg objects.")

    if (!all(sapply(models, class) %in% c('flexsurvreg', 'oldage')))
        stop("Error: must provide a list of flexsurvreg objects.")

    cov_names <- unique(unlist(lapply(models, function(x) {
        attr(x$concat.formula, "covnames")
    })))
}

clean_newdata <- function(newdata, models, agelimit, agecol) {
    # Filter newdata to covariates in models
    used_covars <- get_covariates(models)


    if (is.numeric(agelimit)) {
        if (! agecol %in% used_covars)
            used_covars <- c(used_covars, agecol)
    }

    miss_cols <- setdiff(used_covars, colnames(newdata))
    if (length(miss_cols) > 0)
        stop(paste0("Error: missing columns ",
                    paste(miss_cols, collapse=', '),
                    " in newdata."))

    newdata <- dplyr::select(newdata, dplyr::one_of(used_covars))
    newdata$id <- seq(nrow(newdata)) - 1  # Add column id as rownumber 0-indexed
    newdata
}

validate_oldage <- function(agelimit, agecol, newdata) {
    if (agelimit == FALSE) {
        return(TRUE)
    }

    if (!(is.numeric(agelimit) && agelimit > 0))
        stop("Error: If agelimit is provided it must be a positive numerical value.")

    if (!agecol %in% colnames(newdata))
        stop(paste0("Error: agecol '", agecol, "' not found in newdata."))

    TRUE
}

# Clean up after package unloaded
.onUnload <- function (libpath) {
    library.dynam.unload("multistateutils", libpath)
}