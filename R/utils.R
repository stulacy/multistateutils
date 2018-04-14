# Function to obtain the transition list per transition model as required
obtain_model_coef <- function(mod, attrs) {
    all_coefs <- stats::coef(mod)
    dist <- mod$dlist$name
    dist <- gsub("\\.[a-zA-Z]+", "", dist)
    attr_names <- colnames(attrs)
    param_names <- DISTS[[dist]]
    num_params <- length(param_names)
    # for each distribution parameter, get the associated coefficients and find their index in the overall attribute matrix
    coefs_out <- lapply(param_names, function(param) {

        # Grab coefficients from this covariate
        covar_effs <- all_coefs[-seq(num_params)][mod$mx[[param]]]

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

    list(name=dist, coefs=coefs_out)
}

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

DISTS <- list("weibull"=c("scale", "shape"),
              "gamma"=c("rate", "shape"),
              "exp"=c('rate'),
              "lnorm"=c("meanlog", "sdlog"),
              "llogis"=c('scale', 'shape'),
              "gompertz"=c('rate', 'shape')
              )
