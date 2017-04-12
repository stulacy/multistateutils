transitions <- reactiveValues()
attributes <- reactiveValues()

MAX_STATES <- 10
MIN_STATES <- 1
NUM_STARTING_STATES <- 3
INITIAL_STATE_NAME <- 'Initial'
NUM_ATTRIBUTE_DRAWS_PREVIEW <- 1000
NUM_TIMES_DRAWS_PREVIEW <- 100
NUM_SAMPLES_ATTRIBUTE_EXPECTED <- 100 # Number of samples from attribute prior to use to calculate expected value of distribution
NUM_CAT_VALUES <- 10  # Maximum number of unique values of an attribute to be counted as categorical
NEWDATA_STUMP = list(age=50, sex=0, 'age.>40'=1, 'age.20-40'=0, 'disease.AML'=0, 'disease.CML'=0, rand=20)
COVAR_TYPES <- c("Individual attribute", "status", "time", "id", "other")
DEFAULT_COVAR_TYPE <- 'other'
NO_COVAR_FORMULA <- '1'

DISTS <- list("Normal"=list(params=c("mean", "variance"),
                            short="N",
                            dtype = "Continuous",
                            time_to_event=FALSE,
                            attribute_prior=TRUE,
                            draw = function(n, params) rnorm(n, mean=params[1], sd=params[2]),
                            cumdens = function(time, params) pnorm(time, mean=params[1], sd=params[2])),
              "Weibull"=list(params=c("scale", "shape"),
                             short="Wei",
                             flex="weibull",
                             dtype = "Continuous",
                             time_to_event=TRUE,
                             attribute_prior=FALSE,
                             get_params_from_mod = function(coefs, covars) {
                                scale <- paste0('exp(', coefs['scale'], covars, ')')
                                shape <- paste(exp(coefs['shape']))
                                c(scale, shape)
                             },
                             draw = function(n, params) rweibull(n, scale=params[1], shape=params[2]),
                             cumdens = function(time, params) pweibull(time, scale=params[1], shape=params[2])),
              "Gamma"=list(params=c("rate", "shape"),
                           short="Gam",
                           flex="gamma",
                           dtype="Continuous",
                           time_to_event=TRUE,
                           attribute_prior=FALSE,
                           get_params_from_mod = function(coefs, covars) {
                              rate <- paste0('exp(', coefs['rate'], covars, ')')
                              shape <- paste(exp(coefs['shape']))
                              c(rate, shape)
                           },
                           draw = function(n, params) rgamma(n, rate=params[1], shape=params[2]),
                           cumdens = function(time, params) pgamma(time, rate=params[1], shape=params[2])),
              "Exponential"=list(params=c('rate'),
                                 short="Exp",
                                 flex="exp",
                                 dtype="Continuous",
                                 time_to_event=TRUE,
                                 attribute_prior=TRUE,
                                 get_params_from_mod = function(coefs, covars) {
                                    rate <- paste0('exp(', coefs['rate'], covars, ')')
                                    rate
                                 },
                                 draw = function(n, params) rexp(n, rate=params[1]),
                                 cumdens = function(time, params) pexp(time, rate=params[1])),
              "Log-Normal"=list(params=c("meanlog", "sdlog"),
                                short="logN",
                                flex='lnorm',
                                dtype = "Continuous",
                                time_to_event=TRUE,
                                attribute_prior=FALSE,
                                get_params_from_mod = function(coefs, covars) {
                                    meanlog <- paste0(coefs['meanlog'], covars)
                                    sdlog <- paste(exp(coefs['sdlog']))
                                    c(meanlog, sdlog)
                                },
                                draw = function(n, params) rlnorm(n, meanlog=params[1], sdlog=params[2]),
                                cumdens = function(time, params) plnorm(time, meanlog=params[1], sdlog=params[2])),
              "Log-Logistic"=list(params=c('scale', 'shape'),
                                  short='logLog',
                                  flex='llogis',
                                  dtype='Continuous',
                                  time_to_event=TRUE,
                                  attribute_prior=FALSE,
                                  get_params_from_mod = function(coefs, covars) {
                                      scale <- paste0('exp(', coefs['scale'], covars, ')')
                                      shape <- paste(exp(coefs['shape']))
                                      c(scale, shape)
                                  },
                                  draw = function(n, params) rllogis(n, scale=params[1], shape=params[2]),
                                  cumdens = function(time, params) pllogis(time, scale=params[1], shape=params[2])),
              "Gompertz"=list(params=c('rate', 'shape'),
                              short="gom",
                              flex="gompertz",
                              dtype="Continuous",
                              time_to_event=FALSE,
                              attribute_prior=FALSE,
                              get_params_from_mod = function(coefs, covars) {
                                  scale <- paste0('exp(', coefs['rate'], covars, ')')
                                  shape <- paste(coefs['shape'])
                                  c(scale, shape)
                              },
                              draw = function(n, params) rgompertz(n, rate=params[1], shape=params[2]),
                              cumdens = function(time, params) pgompertz(time, rate=params[1], shape=params[2])),
              "Uniform"=list(params=c("lower", "upper"),
                             short="U",
                             dtype = "Continuous",
                             time_to_event=FALSE,
                             attribute_prior=TRUE,
                             draw = function(n, params) runif(n, min=params[1], max=params[2]),
                             cumdens = function(time, params) punif(time, min=params[1], max=params[2])),
              "Multinomial"=list(short="multi",
                                 dtype = "Categorical",
                                 time_to_event=FALSE,
                                 attribute_prior=TRUE,
                                 draw=function(n, params) as.vector(rmultinom(1, n, params)))
              )
COVAR_DISTS <- names(DISTS)[sapply(DISTS, function(d) d$attribute_prior)]
TIME_DISTS <- names(DISTS)[sapply(DISTS, function(d) d$time_to_event)]


convert_stringdata_to_numeric <- function(df) {
    # Converts a data frame consisting of a list of strings to numeric format.
    # In particular it creates dummy binary predictors for categorical attributes
    attrs_list <- reactiveValuesToList(attributes)

    newdf <- list()

    for (a in names(df)) {
        # Create dummy variables for categorical
        if (attrs_list[[a]]$type == 'Categorical') {
            # TODO Remove reference level
            # Create dummary variables
            for (l in attrs_list[[a]]$levels) {
                newdf[[paste(a, l, sep='.')]] <- 0
            }
            # Set current value to 1
            newdf[[paste(a, df[[a]], sep='.')]] <- 1

        } else if (attrs_list[[a]]$type == 'Continuous') {
            newdf[[a]] <- as.numeric(df[[a]])
        } else {
            stop(paste0("Error: Unknown data type '", attrs_list[[a]]$type, "'."))
        }
    }
    newdf
}

create_param_string_from_mod = function(dist, mod, cat_vars) {
    num_params <- length(dist$params)
    ps <- coef(mod)

    # flexsurv always returns coefficients as distribution parameters followed by covariates
    if (length(ps) > num_params) {
        covar <- paste(sapply(names(ps[(num_params+1):length(ps)]), function(x) paste0('[', x, ']')),
                       ps[(num_params+1):length(ps)],
                       sep='*',
                       collapse='+')
        for (x in cat_vars) {
           covar <- gsub(x, paste0(x, "."), covar)
        }
        covar <- paste0('+', covar)  # Add on the + connecting the intercept to covariates
    } else {
        covar <- NULL
    }
    # Parameterise distribution with covariates
    dist$get_params_from_mod(ps, covar)
}


get_attr_names <- function(str) {
    # Extracts attribute names from a vector of strings
    # with params in format [param_name]

    # Returns a vector of [param1], [param2]... strings
    raw <- regmatches(str, gregexpr("\\[.*?\\]", str))[[1]]

    # Remove the brackets. NB: There's probably a neat regex to
    # do all these steps in one line but I don't know it
    raw1 <- gsub("\\[", "", raw)
    gsub("\\]", "", raw1)

}

calculate_parameters <- function(params, newdata) {
    # Params: A vector of parameter specifications in string format, i.e.:
    # 'exp(3 + 0.25 * [age] + 0.43 * [sex])'
    # Newdata: A data frame containing the attributes that may be referred to in
    # raw_params
    # Returns the numeric values of the distribution parameters as a data frame with
    # e #events rows and p #params columns
    # Calculate parameters from new data
    new_params <- sapply(params, function(p) gsub("\\[", "newdata\\[\\['", p))
    new_params <- sapply(new_params, function(p) gsub("\\]", "'\\]\\]", p))
    new_params <- sapply(new_params, function(p) parse(text=p))
    param_vals <- sapply(new_params, function(p) eval(p))
    params_df <- data.frame(param_vals)
    colnames(params_df) <- paste0('p', seq(ncol(params_df)))
    params_df
}

create_eventtime_draw <- function(dist) {
    # Dist: A string that fits in the entries of the DISTS list
    force(dist)
    func <- DISTS[[dist]]$draw
    force(func)
    function(n, params) {
        # Draw from distribution
        func(n, params)
    }
}

create_sample_func <- function(dist, params, labels=NULL) {
    force(params)
    force(dist)
    func <- DISTS[[dist]]$draw
    force(func)
    force(labels)
    function(n) {
        out <- func(n, params)
        # Only pass in labels if have multinomial distribution
        if (!is.null(labels)) {
            rep(labels, out)
        } else{
            out
        }
    }
}

create_empirical_sample_func <- function(var, filter=NULL) {
    force(var)
    force(filter)
    if (!is.null(filter)) {
        trans_state <- names(all_raw_attrs())[sapply(all_raw_attrs(), function(a) a$use == 'transition')]
        data <- uploaded_data() %>%
                    filter_(paste(trans_state, "==", filter))
    } else {
        data <- uploaded_data()
    }
    emp_dist <- data[[var]]
    function(n) {
        sample(emp_dist, n, replace=T)
    }
}