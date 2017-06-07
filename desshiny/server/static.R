MAX_STATES <- 10
MIN_STATES <- 1
NUM_STARTING_STATES <- 3
INITIAL_STATE_NAME <- 'Initial'
NUM_ATTRIBUTE_DRAWS_PREVIEW <- 1000
NUM_TIMES_DRAWS_PREVIEW <- 100
NUM_SAMPLES_ATTRIBUTE_EXPECTED <- 100 # Number of samples from attribute prior to use to calculate expected value of distribution
NUM_CAT_VALUES <- 10  # Maximum number of unique values of an attribute to be counted as categorical
COVAR_TYPES <- c("Individual attribute", "status", "time", "id", "other")
DEFAULT_COVAR_TYPE <- 'Individual attribute'
NO_COVAR_FORMULA <- '1'
ERROR_MARGIN <- 1.25
MIN_ENTRY_SEPARATION = 1
DEATH_OLD_AGE_STATE <- "oldage"

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
                             parameter_coefficients = function(dist_params, covars) {
                                 scale <- exp(c(intercept=unname(dist_params['scale']), covars))
                                 shape <- c(intercept=unname(exp(dist_params['shape'])))
                                 list(scale, shape)
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
                           parameter_coefficients = function(dist_params, covars) {
                               rate <- exp(c(intercept=unname(dist_params['rate']), covars))
                               shape <- c(intercept=unname(exp(dist_params['shape'])))
                               list(rate, shape)
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
                                 parameter_coefficients = function(dist_params, covars) {
                                     rate <- exp(c(intercept=unname(dist_params['rate']), covars))
                                     list(rate)
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
                                parameter_coefficients = function(dist_params, covars) {
                                    meanlog <- c(intercept=unname(dist_params['meanlog']), covars)
                                    sdlog <- c(intercept=unname(exp(dist_params['sdlog'])))
                                    list(meanlog, sdlog)
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
                                  parameter_coefficients = function(dist_params, covars) {
                                      scale <- exp(c(intercept=unname(dist_params['scale']), covars))
                                      shape <- c(intercept=unname(exp(dist_params['shape'])))
                                      list(scale, shape)
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
                                  rate <- paste0('exp(', coefs['rate'], covars, ')')
                                  shape <- paste(coefs['shape'])
                                  c(rate, shape)
                              },
                              parameter_coefficients = function(dist_params, covars) {
                                  rate <- exp(c(intercept=unname(dist_params['rate']), covars))
                                  shape <- c(intercept=unname(dist_params['shape']))
                                  list(rate, shape)
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
                                 draw=function(n, params) as.vector(rmultinom(1, n, params))),
              "Oldage"=list(short="oldage",
                            dtype="continuous",
                            flex="oldage",
                            parameter_coefficients = function(dist_params, covars) {
                                list(unname(covars))
                            },
                            time_to_event=FALSE,
                            attribute_prior=FALSE)
              )
COVAR_DISTS <- names(DISTS)[sapply(DISTS, function(d) d$attribute_prior)]
TIME_DISTS <- names(DISTS)[sapply(DISTS, function(d) d$time_to_event)]

run_simulation_cpp <- function(trans_mat, num_inds, entryrate, censor_time, attributes, transitions) {

    # Obtain entry times and attributes for incident individuals
    initial_times <- calculate_event_times(num_inds, entryrate, censor_time)
    n_inds <- length(initial_times)
    raw_attrs <- lapply(attributes, function(x) x$draw(n_inds))
    setDT(raw_attrs)
    new_data <- bind_rows(apply(raw_attrs, 1, convert_stringdata_to_numeric, attributes))
    new_data <- cbind(intercept=1, new_data)

    # TODO Have some switch where this is governed by a time-scale parameter
    if ('age' %in% colnames(new_data)) {
        new_data$age <- new_data$age * 365.25
    }

    # Line that runs the simulation
    history <- desCpp(transitions, trans_mat, as.matrix(new_data), initial_times)

    history <- data.table(history)
    setnames(history, c('id', 'state', 'time'))
    history[, c('id', 'state') := list(as.factor(id + 1),
                                       as.integer(state))]

    # Add patient attribute information to the results if have it
    if (ncol(raw_attrs) > 0) {
        raw_attrs[, id := as.factor(seq(n_inds))]
        history <- history[raw_attrs, nomatch=0, on='id']  # Inner join in DT syntax
    }
    history
}

calculate_number_individuals <- function(entry_rate, termination_method, termination_value) {

    if (termination_method == "Time limit") {
        # If specify time limit then number of individuals is rate * time limit, plus an error margin
        initial_n <- ERROR_MARGIN * (entry_rate * termination_value)
    } else if (termination_method == "Individual limit") {
        # Otherwise can specify number of individuals directly
        initial_n <- termination_value
    } else {
        message(paste0("Error: Unknown termination criteria option '", termination_method, "'."))
        return()
    }
}

calculate_event_times <- function(initial_n, entryrate, censor_time) {
    inter_arrival_times <- rexp(initial_n, entryrate)
    inter_arrival_times[inter_arrival_times < MIN_ENTRY_SEPARATION] <- MIN_ENTRY_SEPARATION
    entry_times <- cumsum(inter_arrival_times)

    if (!is.null(censor_time)) {
        entry_times <- entry_times[entry_times < censor_time]
    }

    if (length(entry_times) == 0)
        return()

    entry_times
}



setup_eventlist_cpp <- function(entryrate, termination_criteria, termination_value) {
    if (termination_criteria == "Time limit") {
        # If specify time limit then number of individuals is rate * time limit, plus an error margin
        initial_n <- ERROR_MARGIN * (entryrate * termination_value)
    } else if (termination_criteria == "Individual limit") {
        # Otherwise can specify number of individuals directly
        initial_n <- termination_value
    } else {
        message(paste0("Error: Unknown termination criteria option '", termination_value, "'."))
    }

    entry_times <- cumsum(rexp(initial_n, entryrate))
    # Remove values which are above maximum time if using one
    if (termination_criteria == "Time limit") {
        entry_times <- entry_times[entry_times < termination_value]
    }
    n_inds <- length(entry_times)

    if (n_inds == 0)
        return(NULL)

    entry_times
}


convert_stringdata_to_numeric <- function(df, attrs_list) {
    # Converts a data frame consisting of a list of strings to numeric format.
    # In particular it creates dummy binary predictors for categorical attributes
    newdf <- list()

    for (a in names(df)) {
        # Create dummy variables for categorical
        if (attrs_list[[a]]$type == 'Categorical') {
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

create_param_string_from_mod <- function(dist, mod, cat_vars) {
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

get_coefs_from_mod <- function(dist, mod, cat_vars) {
    coefs <- coef(mod)

    # Replace categorical variable names with ones with fullstops in as this how they
    # are stored when expanded to dummy variables
    for (x in cat_vars) {
       names(coefs) <- gsub(x, paste0(x, "."), names(coefs))
    }

    # Extract distribution params and covariates
    dist_params <- coefs[names(coefs) %in% dist$params]
    covars <- coefs[!names(coefs) %in% dist$params]

    # Each distribution is parameterised slightly differently so delegate the definition of
    # parameter specific coefficients to them
    dist$parameter_coefficients(dist_params, covars)
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

calculate_parameters <- function(params, newdata, n_entries) {
    # Params: A vector of parameter specifications in string format, i.e.:
    # 'exp(3 + 0.25 * [age] + 0.43 * [sex])'
    # Newdata: A data frame containing the attributes that may be referred to in
    # raw_params
    # n_entries: The number of people being observed in the simulation. Should be equal to
    #   newdata, but newdata may be 0 if just the baseline survival curves are being built
    #   so the number of individuals is explicitly provided in this case.
    # Returns the numeric values of the distribution parameters as a data frame with
    # e #events rows and p #params columns
    # Calculate parameters from new data
    new_params <- sapply(params, function(p) gsub("\\[", "newdata\\[\\['", p))
    new_params <- sapply(new_params, function(p) gsub("\\]", "'\\]\\]", p))
    new_params <- sapply(new_params, function(p) parse(text=p))
    param_vals <- lapply(new_params, function(p) eval(p))

    params_df <- data.frame(param_vals)

    if (nrow(params_df) != n_entries) {
        # Replicate rows
        params_df <- params_df[rep(row.names(params_df), ceiling(n_entries / nrow(params_df))), ]
    }
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

create_empirical_sample_func <- function(var, data) {
    force(var)
    emp_dist <- data[[var]]
    function(n) {
        sample(emp_dist, n, replace=T)
    }
}