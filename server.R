library(shiny)
library(dplyr)
library(mstate)
library(tidyr)
library(ggplot2)
library(DiagrammeR)
library(cowplot)
library(flexsurv)
library(parallel)
library(jsonlite)
library(profvis)

# Remove when finished debugging
#options(shiny.fullstacktrace=T)

shinyServer(function(input, output, session) {

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

    create_eventtime_draw <- function(dist, params) {
        # Dist: A string that fits in the entries of the DISTS list
        # Params: A vector of parameter specifications in string format, i.e.:
        # 'exp(3 + 0.25 * [age] + 0.43 * [sex])'
        force(params)
        force(dist)
        func <- DISTS[[dist]]$draw
        force(func)
        # Calculate parameters from new data
        new_params <- sapply(params, function(p) gsub("\\[", "newdata\\[\\['", p))
        new_params <- sapply(new_params, function(p) gsub("\\]", "'\\]\\]", p))
        new_params <- sapply(new_params, function(p) parse(text=p))
        function(n, newdata) {
            # Evaluate parameters from data
            param_vals <- sapply(new_params, function(p) eval(p))

            # Draw from distribution
            func(n, param_vals)
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

    ############################### Load Data ##########################################################################
    uploaded_data <- reactive({
        if (is.null(input$covarinput))
            return()

        isolate({
            read.csv(input$covarinput$datapath, header=T, sep=',')
        })
    })

    # Read in attribute information
    all_raw_attrs <- reactive({

        in_data <- uploaded_data()

        if (is.null(in_data))
            return()

        input$speccolsbutton

        # TODO Throw error if have different numbers and names in times and status columns

        isolate({
            setNames(lapply(names(in_data), function(n) {
                # Save categorical and continuous information
                if (length(unique(in_data[[n]])) < NUM_CAT_VALUES) {
                    type <- 'Categorical'
                    levels <- levels(as.factor(in_data[[n]]))
                } else {
                    type <- 'Continuous'
                    levels <- NA
                }

                if (!is.null(input$speccolsbutton) && input$speccolsbutton > 0) {
                    use <- input[[paste0('rawattr', n)]]
                } else if (grepl("^time\\..+", n)) {
                    use <- 'time'
                } else if (grepl("^status\\..+", n)) {
                    use <- 'status'
                } else if (grepl(".*id.*", tolower(n))) {
                    use <- 'id'
                } else {
                    use <- DEFAULT_COVAR_TYPE
                }

                list(type=type, levels=levels, use=use)
            }), names(in_data))
        })
    })

    output$specifycols <- renderUI({
        in_data <- uploaded_data()
        if (is.null(in_data))
            return()

        item_list <- list()
        item_list[[1]] <- h5("Please specify the nature of columns in this data set so they can be used appropriately later on.")
        item_list[[2]] <- spec_col_dropdown
        item_list[[3]] <- actionButton("speccolsbutton", "Update")
        do.call(tagList, item_list)
    })

    spec_col_dropdown <- renderUI({

        isolate({
            raw_attrs <- names(all_raw_attrs())
            item_list <- list()

            for (i in seq_along(raw_attrs)) {

                # Obtain type information
                attr <- all_raw_attrs()[[i]]
                if (attr$type == 'Continuous') {
                    type_info <- 'continuous'
                } else {
                    type_info <- paste('categorical with ', length(attr$levels), "values")
                }
                item_list[[i]] <- selectInput(paste0("rawattr", raw_attrs[i]),
                                              HTML(paste0(raw_attrs[i], ' (', type_info, ')')),
                                              choices=COVAR_TYPES,
                                              selected=all_raw_attrs()[[i]]$use)
            }
            do.call(tagList, item_list)

        })
    })

    output$datatable <- renderUI({
        if (is.null(uploaded_data()))
           return(p("No data uploaded."))

        HTML(renderTable(head(uploaded_data()))())
    })

    output$rawattrinfo <- renderUI({
        if (is.null(uploaded_data()))
            return(p("No data uploaded."))

        attr_list <- paste0("<ul>", paste0(sapply(COVAR_TYPES, function(b) {
                                entries <- names(all_raw_attrs())[sapply(all_raw_attrs(), function(d) d$use == b)]
                                paste0("<li>", b, "<ul>", paste0(sapply(entries, function(d) paste0("<li><code>", d, "</code></li>")), collapse=''), "</ul>", "</li>")
                            }),
                            collapse=''),
                            "</ul>")
        item_list <- list()
        item_list[[1]] <- p("The following column types have been linked with the following variables in the uploaded data set:")
        item_list[[2]] <- HTML(attr_list)
        do.call(tagList, item_list)

    })

    # TODO Place restrictions on attribute types, i.e. only one transition, time, and status col
    #input$speccolsbutton



    ############################### States ############################################################

    # This is the number of default states before the Update button has been pressed
    # in the States tab
    starting_states <- reactive({
        attrs <- all_raw_attrs()
        isolate({
            time_vars <- names(attrs)[sapply(attrs, function(a) a$use == 'time')]
            # If have uploaded data, then use these states
            if (length(time_vars) > 0) {
                c(INITIAL_STATE_NAME, gsub("time\\.", '', time_vars))

            } else {
                LETTERS[1:NUM_STARTING_STATES]
            }
        })
    })


    states <- reactive({
        # If have pressed update button then get states from the inputs
        if (is.null(input$updatestates))
            return()

        if (input$updatestates > 0) {
            isolate({
                num_states <- if (is.null(input$stateslider)) NUM_STARTING_STATES else input$stateslider
                # This should never be true but best to be careful
                sapply(seq(num_states), function (i) {
                    ip <- input[[paste0("statename", i)]]
                    if (is.null(ip)) LETTERS[i] else ip
                })
            })
        } else {
            # Try and get state information from data
            starting_states()
        }
    })

    # TODO Have remove button for each state

    # TODO Remove transitions from states which don't exist

    output$stateslider <- renderUI({
        sliderInput("stateslider", "Number of states", MIN_STATES, MAX_STATES, length(starting_states()))
    })


    output$states <- renderUI({
        state_slider <- input$stateslider
        if (is.null(state_slider))
            return()

        isolate({
            item_list <- list()
            this_states <- states()
            num_states <- length(this_states)
            for (i in seq(state_slider)) {
                label <- if (i <= num_states) this_states[i] else LETTERS[i]
                item_list[[i]] <- textInput(paste0('statename', i), paste('State', i),
                                            label, width='40%')
            }
            do.call(tagList, item_list)
        })
    })

    output$newtransheader <- renderUI({
        if (is.null(states()))  {
            h5('Please add states first')
        } else {
            h4("New transition")
        }
    })

    output$seltrans <- renderUI({
        this_states <- states()
        if (is.null(input$statename1))
          return()

        item_list <- list()
        item_list[[1]] <- selectInput("transfrom", "From", choices=this_states)
        item_list[[2]] <- selectInput("transto", "To", choices=this_states)
        do.call(tagList, item_list)
    })

    output$addtransbutton <- renderUI({
        if (is.null(states()))
            return()
        actionButton("addtrans", "Add")
    })

    observeEvent(input$addtrans, {
        new_index <- length(reactiveValuesToList(transitions)) + 1
        from <- which(states() == input$transfrom)
        to <- which(states() == input$transto)
        transitions[[paste(from, to, sep='-')]] <- list(from=from, to=to, index=new_index)
    })

    output$currtransheader <- renderUI({
        txt <- if (length(reactiveValuesToList(transitions)) >= 1) 'Current transitions' else ''
        h4(txt)
    })

    output$currtrans <- renderTable({
        from <- sapply(reactiveValuesToList(transitions), function(x) states()[x$from])
        to <- sapply(reactiveValuesToList(transitions), function(x) states()[x$to])
        data.frame(From=from, To=to)
    })


    output$statedia <- renderGrViz({

        if (is.null(states())) {
          return()
        }

        input$addtrans

        isolate({
            edges <- sapply(reactiveValuesToList(transitions), function(x) paste(states()[x$from], states()[x$to], sep=" -> "))
            edge_vals <- sapply(reactiveValuesToList(transitions), function(x) x$index)

            states_dot <- paste("node [shape=circle] ", paste0(states(), collapse=";"))
            edges_dot <- paste(
                               mapply(function(e, v)
                                     paste(e, "[label='", v, "']"), edges, edge_vals),
                               collapse=" \n ")

            full <- paste("digraph states {", states_dot, edges_dot, "}")
            grViz(full)
        })

    })

    output$probsheader <- renderUI({
        if (length(reactiveValuesToList(transitions)) < 1) {
            h5("Add transitions before specifying their probability distributions")
        } else {
            return()
        }
    })


    ############################### Attributes ##########################################################################
    output$sampledistheader <- renderUI({
        item_list <- list()
        if (is.null(attributes) || length(reactiveValuesToList(attributes)) == 0) {
            item_list[[1]] <- h3("Sampling distributions of the attributes")
            item_list[[2]] <- p("No individual attributes selected.")
        } else {
            item_list[[1]] <- h3(paste0("Sampling distributions of the attributes (", NUM_ATTRIBUTE_DRAWS_PREVIEW, " samples displayed)"))
        }

        do.call(tagList, item_list)
    })

    output$plotarea <- renderPlot({
        if (is.null(attributes) || length(reactiveValuesToList(attributes)) == 0)
            return()

        isolate({
            vars <- reactiveValuesToList(attributes)
            plots <- lapply(vars[!sapply(vars, is.null)], function(v) {
                vals <- v$draw(NUM_ATTRIBUTE_DRAWS_PREVIEW)
                if (v$type == 'Continuous') {
                    ggplot(data.frame(x=vals), aes(x=x)) +
                            geom_histogram(fill='white', colour='black') +
                            theme_bw()
                } else {
                    vals <- as.vector(vals)
                    ggplot(data.frame(x=vals), aes(x=x)) +
                            geom_bar(stat='count') +
                            theme_bw()
                }
            })

            return(plot_grid(plotlist=plots, labels=names(plots)))
        })
    })


    ############################## Empirical covariates ############################
    output$loadattributes <- renderUI({
        ind_attrs <- all_raw_attrs()[sapply(all_raw_attrs(), function(a) a$use == 'Individual attribute')]

        if (length(ind_attrs) == 0) {
            return(HTML("To include empirical distributions for covariates, please see the <strong>Upload Data</strong> tab to upload a CSV file and identify certain columns as containing individual attributes."))
        }

        item_list <- list()
        item_list[[1]] <- h4("Select attributes")
        item_list[[2]] <- uiOutput("selloadedattrs")
        item_list[[3]] <- actionButton("updateselattrs", "Update")
        do.call(tagList, item_list)
    })

    output$selloadedattrs <- renderUI({
        ind_attrs <- all_raw_attrs()[sapply(all_raw_attrs(), function(a) a$use == 'Individual attribute')]
        item_list <- list()

        # Create checkboxes for each attribute
        for (i in seq_along(ind_attrs)) {
            item_list[[i]] <- create_attribute_checkbox_dropdown(names(ind_attrs)[i],
                                                                 trans_names)
        }
        do.call(tagList, item_list)
    })

    create_attribute_checkbox_dropdown <- function(attr, transitions) {
        checkboxInput(paste0("selattrcheck", attr),
                             HTML(paste0("<strong>", attr, "</strong>")),
                      value=T)

    }

    # Add empirical attributes to 'attributes' reactiveValues
    observeEvent(input$updateselattrs, {
        all_attrs <- all_raw_attrs()[sapply(all_raw_attrs(), function(x) x$use=='Individual attribute')]
        if (is.null(all_attrs))
            return()

        # Iterate through number of attributes, obtaining the relevant checkbox and whether selected or not
        for (i in seq_along(all_attrs)) {
            attr_name <- names(all_attrs)[i]
            is_selected <- input[[paste0('selattrcheck', attr_name)]]
            if (is_selected) {
                attributes[[attr_name]] <- list(type=all_attrs[[i]]$type,
                                                draw=create_empirical_sample_func(attr_name),
                                                levels=all_attrs[[i]]$levels)
            } else {
                # Remove value from list if unchecked and already there
                if (!is.null(attributes[[attr_name]]))
                    attributes[[attr_name]] <- NULL
            }
        }
    })


    ############################## Simulated covariates ############################
    output$simattributes <- renderUI({
        item_list <- list()
        item_list[[2]] <- h4("Add simulated attribute")
        item_list[[3]] <- textInput("simname", "Name")
        item_list[[4]] <- selectInput("simtype", "Type", choices=c('Continuous', 'Categorical'))
        item_list[[5]] <- selectInput("simdist", "Distribution", choices=c('Normal', 'Uniform'))
        item_list[[6]] <- uiOutput("simparams")
        item_list[[7]] <- actionButton("simadd", "Add")
        do.call(tagList, item_list)
    })


    observe({
        sim_type <- input$simtype

        if (is.null(sim_type))
            return()

        choices <- names(DISTS[COVAR_DISTS][sapply(DISTS[COVAR_DISTS], function(d) d$dtype == sim_type)])
        updateSelectInput(session, "simdist", choices=choices)
    })

    output$simparams <- renderUI({
        if (is.null(input$simdist))
            return()

        # Force reactiveness on the Add covariate button, this enforces the resetting of the
        # parameter text inputs, "simparam<x>"
        input$simadd

        item_list <- list()
        if (input$simdist == "Multinomial") {
            item_list[[1]] <- sliderInput("multinomslider", "Number of parameters", 1, 10, value=2)
            item_list[[2]] <- uiOutput("multinomparams")
        } else {
            for (i in seq_along(DISTS[[input$simdist]]$params)) {
                item_list[[i]] <- textInput(paste0("simparam", i), DISTS[[input$simdist]]$params[i], "")
            }
        }
        do.call(tagList, item_list)
    })

    output$multinomparams <- renderUI({
        item_list <- list()
        for (i in seq(input$multinomslider)) {
            item_list[[3*i-2]] <- h4(paste("Param", i))
            item_list[[3*i-1]] <- textInput(paste0("multinomparamname", i), "Label")
            item_list[[3*i]] <- sliderInput(paste0("multinomparamval", i), "Probability", min=0, max=1, value=0.5)
        }
        do.call(tagList, item_list)
    })

    # Add simulated attributes to 'attributes' reactiveValues
    observeEvent(input$simadd, {

        if (input$simdist == "Multinomial") {
            num_params <- input$multinomslider
            params <- as.numeric(sapply(seq(num_params), function(i) input[[paste0("multinomparamval", i)]]))
            labels <- sapply(seq(num_params), function(i) input[[paste0("multinomparamname", i)]])
        } else {
            num_params <- length(DISTS[[input$simdist]]$params)
            params <- as.numeric(sapply(seq(num_params), function(i) input[[paste0("simparam", i)]]))
            labels <- NULL
        }

        # Confirm have name and both parameter values
        if (input$simname == '') {
            print("Error: Please provide a name for the covariate.")
        } else if (any(is.na(params))) {
            print("Error: Please provide values for all distribution parameters.")
        } else {
            attributes[[input$simname]] <- list(type=input$simtype, draw=create_sample_func(input$simdist, params, labels=labels),
                                                levels=labels)

            # Reset all values
            updateTextInput(session, "simname", value='')
            updateSelectInput(session, "simdist", selected='Normal')
        }
    })


    ############################### Long data ############################################################
    long_data <- reactive({

        df <- uploaded_data()

        if (is.null(df))
            return()

        this_states <- states()
        trans <- reactiveValuesToList(transitions)

        isolate({
            # Determine transition matrix
            trans_mat <- matrix(NA, nrow=length(this_states), ncol=length(this_states),
                        dimnames = list(this_states, this_states))
            for (t in trans) {
                trans_mat[this_states[t$from], this_states[t$to]] <- t$index
            }

            # Obtain data and column names
            all_attrs <- all_raw_attrs()
            time_vars <- names(all_attrs)[sapply(all_attrs, function(a) a$use == 'time')]
            status_vars <- names(all_attrs)[sapply(all_attrs, function(a) a$use == 'status')]
            id_var <- names(all_attrs)[sapply(all_attrs, function(a) a$use == 'id')]
            co_var <- names(all_attrs)[sapply(all_attrs, function(a) a$use == 'Individual attribute')]

            # Obtain states that aren't in uploaded data and aren't first state (i.e. won't have column in csv)
            in_dataset <- sapply(seq_along(states()), function(i) {
                col_names <- paste(c('time', 'status'), states()[i], sep='.')
                (all(col_names %in% names(df)) || i == 1)
            })

            # If have any new states that weren't in original uploaded data
            # add time and status columns
            if (!all(in_dataset)) {
                last_followups <- apply(df[, time_vars], 1, max)

                for (s in states()[!in_dataset]){
                    status_col <- paste('status', s, sep='.')
                    time_col <- paste('time', s, sep='.')
                    # status column set to all 0 since these state transitions won't be observed
                    df[[status_col]] <- 0
                    # time column set to row-wise maximum, i.e. last follow-up
                    df[[time_col]] <- last_followups

                    # Add new columns to time and status vars
                    time_vars <- c(time_vars, time_col)
                    status_vars <- c(status_vars, status_col)
                }

            }

            if (!is.null(id_var)) {
                long <- msprep(time=c(NA, time_vars),
                               status=c(NA, status_vars),
                               data=df, trans=trans_mat, id=id_var, keep=co_var)
            } else {
                long <- msprep(time=c(NA, time_vars),
                               status=c(NA, status_vars),
                               data=df, trans=trans_mat,
                               keep=co_var)
            }
        })
    })

    ############################### Transition Probabilities ############################################################


    ############ Generic area for both specifying parameters and having them estimated from data
    output$seltransprobs <- renderUI({
        trans <- reactiveValuesToList(transitions)
        if (length(trans) < 1)
            return()

        trans_nonsel <- sapply(trans, function(t) is.null(t$draw))
        vals <- sapply(trans[trans_nonsel], function(t) t$index)
        labels <- sapply(trans[trans_nonsel], function(t) paste(states()[t$from],
                                                                states()[t$to], sep=' to '))
        ord <- order(vals)

        choices <- setNames(vals[ord], labels[ord])

        selectInput("transprob", "Transition", choices=choices)

    })

    output$transprobsbuttons <- renderUI({
        trans <- reactiveValuesToList(transitions)

        if (length(trans) < 1)
            return()

        if (is.null(input$transprob) || input$transprob == '')
            return()

        item_list <- list()
        item_list[[1]] <- actionButton("paramsspecifybutton", "Specify parameters manually")
        item_list[[2]] <- br()

        this_trans <- trans[sapply(trans, function(t) t$index == input$transprob)][[1]]
        from_state <- states()[this_trans$from]
        to_state <- states()[this_trans$to]

        from_states <- paste(c('time', 'status'), from_state, sep='.')
        to_states <- paste(c('time', 'status'), to_state, sep='.')

        # If source state isn't in data set (excluding starting state) then quit
        # TODO Really should combine this into a single boolean statement but am sure
        # will make a mistake when making it
        if (!all(from_states %in% names(uploaded_data())) && this_trans$from != 1) {

        } else if (!all(to_states %in% names(uploaded_data())) || this_trans$to == 1) {

        } else {
            item_list[[3]] <- br()
            item_list[[4]] <- actionButton("paramsestimatebutton", "Estimate parameters from data")
        }
        do.call(tagList, item_list)
    })

    observeEvent(input$paramsspecifybutton, {
        if (input$paramsspecifybutton > 0)
            output$addtransarea <- specify_params_area
    })

    observeEvent(input$paramsestimatebutton, {
        if (input$paramsestimatebutton > 0)
            output$addtransarea <- estimate_params_area
    })

    # Attribute levels that are allowed for the manual specification of variables
    allowed_attrs <- reactive({
        sapply(names(attributes), function(a) {
            attr <- attributes[[a]]
            if (attr$type == 'Continuous') {
                a
            } else if (attr$type == 'Categorical') {
                paste(a, attr$levels[-1], sep='.')  # TODO Shouldn't levels be stored in attributes rather than getting it from raw data?!
            }
        })

    })

    output$transprobssummary <- renderUI({
        trans <- reactiveValuesToList(transitions)
        have_dist <- sapply(trans, function(t) !is.null(t$draw))

        if (sum(have_dist) < 1)
            return(h5("No distributions defined yet"))

        txt <- paste0("<ul>", sapply(trans[have_dist], function(t) {
            paste0("<li>", states()[t$from], ' to ', states()[t$to],
                   "<ul>", # Start first level of indentation for each transition
                       "<li>Distribution: ", t$dist, "</li>",
                       "<li>Parameters: ",
                           "<ul>",
                           paste(sapply(seq_along(DISTS[[t$dist]]$params), function(i) {
                              paste("<li>", DISTS[[t$dist]]$params[[i]], ": ", t$params[i], "</li>")
                           }), collapse=''),
                           "</ul>", # Closing parameters list
                       "</li>", # Closing Parameters item
                   "</ul>", # Closing transition inner list
                   "</li>" # Closing transition item
                   )}),
            "</ul>" # Closing overall list
        )
        HTML(txt)
    })

    ############ Providing params manually
    specify_params_area <- renderUI({

        item_list <- list()
        item_list[[1]] <- HTML(paste("Enter any valid R expression with attribute names in brackets.",
                                     "Categorical variables must be specified as <code>[var_name.level]</code>",
                                     "omitting the baseline level.",
                                     "<br>",
                                     "For example:<br><code>exp(-3 + 0.25 * [age] + 1.25 * [sex.M])</code><br><br>"))
        item_list[[2]] <- allowed_params(allowed_attrs())
        item_list[[3]] <- seldist
        item_list[[4]] <- selparams
        item_list[[5]] <- addtransprobsbutton
        do.call(tagList, item_list)
    })


    allowed_params <- function(params) {
        if (length(params) < 1) {
            out <- h5("No attributes provided from a data source.")
        } else {
            attr_list <- paste0("<ul>", paste0(sapply(seq_along(params), function(b)
                                paste0("<li>", names(params)[b], "<ul>", paste0(sapply(params[[b]], function(d) paste0("<li><code>[", d, "]</code></li>")), collapse=''), "</ul>", "</li>")
                                ),
                                collapse=''),
                                "</ul>")
            item_list <- list()
            item_list[[1]] <- p("Available parameters:")
            item_list[[2]] <- HTML(attr_list)
            out <- do.call(tagList, item_list)
        }
        renderUI(out)
    }

    seldist <- renderUI({
        if (length(reactiveValuesToList(transitions)) < 1)
            return()

        selectInput("seldist", "Distribution", choices=names(DISTS[TIME_DISTS]))
    })

    selparams <- renderUI({
        if (length(reactiveValuesToList(transitions)) < 1 || is.null(input$seldist))
            return()

        item_list <- list()
        for (i in seq_along(DISTS[[input$seldist]]$params)) {
            item_list[[i]] <- textInput(paste0("param", i), DISTS[[input$seldist]]$params[i], "")
        }
        do.call(tagList, item_list)
    })

    addtransprobsbutton <- renderUI({
        if (length(reactiveValuesToList(transitions)) < 1)
            return()

        actionButton("transprobbutton", "Update")
    })

    observeEvent(input$transprobbutton, {
        index <- input$transprob
        trans_name <- names(transitions)[sapply(names(transitions), function(n) transitions[[n]]$index == index)]
        dist <- input$seldist
        params <- sapply(seq_along(DISTS[[dist]]$params), function(i) input[[paste0('param', i)]])

        # Confirm have covariate names in attributes list
        attr_names <- get_attr_names(params)

        attrs <- allowed_attrs()

        # See if have any categorical variables without all levels specified
        missing_cat_levels <- sapply(attrs, function(x) any(x %in% attr_names) && ! all(x %in% attr_names))

        if (!all(attr_names %in% unlist(attrs))) {
            print(paste("Error: some attribute names not loaded into simulation.", attr_names[!attr_names %in% attrs]))
        } else if (any(missing_cat_levels)) {
            print(paste("Error: Please specify all levels for the following covariates:", paste(names(attrs[missing_cat_levels]), collapse=', ')))
        } else if (any(params == '')) {
            print("Error: Please specify parameter(s).")
        }

        else {
            transitions[[trans_name]]$draw <- create_eventtime_draw(dist, params=params)
            transitions[[trans_name]]$params <- params
            transitions[[trans_name]]$dist <- dist
            # Reset the parameter specification area in preparation for next transition
            output$addtransarea <- renderUI({NULL})
        }

    })


    ############ Estimating params from data
    estimate_params_area <- renderUI({
        time_var <- names(all_raw_attrs())[sapply(all_raw_attrs(), function(x) x$use == 'time')]
        time_label <- if (length(time_var) >= 1) time_var else "unspecified"

        status_var <- names(all_raw_attrs())[sapply(all_raw_attrs(), function(x) x$use == 'status')]
        status_label <- if (length(status_var) >= 1) status_var else "unspecified"

        item_list <- list()
        item_list[[1]] <- var_checkbox
        item_list[[2]] <- if (length(status_var) > 0 && length(time_var) > 0) actionButton("estimateparamsbutton", "Estimate") else HTML("Please specify time and status variables in tab <strong>Upload Data</strong>")
        do.call(tagList, item_list)

    })

    var_checkbox <- renderUI({
        # Only display attribute checklist if have attributes
        # Only include attributes from the data set
        chosen_attrs <- names(attributes)[names(attributes) %in%
                                          names(all_raw_attrs()[sapply(all_raw_attrs(), function(a) a$use == 'Individual attribute')])]

        if (length(chosen_attrs) < 1)
            return()
        checkboxGroupInput("estimateparamsvars", "Attributes to include", choices=chosen_attrs)
    })

    observeEvent(input$estimateparamsbutton, {

        # Obtain details of transition under interest
        trans_index <- input$transprob
        trans_name <- names(transitions)[sapply(names(transitions), function(n) transitions[[n]]$index == trans_index)]

        # Obtain covariate names from checkboxes and names of categorical variables
        if (!is.null(input$estimateparamsvars)) {
            covars <- input$estimateparamsvars
            cat_vars <- covars[sapply(reactiveValuesToList(attributes)[covars], function(x) x$type == 'Categorical')]
        } else {
            covars <- NO_COVAR_FORMULA
            cat_vars <- NULL
        }

        # Obtain time and status variable from correct place
        time_var <- names(all_raw_attrs())[sapply(all_raw_attrs(), function(x) x$use == 'time')]
        status_var <- names(all_raw_attrs())[sapply(all_raw_attrs(), function(x) x$use == 'status')]

        # Subset data set to the transition of interest
        data <- long_data() %>%
                    filter(trans == trans_index)

        form <- paste0("Surv(Tstart, Tstop, status) ~ ", paste(covars, collapse='+'))

        # Iterate through each distribution and build model
        withProgress(message="Fitting models...", value=0, {
            mods <- lapply(TIME_DISTS, function(d) {
                tryCatch(withCallingHandlers(suppressWarnings(flexsurvreg(as.formula(form), data=data, dist=DISTS[[d]]$flex))),
                         error=function(e) NULL
                         )
            })
        })

        AIC <- sapply(mods, function(x) {
            if (!is.null(x)) {
                x$AIC
            } else {
                'Error'
            }
        })
        best_mod <- which.min(AIC)

        # Save AIC scores to table
        aic_scores[[trans_index]] <- data.frame(Distribution=TIME_DISTS, AIC=AIC)

        # Set the draw method for this transition to be the winning dist
        winning_dist <- TIME_DISTS[best_mod]
        winning_mod <- mods[[best_mod]]
        params_str <- create_param_string_from_mod(DISTS[[winning_dist]], winning_mod, cat_vars)
        transitions[[trans_name]]$draw <- create_eventtime_draw(winning_dist, params=params_str)
        transitions[[trans_name]]$params <- params_str
        transitions[[trans_name]]$dist <- winning_dist

        # Reset specification of transition probabilities area for next transition
        output$addtransarea <- renderUI({NULL})
    })

    aic_scores <- reactiveValues()

    output$aicdiv <- renderUI({
        tlist <- reactiveValuesToList(transitions)
        aic_list <- reactiveValuesToList(aic_scores)
        if (length(aic_list) == 0)
            return()

        item_list <- list()
        item_list[[1]] <- h3("Model Comparison")
        for (i in seq_along(aic_list)) {
            this_trans <- tlist[[names(tlist)[sapply(tlist, function(t) t$index == i)]]]
            item_list[[2*i]] <- h4(paste0("Transition: ", states()[this_trans$from], "-", states()[this_trans$to]))
            item_list[[2*i+1]] <- HTML(renderTable(aic_list[[i]] %>% arrange(AIC))())
        }
        do.call(tagList, item_list)
    })


    ############################### Display Event Time Distributions ##########################################################################

    output$newdataarea <- renderUI({
        if (length(reactiveValuesToList(transitions)) < 1)
            return()
        if (sum(sapply(reactiveValuesToList(transitions), function(t) !is.null(t$draw))) < 1)
            return()

        item_list <- list()
        item_list[[1]] <- hr()
        item_list[[2]] <- h4("New data for plotting")
        item_list[[3]] <- h5(paste("Specify the individual characteristics for the individual whose event time distributions are displayed opposite.",
                                   "Note that not all attributes are involved in each transition, but it's necessary to specify all attribute values regardless."))
        item_list[[4]] <- newdata_select
        item_list[[5]] <- actionButton("updatenewdata", "Plot")
        do.call(tagList, item_list)
    })

    newdata_select <- renderUI({
        item_list <- list()
        # Obtain attributes
        attrs <- reactiveValuesToList(attributes)

        # For each attribute obtain expected value to use as default selected value
        for (i in seq_along(attrs)){
            attr <- attrs[[i]]
            default_val <- names(attrs)[i]
            if (attr$type == 'Continuous') {
                default_val <- round(mean(attr$draw(NUM_SAMPLES_ATTRIBUTE_EXPECTED)), 2)
                item_list[[i]] <- textInput(paste0('newdata', names(attrs)[i]),
                                            label=names(attrs)[i],
                                            value=default_val)
            } else if (attr$type == 'Categorical') {
                # Have to make a mode function
                default_val <- attr$levels[which.max(table(factor(attr$draw(NUM_SAMPLES_ATTRIBUTE_EXPECTED), levels=attr$levels)))]
                item_list[[i]] <- selectInput(paste0('newdata', names(attrs)[i]),
                                              label=names(attrs)[i],
                                              choices=attr$levels,
                                              selected=default_val)
            } else {
                stop(paste0("Error: attribute '", attr, "' has unknown type: '", attr$type, "'."))
            }

        }
        do.call(tagList, item_list)
    })

    newdata <- eventReactive(input$updatenewdata, {
        attrs <- reactiveValuesToList(attributes)
        if (length(attrs) < 1)
            return(list())

        if (input$updatenewdata == 0) {
            attr <- attrs[[i]]
            sapply(attrs, function(a) {
                if (a$type == 'Continuous') {
                    # Mean of draws
                    round(mean(a$draw(NUM_SAMPLES_ATTRIBUTE_EXPECTED)), 2)
                } else if (attr$type == 'Categorical') {
                    # Have to make a mode function
                    a$levels[which.max(table(factor(a$draw(NUM_SAMPLES_ATTRIBUTE_EXPECTED), levels=a$levels)))]
                } else {
                    stop(paste0("Error: attribute '", attr, "' has unknown type: '", attr$type, "'."))
                }
            })
        } else {
            # If have pressed button then obtain data from inputs
            # Scrape data from the input UI elements, returning with nothing if haven't been setup yet
            inputs <- sapply(names(attrs), function(a) input[[paste0('newdata', a)]])
            if (any(sapply(inputs, is.null)))
                return(list())

            inputs
        }

    })


    output$plottingeventdraws <- renderPlot({
        if (length(reactiveValuesToList(transitions)) < 1)
            return()

        if (sum(sapply(reactiveValuesToList(transitions), function(t) !is.null(t$draw))) < 1)
            return()

        newdf <- newdata()

        if (length(newdf) != length(reactiveValuesToList(attributes)))
            return()

        # Convert raw string input to numeric data frame (including dummy variables for categorical attributes)
        newdf_numeric <- convert_stringdata_to_numeric(newdf)

        vars <- reactiveValuesToList(transitions)
        vars <- vars[!sapply(vars, function(v) is.null(v$draw))]  # Only include transitions with a specified draw method

        labels <- sapply(vars, function(t) paste(states()[t$from], states()[t$to], sep='-'))

        plots <- lapply(vars, function(v) {
            vals <- v$draw(NUM_TIMES_DRAWS_PREVIEW, newdata=newdf_numeric)
            ggplot(data.frame(x=vals), aes(x=x)) +
                    geom_histogram(fill='white', colour='black') +
                    theme_bw()
        })
        return(plot_grid(plotlist=plots, labels=labels))
    })


    ############################### Simulation ##########################################################################

    curr_time_reactive <- reactiveValues(curr_time=0, next_time=0)

    create_event <- function(id, time, attributes) {
        obj <- list(history=data.frame(state=NULL, entry_time=NULL),
                    curr_state = -1,
                    next_state = 1,
                    id = id,
                    time = time,
                    attributes = attributes,
                    next_transition = NULL # These get filled out later on
                    )
        class(obj) <- c(class(obj), 'event')
        obj
    }
    ERROR_MARGIN <- 1.25
    
    output$termcriteriadiv <- renderUI({
        method <- input$terminationcriteria
        if (is.null(method)) {
            return(NULL)
        }
        
        textInput("termcriteriavalue", "Value of termination criteria", value=method)
    })

    output$timedisplay <- renderTable({
        res <- simoutput()
        
        if (is.null(res)) 
            return(NULL)
        
        num_states <- length(states())
        num_in_states <- sapply(res, function(sim) {
            colSums(sapply(seq(3), function(state) {
                sapply(sim, function(ind) ind$curr_state == state)
            } ))
        })
        mean_num_in_states <- rowMeans(num_in_states)
        
        data.frame(state=seq(num_states), num=mean_num_in_states)
    })
    
    output$savebuttons <- renderUI({
        res <- simoutput()
        
        if (is.null(res)) 
            return(NULL)
        
        fluidRow(
            hr(),
            textInput("simulationname", "Simulation name"),
            downloadButton("savemodel", "Save model"),
            downloadButton("saveresults", "Save results")
        )
    })
    
    output$savemodel <- downloadHandler(
        filename = function() {
            paste0(input$simulationname, '_model.json')
        },
        content = function(file) {
            output <- list(
               'transitions' = lapply(reactiveValuesToList(transitions), function(t) {
                       list('source' = states()[t$from],
                            'target' = states()[t$to],
                            'distribution' = t$dist,
                            'parameters' = t$params)
                            }),
               'simulation_parameters' = list('termination_criteria'=input$terminationcriteria,
                                              'termination_value'=input$termcriteriavalue,
                                              'entry_rate'=input$entryrate)
            ) 
            write(toJSON(output), file)
        }
    )
    
    # TODO This method currently only produces wide formatted CSVs, where each state is either visited or not.
    # This doesn't allow for recurrent visits to states, even though my DES model allows this to happen.
    # I should really change the output of the DES to better reflect this, rather than forcing it into the 
    # wide mstate CSV method. 
    #
    # Maybe just return a long data frame with a row referring to a transitions with the following columns:
    #   - sim number
    #   - individual number
    #   - time
    #   - start state
    #   - end state
    # 
    # I could then write wrappers to convert it to mstate style wide tables, or long mstate transition tables
    # But essentially, since there are so many possibilities of what the user would want to do with the
    # simulation results, it's probably better to provide it as raw as possible and let them do the manual
    # tweaking to obtain the required format for their specific analysis
    output$saveresults <- downloadHandler(
        filename = function() {
            paste0(input$simulationname, '_results.csv')
        },
        content = function(file) {
            res <- simoutput()
            num_states <- length(states())
            full_output <- lapply(seq_along(res), function(sim_num) {
                if (input$terminationcriteria == 'Time limit') {
                    censor_time <- input$termcriteriavalue
                } else if (input$terminationcriteria == 'Number of individuals') {
                   censor_time <- max(sapply(res[[sim_num]], function(e) e$time))
                } else {
                    message(paste0("Error: Unknown termination criteria '", input$terminationcriteria, "'."))
                    return()
                }
                as.data.frame(t(sapply(seq_along(res[[sim_num]]), function(event_num) {
                    # Determine patient individual attributes
                    event <- res[[sim_num]][[event_num]]
                    attrs <- c(event_num, unlist(event$attributes))
                    
                    in_absorbtive_state <- event$curr_state == event$next_state
                    
                    # Determine state transition times and censoring values
                    event_occured <- sapply(seq(num_states), function(s) s %in% event$history[, 1])
                    states_occured <- seq(num_states)[event_occured]
                    status <- as.numeric(event_occured)
                    times <- rep(NA, num_states)
                    times[states_occured] <- sapply(states_occured, function(s) event$history[event$history[, 1] == s, 2])
                    times[-states_occured] <- if (in_absorbtive_state) max(times, na.rm=T) else censor_time
                    state_vals <- as.vector(sapply(seq(num_states), function(s) c(times[s], status[s])))
                    output <- c(attrs, state_vals)
                    output
                    })))
                })
            full_output <- as.data.frame(data.table::rbindlist(full_output, idcol='sim_num', use.names=TRUE))
            num_cols <- ncol(full_output)
            state_names <- c(sapply(states(), function(s) c(paste0(s, '.time'), paste0(s, '.status')))) 
            
            colnames(full_output)[(num_cols - num_states*2 + 1) : num_cols] <- state_names
            colnames(full_output)[2] <- 'event_id'
            
            # Format attributes into long format
            cat_attrs <- names(attributes)[sapply(names(attributes), function(a) attributes[[a]]$type == 'Categorical')]
            cont_attrs <- names(attributes)[sapply(names(attributes), function(a) attributes[[a]]$type == 'Continuous')]
            cat_levels <- setNames(lapply(cat_attrs, function(a) paste(a, attributes[[a]]$levels, sep='.')), cat_attrs)
            
            # Obtain factor value of categorical attributes
            cat_long <- apply(full_output, 1, function(row) {
                            setNames(lapply(cat_attrs, function(a) attributes[[a]]$levels[row[cat_levels[[a]]]==1]),
                                     cat_attrs)
                        })
            
            full_output <- full_output[, !(names(full_output) %in% unlist(cat_levels))] # Drop wide categorical variables
            full_output <- cbind(full_output, bind_rows(cat_long)) # Add long categorical variables
            
            # Reorder columns
            full_output <- full_output[, c('sim_num', 'event_id', cont_attrs, cat_attrs, state_names)]
            write.csv(full_output, file, quote=F, row.names = F)
        }
    )

    # TODO See if force is required here
    create_next_transition <- function(starting_state, attrs) {
        force(starting_state)
        force(attrs)

        # Subset transitions where have specific starting state and draw method
        trans <- reactiveValuesToList(transitions)
        trans <- trans[sapply(trans, function(t) t$from == starting_state && !is.null(t$draw))]
        force(trans)

        function() {
            if (length(trans) == 0)
                return(NULL)

            times <- sapply(trans, function(t) t$draw(1, newdata=attrs))
            # Obtain next event as next time
            next_time <- which.min(times)
            # Return the next state and its time
            c(times[next_time], trans[[next_time]]$to)
        }

    }

    # Function that runs when an event occurs. Creates the subsequent event and returns it
    process_event <- function(event) {
        # Create new event as copy of old
        new_event <- event
        # update current state
        new_event$curr_state <- event$next_state
        # Update history
        new_event$history <- rbind(new_event$history, c(event$next_state, event$time))
        # determine next state and time
        next_trans <- event$next_transition()

        # If in an absorbant state, return state as it is
        if (is.null(next_trans)) {
            return(new_event)
        }

        # update next state and time (time will be current time + time to next event)
        new_event$time <- next_trans[1] + event$time
        new_event$next_state <- next_trans[2]
        # set next_transition
        new_event$next_transition <- create_next_transition(new_event$next_state, new_event$attributes)

        new_event

    }
    
    termination_time <- function(max_time) {
        function(eventlist) {
            # If have empty list or the next value is greater than the limit then terminate
            !((length(eventlist) >= 1) && (eventlist[[1]]$time <= max_time))
        }
    }
    
    termination_individuals <- function(eventlist) {
        length(eventlist) < 1
    }

    run_simulation <- function() {
        # Create initial event_list
        event_list <- setup_eventlist()
        if (length(event_list) == 0) {
            message("Error in configuring event list. Please confirm all parameters are correct.") 
            return()
        }
        absorbant_list <- list()
        
        
        # Setup termination criteria
        if (input$terminationcriteria == "Time limit") {
            end_simulation <- termination_time(as.numeric(input$termcriteriavalue))
        } else if (input$terminationcriteria == "Number of individuals") {
            end_simulation <- termination_individuals 
        } else {
            message(paste0("Error: Unknown termination criteria option '", input$terminationcriteria, "'."))
            return()
        }

        # Peak at top of list and see time of next event
        while (!end_simulation(event_list)) {
            # Processes event on top of event_list and updates both lists
            new_state <- run_timestep(event_list, absorbant_list)
            event_list <- new_state[[1]]
            absorbant_list <- new_state[[2]]
        }
        append(event_list, absorbant_list)
    }

    setup_eventlist <- function() {
        entryrate <- tryCatch(entryrate <- as.numeric(input$entryrate),
                              warning=function(w) {
                                     message("Error: Please provide a numeric value for entry rate.")
                                     return(NULL)
                              })
        termcriteria <- tryCatch(as.numeric(input$termcriteriavalue),
                                 warning=function(w) {
                                     message("Error: Please provide a numeric value for termination criteria.")
                                     return(NULL)
                                 })
        
        if (is.null(entryrate) || is.null(termcriteria))
            return()
        
        if (input$terminationcriteria == "Time limit") {
            # If specify time limit then number of individuals is rate * time limit, plus an error margin
            initial_n <- ERROR_MARGIN * (entryrate * termcriteria)
        } else if (input$terminationcriteria == "Number of individuals") {
            # Otherwise can specify number of individuals directly
            initial_n <- termcriteria
        } else {
            message(paste0("Error: Unknown termination criteria option '", input$terminationcriteria, "'."))
        }
        
        entry_times <- cumsum(rexp(initial_n, entryrate))
        # Remove values which are above maximum time if using one
        if (input$terminationcriteria == "Time limit") {
            entry_times <- entry_times[entry_times < termcriteria]
        }
        n_inds <- length(entry_times)
        
        if (n_inds == 0)
            return(NULL)

        # Create attributes for all individuals
        attrs <- data.frame(lapply(reactiveValuesToList(attributes), function(x) x$draw(n_inds)))
        attrs_numeric <- bind_rows(apply(attrs, 1, convert_stringdata_to_numeric))

        # Create Event objects containing:
        #   index
        #   time of entry into state 1
        #   attributes
        new_events <- lapply(seq(n_inds), function(i) {
            create_event(i, entry_times[i], attrs_numeric[i, ])
        })

        # Create functions to determine the next event for each of these
        for (i in seq_along(new_events)) {
            new_events[[i]]$next_transition <- create_next_transition(new_events[[i]]$next_state, new_events[[i]]$attributes)
        }
        new_events
    }

    run_timestep <- function(event_list, absorbant_list) {
        nevent_list <- event_list
        nabsorbant_list <- absorbant_list

        # Pop the current event from top of list
        curr_event <- nevent_list[[1]]
        nevent_list[[1]] <- NULL
        curr_time <- curr_event$time

        # Process the current event to obtain its next transition
        new_event <- process_event(curr_event)

        # If not in an absorbant state and have a next transition then add to queue
        if (new_event$time > curr_time) {
            # Insert new_event into appropriate place
            times <- sapply(nevent_list, function(e) e$time)
            # -1 as append below is the index AFTER which to insert value at
            new_index <- if (length(times) == 0 || new_event$time < min(times)) {
                            0
                         } else if (new_event$time > max(times)) {
                            length(times) + 1
                         } else {
                            min(which(times - new_event$time > 0)) - 1
                         }
            nevent_list <- append(nevent_list, list(new_event), new_index)
        } else {
            nabsorbant_list <- append(nabsorbant_list, list(new_event), length(nabsorbant_list)+1)
        }

        # Update times for display
        curr_time_reactive$curr_time <- curr_event$time
        curr_time_reactive$next_time <- if (length(nevent_list) > 0) nevent_list[[1]]$time else NA

        # Return updated event lists
        list(nevent_list, nabsorbant_list)
    }

    simoutput <- eventReactive(input$runmultiplesimbutton, {
        n_sims <- input$simslider
        withProgress(message="Simulating...", value=0, {
            print(system.time({
                platform <- .Platform$OS.type
                
                # TODO PROFILE CODE
                #Rprof("profile.txt", interval=0.1)
                if (platform == "unix" && n_sims > 1) {
                    end_states <- mclapply(seq(n_sims), function(i) run_simulation(),
                                           mc.cores=4)
                } else {
                    end_states <- lapply(seq(n_sims), function(i) run_simulation())
                }
                #Rprof(NULL)
            }))
        })
        end_states
    })

})