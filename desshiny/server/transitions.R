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

    selectInput("transnumber", "Transition", choices=choices)

})

output$transprobsbuttons <- renderUI({
    trans <- reactiveValuesToList(transitions)

    if (length(trans) < 1)
        return()

    if (is.null(input$transnumber) || input$transnumber == '')
        return()

    item_list <- list()
    item_list[[1]] <- actionButton("paramsspecifybutton", "Specify parameters manually")
    item_list[[2]] <- br()

    this_trans <- trans[sapply(trans, function(t) t$index == input$transnumber)][[1]]
    from_state <- states()[this_trans$from]
    to_state <- states()[this_trans$to]

    from_states <- paste(c('time', 'status'), from_state, sep='.')
    to_states <- paste(c('time', 'status'), to_state, sep='.')

    # If source state isn't in data set (excluding starting state) then quit
    # Really should combine this into a single boolean statement but am sure
    # will make a mistake when making it
    if (!all(from_states %in% names(uploaded_data())) && this_trans$from != 1) {

    } else if (!all(to_states %in% names(uploaded_data())) || this_trans$to == 1) {

    } else {
        item_list[[3]] <- br()
        # TODO Display text that "no params selected in previous tab" when want to estimate params from data and no attrs are available
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
            paste(a, attr$levels[-1], sep='.')
        }
    })

})

clean_parameter_string <- function(raw) {
    raw_split <- strsplit(raw, "\\+")[[1]]
    num_terms <- length(raw_split) - 1
    intercept <- round(as.numeric(raw_split[1]), 2)

    if (num_terms > 1) {
        coefs <- raw_split[2:length(raw_split)]
        coefs_clean <- sapply(coefs, function(coef) {
            coef_split <- strsplit(coef, "\\*")[[1]]
            val <- abs(as.numeric(coef_split[2]))
            operator <- if (sign(val) == 1) '+' else '-'
            sprintf("%s %.2f<b>%s</b>", operator, val, coef_split[1])
        })
    } else {
        coefs_clean <- ''
    }

    coefs_clean

    paste(intercept, paste(coefs_clean, collapse=' '))
}



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
                          paste("<li>", DISTS[[t$dist]]$params[[i]], ": ", clean_parameter_string(t$params[i]), "</li>")
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

    actionButton("manualaddtrans", "Update")
})

observeEvent(input$manualaddtrans, {
    index <- input$transnumber
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
    } else {
        transitions[[trans_name]]$draw <- create_eventtime_draw(dist)
        transitions[[trans_name]]$params <- params
        transitions[[trans_name]]$dist <- dist
        # TODO Add coefficients

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
    trans_index <- input$transnumber
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
    num_dists <- length(TIME_DISTS)

    # Iterate through each distribution and build model
    withProgress(message="Fitting models", value=0, max=num_dists, {

        # Update progress
        mods <- lapply(seq(num_dists), function(i) {
            d <- TIME_DISTS[[i]]
            incProgress(1, detail=paste(d))

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
    transitions[[trans_name]]$params <- params_str
    transitions[[trans_name]]$coefs <- get_coefs_from_mod(DISTS[[winning_dist]], winning_mod, cat_vars)
    transitions[[trans_name]]$dist <- winning_dist
    transitions[[trans_name]]$draw <- create_eventtime_draw(winning_dist)

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

