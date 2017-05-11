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
                                            draw=create_empirical_sample_func(attr_name, uploaded_data()),
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
