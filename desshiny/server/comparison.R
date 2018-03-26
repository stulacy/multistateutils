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
    vars <- reactiveValuesToList(transitions)
    if (length(vars) < 1)
        return()

    if (sum(sapply(vars, function(t) !is.null(t$draw))) < 1)
        return()

    newdf <- newdata()

    if (length(newdf) != length(reactiveValuesToList(attributes)))
        return()

    # Convert raw string input to numeric data frame (including dummy variables for categorical attributes)
    newdf_numeric <- convert_stringdata_to_numeric(newdf, reactiveValuesToList(attributes))
    vars <- vars[!sapply(vars, function(v) is.null(v$draw))]  # Only include transitions with a specified draw method
    labels <- sapply(vars, function(t) paste(states()[t$from], states()[t$to], sep='-'))

    plots <- lapply(vars, function(v) {
        params <- t(calculate_parameters(v$params, newdf_numeric, 1))  # TODO What is this last paramter used for?
        vals <- v$draw(NUM_TIMES_DRAWS_PREVIEW, params)
        ggplot(data.frame(x=vals), aes(x=x)) +
                geom_histogram(fill='white', colour='black') +
                theme_bw()
    })
    return(plot_grid(plotlist=plots, labels=labels))
})
