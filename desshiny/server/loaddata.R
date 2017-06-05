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
        attrs <- all_raw_attrs()
        raw_attrs <- names(attrs)
        item_list <- list()

        for (i in seq_along(raw_attrs)) {

            # Obtain type information
            attr <- attrs[[i]]
            if (attr$type == 'Continuous') {
                type_info <- 'continuous'
            } else {
                type_info <- paste('categorical with ', length(attr$levels), "values")
            }
            item_list[[i]] <- selectInput(paste0("rawattr", raw_attrs[i]),
                                          HTML(paste0(raw_attrs[i], ' (', type_info, ')')),
                                          choices=COVAR_TYPES,
                                          selected=attrs[[i]]$use)
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

    attrs <- all_raw_attrs()

    attr_list <- paste0("<ul>", paste0(sapply(COVAR_TYPES, function(b) {
                            entries <- names(attrs)[sapply(attrs, function(d) b %in% d$use)]
                            paste0("<li>", b, "<ul>", paste0(sapply(entries, function(d) paste0("<li><code>", d, "</code></li>")), collapse=''), "</ul>", "</li>")
                        }),
                        collapse=''),
                        "</ul>")
    item_list <- list()
    item_list[[1]] <- p("The following column types have been linked with the following variables in the uploaded data set:")
    item_list[[2]] <- HTML(attr_list)
    do.call(tagList, item_list)

})