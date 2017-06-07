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
    if (is.null(input$updatestates))
        return()

    # If have pressed update button then get states from the inputs
    main_states <- if (input$updatestates > 0) {
        isolate({
            num_states <- if (is.null(input$stateslider)) NUM_STARTING_STATES else input$stateslider
            # This should never be true but best to be careful
            sapply(seq(num_states), function (i) {
                ip <- input[[paste0("statename", i)]]
                if (is.null(ip)) LETTERS[i] else ip
            })
        })
    } else {
        starting_states()
    }
    main_states
})

have_age <- reactive({
    attrs <- reactiveValuesToList(attributes)
    "age" %in% names(attrs) && attrs[['age']]$type == "Continuous"
})


sink_states <- reactive({
    trans_mat <- Q()
    row.names(trans_mat)[apply(trans_mat, 1, function(x) all(is.na(x)))]
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

Q <- reactive({
    this_states <- states()
    mat <- matrix(NA, length(this_states), length(this_states), dimnames=list(this_states, this_states))
    for (i in names(transitions)) {
        mat[transitions[[i]]$from, transitions[[i]]$to] <- transitions[[i]]$index
    }
    mat
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
    this_trans <- reactiveValuesToList(transitions)
    this_states <- states()
    from <- sapply(this_trans, function(x) this_states[x$from])
    to <- sapply(this_trans, function(x) this_states[x$to])
    data.frame(From=from, To=to)
})


output$statedia <- renderGrViz({

    trans_mat <- Q()
    trans_mat[trans_mat > 1] <- 1
    graph <- from_adj_matrix(trans_mat, mode="directed")
    #graph <- set_node_attrs(graph, "style", "solid")
    #graph <- set_node_attrs(graph, "color", "black")
    render_graph(graph)

})

output$probsheader <- renderUI({
    if (length(reactiveValuesToList(transitions)) < 1) {
        h5("Add transitions before specifying their probability distributions")
    } else {
        return()
    }
})

