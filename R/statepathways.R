#' Displays the predicted flow for a given individual through the system
#'
#' Produces an interactive HTML widget that displays a Sankey diagram showing a
#' predicted patient pathway through the multi-state model.
#'
#' @param models A list of \code{flexsurv} survival models for
#'   each transition in \code{trans_mat}.
#' @param trans_mat A transition matrix.
#' @param newdata A data frame containing the attributes of the person
#'   to display the predicted state flow for. As the diagram can only
#'   be displayed for a single individual it will ignore any rows
#'   after the first.
#' @param times The time-points at which to estimate transition
#'  probabilities.
#' @return The HTML widget.
#' @export
state_pathway_flow <- function(models, trans_mat, newdata, times) {

    time_points <- seq(0, 10, by=2)

    # For each time-point I want to:
    #  - Get transition probability of next time given current
    #  - combine together
    all_probs <- bind_rows(lapply(seq(length(time_points)-1), function(i) {
        start_time <- time_points[i]
        end_time <- time_points[i+1]
        predict_transitions(mod_simp_2, newdata=data.frame(age_arrival=65),
                            trans_mat=trans_mat_simp, times=end_time, start_times=start_time,
                            tcovs=c('age_arrival'))
    }))


    # Make dataframe to hold all transitions
    foo <- all_probs %>%
                select(-age_arrival) %>%
                gather(end_state, prob, -start_time, -end_time, -start_state) %>%
                filter(!(start_time == 0 & start_state %in% c('Further treatment', 'Transformation', 'Death'))) %>%
                       #!(start_state == 'Transformation' & end_state == 'Transformation'),
                       #!(start_state == 'Death' & end_state == 'Death')) %>%
                mutate(start_state = sprintf("%s.%d", start_state, start_time),
                       end_state = sprintf("%s.%d", end_state, end_time)) %>%
                filter(prob > 0.0) %>%
                rbind(data.frame(start_time=0, end_time=0, start_state='Diagnosis',
                                 end_state=c('Observation.0', 'Chemotherapy.0', 'Radiotherapy.0'),
                                 prob=c(0.4234801, 0.4350105, 0.1415094))) %>%
                mutate(percent=prob * 100)

    # Scale the transition probabilities to global
    entering_state_prob <- foo %>%
                             group_by(end_state) %>%
                             summarise(entering_prob = sum(prob)) %>%
                             rename(state=end_state)

    foo$prob_scale <- foo$prob

    # Iterate through each year, updating transition probabilites
    for (i in time_points) {
        if (i == 0) {
            states <- c('Observation', 'Chemotherapy', 'Radiotherapy')
        } else {
            states <- c('Observation', 'Chemotherapy', 'Radiotherapy', 'Further treatment', 'Transformation', 'Death')
        }
        for (state in states) {
            comb_state <- paste(state, i, sep='.')
            state_cum_prob <- entering_state_prob %>%
                                filter(state == comb_state) %>%
                                pull(entering_prob)

            # Rescale probabilities exiting this state
            foo$prob_scale[foo$start_state == comb_state] <- foo$prob_scale[foo$start_state == comb_state] * state_cum_prob
        }

        # Update starting states cumulative probability
        entering_state_prob <- foo %>%
                                 group_by(end_state) %>%
                                 summarise(entering_prob = sum(prob_scale)) %>%
                                 rename(state=end_state)
    }


    # Form data frames holding node information for sankey
    nodes <- data.frame(name=c('Diagnosis',
                               paste0(c('Observation', 'Chemotherapy', 'Radiotherapy'),
                                     '.0'),
                               paste(rep(c('Observation', 'Chemotherapy', 'Radiotherapy',
                                         'Further treatment', 'Transformation', 'Death'),
                                       each=length(time_points)-1),
                                   time_points[-1],
                                   sep='.')))
    nodes$group <- gsub("\\.[0-9]+", "", nodes$name)
    nodes$text <- ifelse(grepl("\\.[0-9]{1}$", nodes$name), "",
                         nodes$group)

    # Then need to add IDs as per this tutorial: https://www.r-graph-gallery.com/321-introduction-to-interactive-sankey-diagram-2/
    # These also must be zero indexed
    foo$IDsource <- match(foo$start_state, nodes$name) - 1
    foo$IDtarget <- match(foo$end_state, nodes$name) - 1
    foo$group <- gsub("\\.[0-9]+", "", foo$start_state)

    foo$percent <- foo$prob_scale * 100

    sankeyNetwork(Links=foo, Nodes=nodes,
                  Source="IDsource", Target="IDtarget",
                  Value="percent", NodeID="text", NodeGroup="group", LinkGroup = "group",
                  nodePadding=50,
                  fontSize=12,
                  sinksRight = FALSE,
                  iterations = 100)
}
