#' Displays the predicted flow for a given individual through the system
#'
#' Produces an interactive HTML widget that displays a Sankey diagram showing a
#' predicted patient pathway through the multi-state model.
#'
#' @inheritParams predict_transitions
#' @param newdata A data frame containing the attributes of the person
#'   to display the predicted state flow for. As the diagram can only
#'   be displayed for a single individual it will ignore any rows
#'   after the first.
#' @param times The time-points at which to estimate transition
#'  probabilities.
#' @param starting_state Starting state. Either number or character name in \code{trans_mat}.
#' @return The HTML widget.
#' 
#' @examples 
#' 
#' library(multistateutils)
#' library(mstate)
#' library(flexsurv)
#' 
#' # Convert data to long
#' data(ebmt3)
#' tmat <- trans.illdeath()
#' long <- msprep(time=c(NA, 'prtime', 'rfstime'), 
#'                status=c(NA, 'prstat', 'rfsstat'), 
#'                data=ebmt3, 
#'                trans=tmat, 
#'                keep=c('age', 'dissub'))
#'                
#' # Fit parametric models
#' models <- lapply(1:3, function(i) {
#'     flexsurvreg(Surv(time, status) ~ age + dissub, data=long, dist='weibull')
#' })
#' 
#' # New individual to estimate transition probabilities for
#' newdata <- data.frame(age="20-40", dissub="AML")
#' 
#' # Plot pathway diagram at 2-yearly intervals up to 10-years 
#' time_points <- seq(0, 10, by=2) * 365.25
#' 
#' \donttest{
#' plot_predicted_pathway(models, tmat, newdata, time_points, 1)
#' }
#' 
#' @export
plot_predicted_pathway <- function(models, trans_mat, newdata, times, starting_state=1, 
                                   tcovs=NULL) {
    
    # R CMD CHECK
    start_time <- NULL
    end_time <- NULL
    start_state <- NULL
    end_state <- NULL
    prob <- NULL
    entering_prob <- NULL
    prob_scale <- NULL
    
    all_states <- colnames(trans_mat)
    sinks <- get_sink_states(trans_mat)
    
    # This validates starting state and returns it as int. Index again to get state name
    starting_state_int <- validate_starting_state(starting_state, trans_mat)
    starting_state <- all_states[starting_state_int]
    
    # Obtain names of states that are possible to visit in this pathway
    visited_states_int <- get_visited_states(starting_state_int, trans_mat)
    states <- all_states[visited_states_int]
    
    if (nrow(newdata) > 1)
        newdata <- newdata[1, ]
    
    if (times[1] != 0) {
        times <- c(0, times)
    }
    
    
    # For each time-point I want to:
    #  - Get transition probability of next time given current
    #  - combine together
    all_probs <- dplyr::bind_rows(lapply(seq(length(times)-1), function(i) {
        start_time <- times[i]
        end_time <- times[i+1]
        predict_transitions(models, newdata=newdata,
                            trans_mat=trans_mat, times=end_time, start_times=start_time,
                            tcovs=tcovs)
    }))
    
    # Make dataframe to hold all transitions
    clean_probs <- all_probs %>%
                dplyr::mutate(start_time = match(start_time, times),
                              end_time = match(end_time, times)) %>%
                dplyr::select(start_time, end_time, start_state, dplyr::one_of(states)) %>%
                dplyr::filter(start_state %in% states) %>%
                tidyr::gather(end_state, prob, -start_time, -end_time, -start_state) %>%
                dplyr::filter(!(start_time == 1 & start_state != starting_state)) %>%
                dplyr::mutate(start_state = sprintf("%s.%d", start_state, start_time),
                              end_state = sprintf("%s.%d", end_state, end_time)) %>%
                dplyr::filter(prob > 0.0)

    # Scale the transition probabilities to global
    entering_state_prob <- clean_probs %>%
                             dplyr::group_by(end_state) %>%
                             dplyr::summarise(entering_prob = sum(prob)) %>%
                             dplyr::rename(state=end_state) %>%
                             rbind(data.frame(state=paste0(starting_state, '.1'), entering_prob=1))

    clean_probs$prob_scale <- clean_probs$prob

    # Iterate through each year, updating transition probabilites
    for (i in seq_along(times)) {
        it_states <- if (i == 1) starting_state else states
        for (state in it_states) {
            comb_state <- paste(state, i, sep='.')
            state_cum_prob <- entering_state_prob %>%
                                dplyr::filter(state == comb_state) %>%
                                dplyr::pull(entering_prob)

            # Rescale probabilities exiting this state
            clean_probs$prob_scale[clean_probs$start_state == comb_state] <- clean_probs$prob_scale[clean_probs$start_state == comb_state] * state_cum_prob
        }

        # Update starting states cumulative probability
        entering_state_prob <- clean_probs %>%
                                 dplyr::group_by(end_state) %>%
                                 dplyr::summarise(entering_prob = sum(prob_scale)) %>%
                                 dplyr::rename(state=end_state)
    }
    
    # Form data frames holding node information for sankey
    nodes <- data.frame(name=c(paste0(starting_state, '.1'),
                               paste(rep(states, each=length(times)-1),
                                     seq_along(times)[-1],
                                     sep='.')))
    nodes$group <- gsub("\\.[0-9]+", "", nodes$name)
    nodes$text <- ifelse(grepl(paste0("\\.", length(times)), nodes$name),
                         nodes$group, 
                         "")

    # Then need to add IDs 
    # These also must be zero indexed
    clean_probs$IDsource <- match(clean_probs$start_state, nodes$name) - 1
    clean_probs$IDtarget <- match(clean_probs$end_state, nodes$name) - 1
    clean_probs$group <- gsub("\\.[0-9]+", "", clean_probs$start_state)

    clean_probs$percent <- clean_probs$prob_scale * 100
    
    networkD3::sankeyNetwork(Links=clean_probs, Nodes=nodes,
                             Source="IDsource", Target="IDtarget",
                             Value="percent", NodeID="text", NodeGroup="group", LinkGroup = "group",
                             nodePadding=50,
                             fontSize=12,
                             sinksRight = FALSE,
                             iterations = 100)
}
