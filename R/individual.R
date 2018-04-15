#' Runs an individual level simulation
#'
#' @param transitions A list of transitions as required by \code{desCpp}.
#' @param newdata_mat New data already in data matrix format as a single row
#' @param trans_mat Transition matrix
#' @param N Number of times to replicate this individual.
#' @return Usual output from DES, with state entry times
individual_simulation <- function(transitions, newdata_mat, trans_mat, N) {
    
    # Form new data into matrix
    mat_exp <- t(newdata_mat)[rep(1, N), ]
    initial_times <- rep(0, nrow(mat_exp))

    # Split people to evenly start in non-sink states. 
    # Assign 1 person per sink state to get probability of 1
    is_sink <- apply(trans_mat, 1, function(col) all(col == 0))
    sink_states <- unname(which(is_sink) - 1)  # 0-index for c++
    non_sink <- setdiff(seq(ncol(trans_mat))-1, sink_states)
    
    # Form vector of starting states, one per individual uniformly distributed
    start_states_long <- sample(non_sink, N-length(sink_states), replace=T)
    start_states_long <- c(start_states_long, sink_states)

    desCpp(transitions, trans_mat, mat_exp, initial_times, start_states_long)
}

#' Estimates transition probabilities and length of stay
#'
#' Estimates various measures of an individual's passage through a multi-state model
#' by discrete event simulation.
#'
#' @param models List of \code{flexsurvreg} objects.
#' @param newdata Data frame with covariates of individual to simulate times for. Must contain all fields
#'   required by models.
#' @param trans_mat Transition matrix, such as that used in \code{mstate}.
#' @param times Times at which to estimate transition probabilities. If not provided then doesn't estimate
#'   transition probabilities, just length of stay.
#' @param start_time Conditional time for transition probability.
#' @param N Number of times to repeat the individual
#' @param M Number of times to run the simulations in order to obtain confidence estimates
#' @export
#' @return A list for each individual with items for length of stay and transition probabilities.
#' 
#' @importFrom magrittr '%>%'
#' @import data.table 
predict_transitions <- function(models, newdata, trans_mat, times,
                                start_time=0,  N=10000, M=1000) {

    if (ncol(trans_mat) != nrow(trans_mat)) {
        stop(paste0("Error: trans_mat has differing number of rows and columns (",
                    nrow(trans_mat), " and ",
                    ncol(trans_mat), ")."))
    }
    
    # Required by CRAN checks
    id <- NULL
    state <- NULL
    time <- NULL
    individal <- NULL
    num_start <- NULL
    from <- NULL
    individual <- NULL
    
    # TODO More guards! Check nature of trans_mat, check that covariates required
    # by all models are in newdata

    # Obtain attributes as a matrix
    attr_mat <- form_model_matrix(newdata, models)

    # Convert models to list of transitions as required
    transition_list <- lapply(models, obtain_model_coef, attr_mat)

    # Ensure that the transition matrix doesn't have NA values, replacing these with 0
    trans_mat[is.na(trans_mat)] <- 0

    # Run simulation for each individual
    res <- lapply(1:nrow(newdata), function(i) {
        individual_simulation(transition_list, attr_mat[i, ], trans_mat, N)
    })

    # Obtain neat labels for results detailing individual attributes
    label_df <- data.frame(lapply(newdata, as.character), stringsAsFactors=FALSE)
    names(res) <- sapply(1:nrow(newdata), function(i) {
        paste(paste(colnames(label_df), label_df[i, ], sep='='), collapse=',')
    })
    
    state_names <- colnames(trans_mat)
    nstates <- nrow(trans_mat)

    resDT <- data.table::rbindlist(lapply(res, data.table::as.data.table), idcol='individual')
    data.table::setnames(resDT, c('individual', 'id', 'state', 'time'))

    # Find state was in at start time
    # Obtain state that a person is in at starting times
    start_states <- resDT[, .(from = state[findInterval(start_time, time)]),by=.(individual, id)]
    
    # Obtain state that a person is in at all times want to calculate probabilities for
    end_states <- resDT[, .(end = state[findInterval(times, time)]), by=.(individual, id)]
    end_states[, time := times ]
    
    # Join the two tables together
    combined <- start_states[end_states, on=c('individual', 'id')]
    
    # Calculate transition probabilities
    counts <- data.table::dcast(combined, individual + time + from ~ end, value.var='time', length)
    end_state_names <- colnames(counts)[4:ncol(counts)]  # First 3 rows are start_state, time, individual
    # Calculate the number in the starting state at specified starting time
    counts[, num_start:=sum(.SD), .SDcols=end_state_names, by=.(from, time, individual)]
    
    # Calculate proportions
    proportions <- counts[, lapply(.SD, function(x) x / num_start), 
                          .SDcols=end_state_names, 
                          by=.(individual, time, from)]
    proportions_df <- as.data.frame(proportions)
    
    # Split individual into columns
    covars <- colnames(newdata)
    proportions_df <- proportions_df %>%
        tidyr::separate(individual, sep=',', into=covars)
    
    proportions_df[covars] <- lapply(proportions_df[covars], 
                                     function(x) gsub("[[:alnum:]]+=", "", x))
    
    proportions_df <- proportions_df %>%
                            dplyr::mutate(from = factor(from, 
                                                        levels=seq_along(state_names)-1,
                                                        labels=state_names))
    colnames(proportions_df)[ (ncol(proportions_df) - nstates + 1) : ncol(proportions_df) ] <- state_names
    proportions_df

    # TODO Calculate length of stay for each state in 'states'

    # TODO Add in repeating whole thing M times to obtain standard errors

}