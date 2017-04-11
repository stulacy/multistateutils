run_simulation <- function(transitions, transmat, initial_times) {
    raw_mat <- .Call('des_desCpp', PACKAGE = 'des', transitions, transmat, initial_times)
    history <- data.frame(raw_mat)
    colnames(history) <- c('id', 'state', 'time')
    history
}

