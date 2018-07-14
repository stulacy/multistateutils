#' Converts long state entry data into format suitable for multi-state modelling
#' 
#' This function performs the same role as \code{msprep} from \code{mstate}, except
#' that it accepts data in a long (each row corresponds to a state entry) rather
#' than the wide format (each row corresponding to an individual with state entry
#' indicated in columns) used by \code{msprep}.
#' 
#' The wide format already requires a bit of data munging, whereas the long format
#' accepted by \code{msprep2} might be a more natural fit for some data sets.
#' 
#' @param entry Long data frame of format id | state | time.
#'   State can either be character, with the names the same as in \code{tmat}, or
#'   integer where they refer to the rownumber of that state in \code{tmat}.
#' @param tmat transition matrix in the standard format required by \code{msprep}.
#' @param censors A long data frame with 2 columns, id and censor_time. Gives the last
#'   date of follow-up for individuals that haven't entered a sink state.
#' @param start_times A long data frame with 2 columns, id and start_time. Gives
#'   the time at which the patient entered the simulation. Defaults to 0.
#' @param start_states A long data frame with 2 columns, id and start_state. Gives
#'   the state the patient entered the simulation in. Defaults to 1.
#' @param covars Data frame where each row corresponds to an individual and details their
#'   covariate values. Must contain the id column specified in \code{idcol} alongside
#'   any covariate fields of interest.
#' @param idcol The column that indexes these patients, must be present in 
#'   \code{entry} and \code{censors}, \code{start_times}, \code{start_states}, 
#'   and \code{covars} if supplied.
#' @importFrom magrittr '%>%'
msprep2 <- function(entry, tmat, censors=NULL, 
                    start_times=NULL, start_states=NULL, covars=NULL,
                    idcol='id') {
    # TODO Do I need start_times & start_states or should I make it compulsory to 
    # include these in entry?
    
    # R CMD CHECK
    id <- NULL
    Tstop <- NULL
    Tstart <- NULL
    state <- NULL
    prev_state <- NULL
    start_state <- NULL
    start_time <- NULL
    time <- NULL
    status <- NULL
    from <- NULL
    to <- NULL
    to.actual <- NULL
    to.possible <- NULL
    trans <- NULL
    censor_time <- NULL
    
    # TODO How to make this function work for repeated state entry?
    # TODO Guards
    entry <- entry %>% 
                dplyr::rename(id = idcol)
    # Build up list of unique_ids
    unique_ids <- unique(entry$id)
    
    if (!is.null(censors)) {
        censors <- censors %>% 
                    dplyr::rename(id = idcol)
        unique_ids <- union(unique_ids, unique(censors$id))
    }
    if (!is.null(start_times)) {
        start_times <- start_times %>% 
                    dplyr::rename(id = idcol)
        unique_ids <- union(unique_ids, unique(start_times$id))
    }
    if (!is.null(start_states)) {
        start_states <- start_states %>% 
                    dplyr::rename(id = idcol)
        unique_ids <- union(unique_ids, unique(start_states$id))
    }
    if (!is.null(covars)) {
        covars <- covars %>% 
                    dplyr::rename(id = idcol)
        unique_ids <- union(unique_ids, unique(covars$id))
    }
    
    if (is.null(start_states))
        start_states <- data.frame(id=unique_ids, start_state=1)
    if (is.null(start_times))
        start_times <- data.frame(id=unique_ids, start_time=0)
    
    # Convert state names to numbers
    state_names <- colnames(tmat)
    
    if (is.character(entry$state))
        entry$state <- match(entry$state, state_names)
    
    ntrans <- sum(!is.na(tmat))
    ninds <- length(unique_ids)
    
    # Now need to add starting state and times.
    # Firstly, obtain the rank order of each state entry
    # TODO This would need guard on initial data input that each time
    # is individual for each individual
    entry2 <- entry %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(entry_order = dplyr::row_number(Tstop)) %>%
        dplyr::mutate(prev_state = dplyr::lag(state)) %>%
        dplyr::left_join(start_states, by='id') %>%
        dplyr::mutate(prev_state = ifelse(is.na(prev_state), start_state, prev_state)) %>%
        dplyr::select(-start_state)
    
    # Likewise, obtain previous times
    entry3 <- entry2 %>% 
        dplyr::mutate(Tstart = dplyr::lag(Tstop)) %>%
        dplyr::left_join(start_times, by='id') %>%
        dplyr::mutate(Tstart = ifelse(is.na(Tstart), start_time, Tstart)) %>%
        dplyr::select(-start_time) %>%
        dplyr::ungroup() %>%       
        dplyr::mutate(time = Tstop - Tstart, status=1) %>%
        dplyr::select(id, from=prev_state, to=state, Tstart, Tstop, time, status)
    
    # Now need to add the transitions that werent made
    # Need DF of all possible ids and transitions
    nstates <- ncol(tmat)
    sink_states <- match(get_sink_states(tmat), state_names)
    trans_ids <- tmat[!is.na(tmat)]
    
    tmat_long <- dplyr::bind_rows(lapply(stats::setNames(trans_ids, trans_ids), function(transition) {
        index <- which(tmat == transition) - 1  # Easier to do column/row index arithmetic in zero base
        from <- (index %% nstates) + 1          # Convert back to 1-index
        to <- floor(index / nstates) + 1        # Ditto
        data.frame(from, to) 
    }), .id='trans')
    
    tmat_withids <- cbind(tmat_long[rep(1:nrow(tmat_long), ninds), ],
                          id=rep(unique_ids, each=ntrans))
    
    # Now join this onto main entry to get possible 'to' states
    entry4 <- entry3 %>%
        dplyr::left_join(tmat_withids, by=c('id', 'from'), suffix=c('.actual', '.possible')) %>%
        dplyr::mutate(status = ifelse(to.actual == to.possible, status, 0)) %>%
        dplyr::select(id, from, to=to.possible, trans, Tstart, Tstop, time, status)   # Clean up
    
    # Add in covariates (just join onto covars by id)
    if (!is.null(covars))
        entry4 <- entry4 %>%
                    dplyr::left_join(covars, by='id')
    
    if (!is.null(censors)) {
        # Two types of people who have useful censor information.
        #   1. Those who never entered any state
        #   2. Those who haven't entered a sink state yet.
        # Can only handle these cases if have their time of last follow up!
        
        # Identify those who never entered any state
        never_entered_state <- start_states %>% 
            dplyr::left_join(entry, by='id') %>% 
            dplyr::filter(is.na(Tstop)) %>% 
            dplyr::select(id, start_state) %>%
            dplyr::left_join(start_times, by='id') %>%
            dplyr::rename(from=start_state, Tstart=start_time)
    
        # And identify those who joined the system but never entered sink state
        last_states <- entry4 %>%
                dplyr::filter(status == 1) %>%
                dplyr::group_by(id) %>%
                dplyr::top_n(1, Tstop) %>%
                dplyr::filter(!to %in% sink_states) %>%
                dplyr::select(id, to, Tstop) %>%
                dplyr::ungroup() %>%
                dplyr::rename(from=to, Tstart=Tstop)
        
        # Then combine them and work out their censored transitions
        censored_trans <- never_entered_state %>%
            rbind(last_states) %>%
            dplyr::left_join(tmat_long, by='from') %>%
            dplyr::left_join(censors, by='id') %>%
            dplyr::mutate(status=0, time=censor_time - Tstart) %>%
            dplyr::select(id, from, to, trans, Tstart, Tstop=censor_time, time, status) %>%
            dplyr::left_join(covars, by='id')
        
        # And finally add these censored observations to existing long DF
        entry4 <- entry4 %>%
                rbind(censored_trans)
    }
    
    entry4 %>%
        dplyr::arrange(id, Tstart, trans)
} 