#' Converts long state entry data into a format suitable for multi-state modelling
#'
#' This function performs the same role as \code{msprep} from the \code{mstate} package, except
#' that it accepts long data (each row corresponds to a state entry) rather
#' than the wide format used by \code{msprep} (each row represents an individual with state entry
#' indicated in columns).
#'
#' The long data format required by \code{msprep2} is a more natural way of organising
#' state entry data than the wide format required by \code{msprep}.
#' An additional benefit of having the state entries organised in this fashion is that
#' it allows for the situation where an individual enters the same state multiple times,
#' which is not supported by \code{msprep}.
#'
#' @param entry Long data frame of format \code{id} | \code{state} | \code{time}.
#'   State can either be character, with the same state names used in \code{tmat}, or
#'   integer where they refer to the rownumber of that state in \code{tmat}.
#'   Note that the \code{state} and \code{time} fields must be labelled this way.
#' @param tmat Transition matrix in the standard format required by \code{msprep}.
#' @param censors A long data frame with 2 columns, \code{id} and \code{censor_time}.
#'   Gives the last follow-up time for individuals that haven't entered a sink state.
#' @param start_times A long data frame with 2 columns, \code{id} and \code{start_time}.
#'   Gives the time at which the patient entered the simulation. Defaults to 0.
#' @param start_states A long data frame with 2 columns, \code{id} and \code{start_state}.
#'   Gives the state the patient entered the simulation in. Defaults to 1.
#' @param covars Data frame where each row corresponds to an individual and details their
#'   covariate values. Must contain the id column specified in \code{idcol} alongside
#'   any covariate fields of interest.
#' @param idcol The column that indexes these patients, must be present in
#'   \code{entry} and \code{censors}, \code{start_times}, \code{start_states},
#'   and \code{covars} if supplied.
#' @examples
#'  library(multistateutils)
#'  library(mstate)
#'
#'  tmat <- trans.illdeath()
#'  entry <- data.frame(id=c(1, 2, 2),
#'                      state=c(2, 2, 3),
#'                      time=c(23, 35, 360))
#'
#'  msprep2(entry, tmat)
#' @export
#' @importFrom magrittr '%>%'
msprep2 <- function(entry, tmat, censors=NULL,
                    start_times=NULL, start_states=NULL, covars=NULL,
                    idcol='id') {

    DEFAULT_START_TIME <- 0
    DEFAULT_START_STATE <- 1

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

    if (!idcol %in% colnames(entry))
        stop(paste0("Error: id field '", idcol, "' not found in entry."))
    if (!'time' %in% colnames(entry))
        stop("Error: column 'time' not found in entry.")
    if (!'state' %in% colnames(entry))
        stop("Error: column 'state' not found in entry.")

    entry <- entry %>%
                dplyr::rename(id = idcol, Tstop=time)
    # Build up list of unique_ids
    unique_ids <- unique(entry$id)
    nstates <- ncol(tmat)
    state_names <- colnames(tmat)

    if (!is.null(censors)) {
        censors <- censors %>%
                    dplyr::rename(id = idcol)
        unique_ids <- union(unique_ids, unique(censors$id))
    }
    if (!is.null(start_times)) {
        if (!idcol %in% colnames(start_times))
            stop(paste0("Error: id field '", idcol, "' not found in start_times."))
        if (!'start_time' %in% colnames(start_times))
            stop(paste0("Error: column start_time not found in start_times."))
        start_times <- start_times %>%
                    dplyr::rename(id = idcol)
        unique_ids <- union(unique_ids, unique(start_times$id))
    }
    if (!is.null(start_states)) {
        if (!idcol %in% colnames(start_states))
            stop(paste0("Error: id field '", idcol, "' not found in start_states."))
        if (!'start_state' %in% colnames(start_states))
            stop(paste0("Error: column start_state not found in start_states."))

        ss <- start_states$start_state
        if (is.factor(ss)) {
            ss <- as.character(ss)
            start_states$start_state <- as.character(start_states$start_state)
        }

        if (!(is.numeric(ss) || is.character(ss)))
            stop("Error: start_state column must be state name or number.")
        if (is.numeric(ss)) {
            if (!all((ss %% 1) == 0))
                stop("Error: start_state column must be state name or number.")
            if (max(ss) > nstates || min(ss) < 1)
                stop("Error: start_state column must be state name or number.")
        }
        if (is.character(ss)) {
            if (!all(ss %in% state_names))
                stop("Error: start_state column must be state name or number.")
        }

        start_states <- start_states %>%
                    dplyr::rename(id = idcol)
        unique_ids <- union(unique_ids, unique(start_states$id))
    }
    if (!is.null(covars)) {
        if (!idcol %in% colnames(covars))
            stop(paste0("Error: id field '", idcol, "' not found in covars."))
        covars <- covars %>%
                    dplyr::rename(id = idcol)
        unique_ids <- union(unique_ids, unique(covars$id))
    }

    if (is.null(start_states))
        start_states <- data.frame(id=unique_ids, start_state=DEFAULT_START_STATE)
    if (is.null(start_times))
        start_times <- data.frame(id=unique_ids, start_time=DEFAULT_START_TIME)

    # Guards
    # Check that every individual has unique time
    has_dups <- entry %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(has_duplicate_times=sum(duplicated(Tstop))>0)
    if (sum(has_dups$has_duplicate_times) > 0) {
        stop("Error: each id in entry must have unique state entry times.")
    }

    # Convert state names to numbers
    if (is.character(entry$state) || is.factor(entry$state))
        entry$state <- match(entry$state, state_names)
    if (is.character(start_states$start_state) || is.factor(start_states$start_state))
        start_states$start_state <- match(start_states$start_state, state_names)

    ntrans <- sum(!is.na(tmat))
    ninds <- length(unique_ids)

    # Now need to add starting state and times.
    # Firstly, obtain the rank order of each state entry
    entry2 <- entry %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(entry_order = dplyr::row_number(Tstop)) %>%
        dplyr::mutate(prev_state = dplyr::lag(state)) %>%
        dplyr::left_join(start_states, by='id') %>%
        dplyr::mutate(start_state = ifelse(is.na(start_state), DEFAULT_START_STATE, start_state),
                      prev_state = ifelse(is.na(prev_state), start_state, prev_state)) %>%
        dplyr::select(-start_state)

    # Likewise, obtain previous times
    entry3 <- entry2 %>%
        dplyr::mutate(Tstart = dplyr::lag(Tstop)) %>%
        dplyr::left_join(start_times, by='id') %>%
        dplyr::mutate(start_time = ifelse(is.na(start_time), DEFAULT_START_TIME, start_time),
                      Tstart = ifelse(is.na(Tstart), start_time, Tstart)) %>%
        dplyr::select(-start_time) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(time = Tstop - Tstart, status=1) %>%
        dplyr::select(id, from=prev_state, to=state, Tstart, Tstop, time, status)

    # Now need to add the transitions that werent made
    # Need DF of all possible ids and transitions
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
            dplyr::mutate(start_time = ifelse(is.na(start_time), DEFAULT_START_TIME, start_time)) %>%
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
            dplyr::select(id, from, to, trans, Tstart, Tstop=censor_time, time, status)

        # And finally add these censored observations to existing long DF
        entry4 <- entry4 %>%
                rbind(censored_trans)
    }

    if (!is.null(covars)) {
        entry4 <- entry4 %>%
                    dplyr::left_join(covars, by='id')
    }

    to_int <- c('id', 'from', 'to', 'trans', 'status')
    entry4[to_int] <- lapply(entry4[to_int], as.integer)

    entry4 %>%
        dplyr::arrange(id, Tstart, trans)
}