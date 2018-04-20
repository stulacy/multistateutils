#ifndef _STATE_H
#define _STATE_H

#include <vector>
#include <utility>
#include "transitions.h"

// Minimum event time that can be drawn. Rounds up event times if less than this. I've removed it (well, set to 0)
// for now, as this value completely depends on the time-scale. I.e. a value of 1 day makes sense, but the models
// have no idea whether the time-scale is days, or years, or hours.
const double MINIMUM_EVENT_TIME = 0.0;

class State {
    public:
        const int num;
        State(int num);
        std::pair<int, double> get_next_transition(Rcpp::NumericVector, double);
        void add_transition(std::unique_ptr<Transition>);
        bool is_transient() const;

    private:
        std::vector<std::unique_ptr<Transition> > outgoing_transitions;
};

#endif