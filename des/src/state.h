#ifndef _STATE_H
#define _STATE_H

#include <vector>
#include <utility>
#include "transitions.h"

const double MINIMUM_EVENT_TIME = 1.0;

class State {
    public:
        const int num;
        State(int num);
        std::pair<int, double> get_next_transition(Rcpp::NumericVector, double, double);
        void add_transition(std::unique_ptr<Transition>);
        bool is_transient() const;

    private:
        std::vector<std::unique_ptr<Transition> > outgoing_transitions;
};

#endif