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
        std::pair<int, double> get_next_transition(int, double, double);
        void add_transition(std::unique_ptr<Transition>);
        bool is_transient() const;

        // Was getting errors as State was being copied causing unique_ptr to panic as
        // it's used to store outgoing transitions. Fix from this SO post.
        // https://stackoverflow.com/questions/21943569/deleted-function-unique-ptr
        //explicit State();
        //State(const State&) = delete;
        //State& operator=(const State&) = delete;
        //~State() = default;

    private:
        std::vector<std::unique_ptr<Transition> > outgoing_transitions;
};

#endif