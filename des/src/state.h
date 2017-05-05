#ifndef _STATE_H
#define _STATE_H

#include <vector>
#include <utility>
#include "transitions.h"

class State {
    public:
        const int num;
        State(int num);
        ~State(void);
        std::pair<int, float> get_next_transition(int);
        void add_transition(Transition*);
        bool is_transient() const;

    private:
        std::vector<Transition*> outgoing_transitions;
};

#endif