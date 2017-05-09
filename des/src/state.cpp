#include "state.h"
#include "transitions.h"
#include <vector>
#include <utility>
#include <limits>

// Constructor
State::State(int num): num(num) {};

// Destructor
State::~State(void) {
    for (auto it = outgoing_transitions.begin(); it != outgoing_transitions.end(); ++it){
        delete *it;
    }
    outgoing_transitions.clear();
}

void State::add_transition(Transition* transition) {
    outgoing_transitions.push_back(transition);
    return;
}

bool State::is_transient() const {
    return (outgoing_transitions.size() > 0);
}

std::pair<int, float> State::get_next_transition(int id) {
    if (is_transient()) {
        int winning_state;
        float lowest_event_time;
        float this_event_time;

        lowest_event_time = std::numeric_limits<float>::max();
        winning_state = -1;

        // Iterate over all transitions and obtain the event time
        for (auto it = outgoing_transitions.begin(); it != outgoing_transitions.end(); ++it) {
            this_event_time = std::max(MINIMUM_EVENT_TIME, (*it)->draw_event_time(id));
            if (this_event_time < lowest_event_time) {
                lowest_event_time = this_event_time;
                winning_state = (*it)->to;
            }
        }

        if (winning_state == -1) {
            // TODO Should raise error here
            Rcpp::Rcout << "Error: didn't find next state \n";
        }
        return std::pair<int, float> (winning_state, lowest_event_time);
    } else {
        // TODO This should really raise an error instead
        Rcpp::Rcerr << "Error: Asked to get next transition for a sink state \n";
        return std::pair<int, float> (0, 0);
    }
}