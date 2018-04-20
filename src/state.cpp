#include "state.h"
#include "transitions.h"
#include <vector>
#include <utility>
#include <limits>

// Constructor
State::State(int num): num(num) {};

void State::add_transition(std::unique_ptr<Transition> transition) {
    outgoing_transitions.emplace_back(std::move(transition));
    return;
}

bool State::is_transient() const {
    return (outgoing_transitions.size() > 0);
}

std::pair<int, double> State::get_next_transition(Rcpp::NumericVector attributes, double time_since_entry) {
    if (is_transient()) {
        int winning_state;
        double lowest_event_time;
        double this_event_time;

        lowest_event_time = std::numeric_limits<double>::max();
        winning_state = -1;

        // Iterate over all transitions and obtain the event time
        for (auto it = outgoing_transitions.begin(); it != outgoing_transitions.end(); ++it) {
            this_event_time = std::max(MINIMUM_EVENT_TIME, (*it)->draw_event_time(attributes, time_since_entry));
            if (this_event_time < lowest_event_time) {
                lowest_event_time = this_event_time;
                winning_state = (*it)->to;
            }
        }

        if (winning_state == -1) {
            // TODO Should raise error here
            Rcpp::Rcerr << "Error: didn't find next state \n";
        }

        return std::pair<int, double> (winning_state, lowest_event_time);
    } else {
        // TODO This should really raise an error instead
        Rcpp::Rcerr << "Error: Asked to get next transition for a sink state \n";
        return std::pair<int, double> (0, 0);
    }
}