#include "state.h"
#include "transitions.h"
#include <vector>
#include <utility>

// Constructor
State::State(int num): num(num) {};

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

        lowest_event_time = INT_MAX; // TODO Anyway to avoid these defaults? Just to stop compiler warning that may never reach these values
        winning_state = -1;

        // Iterate over all transitions and obtain the event time
        for (std::vector<Transition*>::iterator it = outgoing_transitions.begin(); it != outgoing_transitions.end(); ++it) {
            this_event_time = (*it)->draw_event_time(id);
            std::cout << "Drawn event time: " << this_event_time << "\n";
            if (this_event_time < lowest_event_time) {
                lowest_event_time = this_event_time;
                std::cout << "This is new lowest event time \n";
                winning_state = (*it)->to;
                std::cout << "Set winning state to: " << winning_state << "\n";
            }

        }

        if (winning_state == -1) {
            // TODO Should raise error here
            std::cout << "Error: didn't find next state \n";
        }
        return std::pair<int, float> (winning_state, lowest_event_time);
    } else {
        // TODO This should really raise an error instead
        std::cout << "Error: Asked to get next transition for a sink state \n";
        return std::pair<int, float> (0, 0);
    }
}