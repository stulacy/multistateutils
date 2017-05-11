#include "event.h"
#include "simulation.h"
#include "state.h"

#include <tuple>
#include <iostream>

// Constructor
Event::Event(int individual_id, int state, double time): individual_id(individual_id), state_entering(state), time(time) {}

void Event::processEvent(Simulation* sim) {
    State* next_state;

    sim->add_history(std::tuple<int, int, double> (individual_id, state_entering, time));

    next_state = sim->get_state(state_entering);
    if (next_state->is_transient()) {
        std::pair<int, double> next_transition = next_state->get_next_transition(individual_id);
        Event *new_event = new Event(individual_id, next_transition.first, next_transition.second + time);  // Convert relative time-to-event to simulation time
        sim->add_event(new_event);
    }
}