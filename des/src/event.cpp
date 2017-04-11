#include "event.h"
#include "simulation.h"
#include "state.h"

#include <tuple>
#include <iostream>

// Constructor
Event::Event(int individual_id, int state, float time): individual_id(individual_id), state_entering(state), time(time) {}

void Event::processEvent(Simulation* sim) {
    State* next_state;

    // Add record of transition (time and state entering) to simulation
    sim->add_history(std::tuple<int, int, float> (individual_id, state_entering, time));

    // If entering non-transient state
    next_state = sim->get_state(state_entering);
    //std::cout << "Processing individual " << individual_id << " in state " << state_entering << " at time " << time << "\n";

    if (next_state->is_transient()) {
        // determine next transition
        std::pair<int, float> next_transition = next_state->get_next_transition(individual_id);

        // Create new event
        Event *new_event = new Event(individual_id, next_transition.first, next_transition.second + time);  // Convert relative time-to-event to simulation time

        // Add this to simulation event queue
        sim->add_event(new_event);

        //std::cout << "Just added new event with attributes:" << individual_id << ", " << next_transition.first << ", " << next_transition.second << " to the simulation queue \n";
        //std::cout << "Next event time is: " << sim->get_next_event_time() << "\n";
    }
}