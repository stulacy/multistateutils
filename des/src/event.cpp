#include "event.h"
#include "simulation.h"
#include "state.h"

#include <tuple>
#include <iostream>

// Constructor
Event::Event(int individual_id, int state, double time, double sim, double prev_state):
    individual_id(individual_id), state_entering(state), time(time), sim_entry(sim), prev_state_entry(prev_state) {}

void Event::processEvent(Simulation* sim) {
    //sim->add_history(individual_id, std::pair<int, double> (state_entering, time));
    sim->add_history(std::tuple<int, int, double> (individual_id, state_entering, time));

    // TODO Debugging. If it works with this commented out then problem is with accessing states
    State* entering_state = sim->get_state(state_entering);
    if (entering_state->is_transient()) {
        double entry_time = this->time - this->sim_entry;
        double soujourn_time = this->time - this->prev_state_entry;
        std::pair<int, double> next_transition = entering_state->get_next_transition(individual_id, entry_time, soujourn_time);
        // Convert relative time-to-event to simulation time
        Event *new_event = new Event(individual_id, next_transition.first, next_transition.second + this->time, this->sim_entry, this->time);
        sim->add_event(new_event);
    }

}