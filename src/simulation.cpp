#include "simulation.h"
#include "event.h"

Simulation::Simulation(List trans_list, IntegerMatrix trans_mat, NumericMatrix attrs, std::vector<double> times,
                       std::vector<int> start_states, std::vector<int> tcovs):
    clock(0), patient_attributes(attrs) {

    // Create the list of states with their associated transitions
    int nstates, cell;
    List this_trans;
    std::string trans_name;
    NumericMatrix trans_params;

    nstates = trans_mat.nrow();

    // Setup states
    for (int source=0; source < nstates; source++) {
        State nstate = State(source);
        for (int dest=0; dest < nstates; dest++) {
            cell = trans_mat(source, dest);
            if (cell == 0) { // No transition is indicated by 0 in transition matrix
                continue;
            }

            // Have found a transition
            this_trans = as<List>(trans_list[cell-1]);  // Convert back to 0-index
            trans_name  = as<std::string>(this_trans["name"]);

            // Extract transition coefficients and pass appropriate values into Transition factory
            nstate.add_transition(Transition::create_transition(trans_name, dest, as<List>(this_trans["coefs"]), tcovs));
        }
        states.emplace_back(std::move(nstate));
    }

    // Populate event list with initial entries into the system
    int id;
    std::size_t i;
    double initial_time;

    if (times.size() != start_states.size()) {
        // TODO Should raise error
        Rcpp::Rcerr << "Error. starting states does not have the same length as starting times (" << start_states.size() << " and " << times.size() << " respectively).\n";
    }

    for (id=0, i=0; i < times.size(); ++id, ++i) {
        initial_time = times[i];
        add_event(Event(id, start_states[i], initial_time, initial_time));
    }
}

void Simulation::run() {
    while (! event_queue.empty()) {
        Event next_event = event_queue.top();
        event_queue.pop();
        this->clock = next_event.time;
        next_event.processEvent(this);
    }
}

double Simulation::get_next_event_time() {
    return event_queue.top().time;
}


void Simulation::add_event(Event newEvent) {
    event_queue.emplace(std::move(newEvent));
}


State* Simulation::get_state(int index) {
    return &states[index];
}

Rcpp::NumericVector Simulation::get_patient_attrs(int id) {
    return patient_attributes(id, _);
}

void Simulation::add_history(std::tuple<int, int, double> curr_state) {
    history.emplace_back(curr_state);
}

std::vector<std::tuple<int, int, double> > Simulation::get_history() {
    return history;
}

bool CompareTimes::operator() (const Event left, const Event right) const {
    return left.time > right.time;
}
