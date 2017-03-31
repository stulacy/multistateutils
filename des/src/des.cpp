#include <Rcpp.h>
#include <limits.h>
#include <utility>

#include "transitions.h"
#include "state.h"
#include "event.h"
#include "simulation.h"

using namespace Rcpp;

// [[Rcpp::export]]
bool desCpp(List transitions, IntegerMatrix transmat, NumericVector initial_times) {
    std::cout << transmat << "\n";

    int nstates;
    int cell;
    List this_trans;
    std::string trans_name;
    NumericMatrix trans_params;
    Simulation* sim;

    nstates = transmat.nrow();
    std::vector<State*> state_objects(nstates);
    std::vector<float> init_times = as<std::vector<float> > (initial_times);



    // TODO Put into separate function. Maybe in Simulation constructor?
    for (int source=0; source < nstates; source++) {
        // Instantiate new state object
        state_objects[source] = new State(source);

        for (int dest=0; dest < nstates; dest++) {
            cell = transmat(source, dest);
            if (cell == 0) { // No transition is indicated by 0 in transition matrix
                continue;
            }

            // Subset transitions list to obtain this transition
            this_trans = as<List>(transitions[cell-1]); // Remember to convert to 0-index
            trans_name  = as<std::string>(this_trans["name"]);
            trans_params = as<NumericMatrix>(this_trans["params"]);

            // Add this transition to the current states available ones
            state_objects[source]->add_transition(Transition::create_transition(trans_name, dest, trans_params));
        }
    }

    sim = new Simulation(state_objects, init_times);
    sim->run();

    // Display summary
    std::vector<std::tuple<int, int, float>>::iterator it;
    std::vector<std::tuple<int, int, float>> history = sim->get_history();

    std::cout << "id\tstate\ttime\n";
    for (it = history.begin(); it != history.end(); ++it) {
        std::cout << std::get<0>(*it) << "\t" << std::get<1>(*it) << "\t" << std::get<2>(*it) << "\n";

    }

    return(true);
}

