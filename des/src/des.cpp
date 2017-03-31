#include <Rcpp.h>
#include <limits.h>
#include "transitions.h"
#include "state.h"
#include "event.h"
#include "simulation.h"
using namespace Rcpp;


// [[Rcpp::export]]
bool desCpp(List transitions, IntegerMatrix transmat) {
    std::cout << transmat << "\n";

    int nstates;
    int cell;
    List this_trans;
    std::string trans_name;
    NumericMatrix trans_params;

    nstates = transmat.nrow();
    std::vector<State*> state_objects(nstates);


    // TODO Put into separate function
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

    // Quickly summarise data
    for (std::vector<State*>::iterator it = state_objects.begin(); it != state_objects.end(); ++it) {
        std::cout << "\nOn state: " << (*it)->num << " and is transient = : " << (*it)->is_transient() << "\n\n";
        if ((*it)->is_transient()) {
            std::pair<int, float> next_trans = (*it)->get_next_transition(17);
            std::cout << "Next state and time for patient with id 17: " << next_trans.first << "," << next_trans.second << "\n";
        }
    }

    return(true);
}

