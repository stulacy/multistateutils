#include <Rcpp.h>
#include <limits.h>
#include <utility>

#include "transitions.h"
#include "state.h"
#include "event.h"
#include "simulation.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix desCpp(List transitions, IntegerMatrix transmat, NumericVector initial_times) {
    int nstates;
    int cell;
    List this_trans;
    std::string trans_name;
    NumericMatrix trans_params;

    nstates = transmat.nrow();
    std::vector<State*> state_objects(nstates);
    std::vector<double> init_times = as<std::vector<double> > (initial_times);

    // TODO Put into separate function. Maybe in Simulation constructor?
    for (int source=0; source < nstates; source++) {
        // Instantiate state object
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

    Simulation sim(state_objects, init_times);
    sim.run();

    std::vector<std::tuple<int, int, double>> history = sim.get_history();
    NumericMatrix hist_mat(history.size(), 3);

    for (std::size_t i = 0; i != history.size(); ++i) {
        hist_mat(i, 0) = std::get<0>(history[i]);
        hist_mat(i, 1) = std::get<1>(history[i]);
        hist_mat(i, 2) = std::get<2>(history[i]);
    }

    if (Rf_isNull(hist_mat)) {
        Rcpp::Rcout << "NULL history matrix!!" << "\n";
    }


    // TODO check if NULL and print in case it is
    return(hist_mat);
}

