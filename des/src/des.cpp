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
    double max_time;
    NumericMatrix trans_params;

    nstates = transmat.nrow();
    std::vector<State*> state_objects(nstates);
    std::vector<double> init_times = as<std::vector<double> > (initial_times);

    // TODO Put into separate function. Maybe in Simulation constructor?
    int trans_num = 0;
    for (int source=0; source < nstates; source++) {
        // Instantiate state object
        state_objects[source] = new State(source);

        for (int dest=0; dest < nstates; dest++) {
            cell = transmat(source, dest);
            if (cell == 0) { // No transition is indicated by 0 in transition matrix
                continue;
            }

            // Have found a transition
            this_trans = as<List>(transitions[trans_num]);
            ++trans_num;

            trans_name  = as<std::string>(this_trans["name"]);
            trans_params = as<NumericMatrix>(this_trans["params"]);
            max_time = as<double>(this_trans["max"]);

            // Add this transition to the current states available ones
            state_objects[source]->add_transition(Transition::create_transition(trans_name, dest, trans_params, max_time));
        }
    }

    Simulation sim(state_objects, init_times);
    sim.run();

    // Flatten history into vector of tuples, i.e. a matrix
    //auto history = sim.get_history();
    //std::vector< std::tuple<int, int, double> > hist_vec;
    //for(auto const &ent1 : history) {
    //    for(auto const &ent2 : ent1.second) {
    //        hist_vec.push_back(std::tuple<int, int, double>(ent1.first, ent2.first, ent2.second));
    //    }
    //}
//
    //// Then convert into R data structure for returning
    //NumericMatrix hist_mat(hist_vec.size(), 3);
    //for (std::size_t i = 0; i < hist_vec.size(); ++i) {
    //    hist_mat(i, 0) = std::get<0>(hist_vec[i]);
    //    hist_mat(i, 1) = std::get<1>(hist_vec[i]);
    //    hist_mat(i, 2) = std::get<2>(hist_vec[i]);
    //}

    auto history = sim.get_history();
    NumericMatrix hist_mat(history.size(), 3);
    for (std::size_t i = 0; i < history.size(); ++i) {
        hist_mat(i, 0) = std::get<0>(history[i]);
        hist_mat(i, 1) = std::get<1>(history[i]);
        hist_mat(i, 2) = std::get<2>(history[i]);
    }


    if (Rf_isNull(hist_mat)) {
        Rcpp::Rcerr << "Error: null history matrix" << "\n";
    }

    return(hist_mat);
}

