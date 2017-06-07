#include <Rcpp.h>
#include <limits.h>
#include <utility>

#include "transitions.h"
#include "state.h"
#include "event.h"
#include "simulation.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix desCpp(List transitions, IntegerMatrix transmat, NumericMatrix individual_attributes, NumericVector initial_times) {

    Simulation sim(transitions, transmat, individual_attributes, as<std::vector<double> > (initial_times));
    sim.run();

    // Convert history into Rcpp::NumericMatrix to be returned
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

    return hist_mat;
}

