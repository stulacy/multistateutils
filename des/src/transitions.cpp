#include <Rcpp.h>
#include "transitions.h"
using namespace Rcpp;

// Constructor
Transition::Transition(std::string const& name, int to, NumericMatrix params): name(name), to(to), params(params) {};

float Transition::draw_event_time(int id) const {
    return draw(params(id, _));
}

float WeibullTransition::draw(NumericMatrix::ConstRow row) const {
    return as<float>(rweibull(1, row[1], row[0]));
}

float LogNormalTransition::draw(NumericMatrix::ConstRow row) const {
    return as<float>(rlnorm(1, row[0], row[1]));
}

float ExpTransition::draw(NumericMatrix::ConstRow row) const {
    return as<float>(rexp(1, row[0]));
}

// Factory method
Transition *Transition::create_transition(std::string const& dist, int to, NumericMatrix params) {
    if (dist == "weibull") {
        return new WeibullTransition(dist, to, params);
    } else if (dist == "lognorm") {
        return new LogNormalTransition(dist, to, params);
    } else if (dist == "exp") {
        return new ExpTransition(dist, to, params);
    } else {
        // TODO Should raise error instead
        std::cout << "Error: Distribution choice '" << dist << "' not found in options. \n";

        return new WeibullTransition(dist, to, params);
    }

}
