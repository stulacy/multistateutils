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
float LogLogisticTransition::draw(NumericMatrix::ConstRow row) const {
    return as<float>(rlnorm(1, row[0], row[1]));
}
float GammaTransition::draw(NumericMatrix::ConstRow row) const {
    return as<float>(rgamma(1, row[1], row[0]));
}
float GompertzTransition::draw(NumericMatrix::ConstRow row) const {
    return as<float>(rlnorm(1, row[1], row[0]));
}
float ExpTransition::draw(NumericMatrix::ConstRow row) const {
    return as<float>(rexp(1, row[0]));
}

// Factory method
Transition *Transition::create_transition(std::string const& dist, int to, NumericMatrix params) {
    if (dist == "weibull") {
        return new WeibullTransition(dist, to, params);
    } else if (dist == "lnorm") {
        return new LogNormalTransition(dist, to, params);
    } else if (dist == "llogis") {
        return new LogLogisticTransition(dist, to, params);
    } else if (dist == "gamma") {
        return new GammaTransition(dist, to, params);
    } else if (dist == "gompertz") {
        return new GompertzTransition(dist, to, params);
    } else if (dist == "exp") {
        return new ExpTransition(dist, to, params);
    } else {
        // TODO Should raise error instead
        Rcpp::Rcout << "Error: Distribution choice '" << dist << "' not found in options. \n";

        return new WeibullTransition(dist, to, params);
    }

}
