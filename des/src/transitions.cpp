#include <Rcpp.h>
#include "transitions.h"
using namespace Rcpp;

// Constructor
Transition::Transition(std::string const& name, int to, NumericMatrix params): name(name), to(to), params(params) {};

double Transition::draw_event_time(int id) const {
    return draw(params(id, _));
}

double WeibullTransition::draw(NumericMatrix::ConstRow row) const {
    return as<double>(rweibull(1, row[1], row[0]));
}
double LogNormalTransition::draw(NumericMatrix::ConstRow row) const {
    return as<double>(rlnorm(1, row[0], row[1]));
}
double LogLogisticTransition::draw(NumericMatrix::ConstRow row) const {
    return as<double>(rlnorm(1, row[0], row[1]));
}
double GammaTransition::draw(NumericMatrix::ConstRow row) const {
    return as<double>(rgamma(1, row[1], row[0]));
}
double GompertzTransition::draw(NumericMatrix::ConstRow row) const {
    return as<double>(rlnorm(1, row[1], row[0]));
}
double ExpTransition::draw(NumericMatrix::ConstRow row) const {
    return as<double>(rexp(1, row[0]));
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
