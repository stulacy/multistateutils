#include <Rcpp.h>
#include "transitions.h"
using namespace Rcpp;

// Constructor
Transition::Transition(std::string const& name, int to, NumericMatrix params, double max_time):
    name(name), to(to), params(params), max_time(max_time) {};

double Transition::draw_event_time(int id) const {
    double drawn_time;
    drawn_time = draw(params(id, _));

    //do {
    //    drawn_time = draw(params(id, _));
    //} while (drawn_time > max_time);
    //drawn_time = draw(params(id, _));
    //if (drawn_time > max_time) {
    //    drawn_time = max_time;
    //}

    return drawn_time;
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
Transition *Transition::create_transition(std::string const& dist, int to, NumericMatrix params, double max_time) {
    if (dist == "weibull") {
        return new WeibullTransition(dist, to, params, max_time);
    } else if (dist == "lnorm") {
        return new LogNormalTransition(dist, to, params, max_time);
    } else if (dist == "llogis") {
        return new LogLogisticTransition(dist, to, params, max_time);
    } else if (dist == "gamma") {
        return new GammaTransition(dist, to, params, max_time);
    } else if (dist == "gompertz") {
        return new GompertzTransition(dist, to, params, max_time);
    } else if (dist == "exp") {
        return new ExpTransition(dist, to, params, max_time);
    } else {
        // TODO Should raise error instead
        Rcpp::Rcerr << "Error: Distribution choice '" << dist << "' not found in options. \n";

        return new WeibullTransition(dist, to, params, max_time);
    }

}
