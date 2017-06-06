#include <Rcpp.h>
#include "transitions.h"
using namespace Rcpp;

// Constructor
Transition::Transition(std::string const& name, int to, NumericMatrix params):
    name(name), to(to), params(params) {};

double Transition::draw_event_time(int id, double time_since_start, double sojourn_time) const {
    double drawn_time;
    drawn_time = draw(params(id, _), time_since_start, sojourn_time);
    return drawn_time;
}

double WeibullTransition::draw(NumericMatrix::ConstRow row, double time_since_start, double sojourn_time) const {
    return as<double>(rweibull(1, row[1], row[0]));
}
double LogNormalTransition::draw(NumericMatrix::ConstRow row, double time_since_start, double sojourn_time) const {
    return as<double>(rlnorm(1, row[0], row[1]));
}
double LogLogisticTransition::draw(NumericMatrix::ConstRow row, double time_since_start, double sojourn_time) const {
    return as<double>(rlnorm(1, row[0], row[1]));
}
double GammaTransition::draw(NumericMatrix::ConstRow row, double time_since_start, double sojourn_time) const {
    return as<double>(rgamma(1, row[1], row[0]));
}
double GompertzTransition::draw(NumericMatrix::ConstRow row, double time_since_start, double sojourn_time) const {
    return as<double>(rlnorm(1, row[1], row[0]));
}
double ExpTransition::draw(NumericMatrix::ConstRow row, double time_since_start, double sojourn_time) const {
    return as<double>(rexp(1, row[0]));
}
double OldAgeTransition::draw(NumericMatrix::ConstRow row, double time_since_start, double sojourn_time) const {
    double time_to_death = 100*365.25-(row[0] + time_since_start);
    return time_to_death;
}

// Factory method
std::unique_ptr<Transition> Transition::create_transition(std::string const& dist, int to, NumericMatrix params) {
    if (dist == "weibull") {
        return std::unique_ptr<Transition>(new WeibullTransition(dist, to, params));
    } else if (dist == "lnorm") {
        return std::unique_ptr<Transition>(new LogNormalTransition(dist, to, params));
    } else if (dist == "llogis") {
        return std::unique_ptr<Transition>(new LogLogisticTransition(dist, to, params));
    } else if (dist == "gamma") {
        return std::unique_ptr<Transition>(new GammaTransition(dist, to, params));
    } else if (dist == "gompertz") {
        return std::unique_ptr<Transition>(new GompertzTransition(dist, to, params));
    } else if (dist == "exp") {
        return std::unique_ptr<Transition>(new ExpTransition(dist, to, params));
    } else if (dist == "oldage") {
        return std::unique_ptr<Transition>(new OldAgeTransition(dist, to, params));
    } else {
        // TODO Should raise error instead
        Rcpp::Rcerr << "Error: Distribution choice '" << dist << "' not found in options. \n";
        return std::unique_ptr<Transition>(new WeibullTransition(dist, to, params));
    }

}
