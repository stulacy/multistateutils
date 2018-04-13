#include <Rcpp.h>
#include "transitions.h"
using namespace Rcpp;

// Constructor
Transition::Transition(std::string const& name, int to, List in_params):
    name(name), to(to)  {

    // Create a vector of pairs to handle each covariate
    for (int i = 0; i < in_params.size(); i++) {
        List param_list = as<List>(in_params[i]);
        std::vector<int> indices = as<std::vector<int>>(param_list[0]);
        std::vector<double> coefs = as<std::vector<double>>(param_list[1]);

        std::vector<std::pair<int, double>> target;
        target.reserve(indices.size());

        std::transform(indices.begin(), indices.end(),
                       coefs.begin(),
                       std::back_inserter(target),
                       [](int a, double b) {
                           return std::make_pair(a, b);
                       });

        params.push_back(target);
    }

};

double Transition::draw_event_time(Rcpp::NumericVector attributes, double time_since_start, double sojourn_time) const {
    double drawn_time;

    std::vector<double> param_values;
    for (auto param_it : params) {
        param_values.emplace_back(std::accumulate(param_it.begin(), param_it.end(), 0.0,
                                                  [&attributes](double curr_sum, std::pair<int, double> const& b)->double {
                                                        return curr_sum + attributes[b.first] * b.second;
                                           }));
    }
    drawn_time = draw(param_values, time_since_start, sojourn_time);

    return drawn_time;
}

double WeibullTransition::draw(std::vector<double> row, double time_since_start, double sojourn_time) const {
    return as<double>(rweibull(1, std::exp(row[1]), std::exp(row[0])));
}
double LogNormalTransition::draw(std::vector<double> row, double time_since_start, double sojourn_time) const {
    return as<double>(rlnorm(1, row[0], std::exp(row[1])));
}
double LogLogisticTransition::draw(std::vector<double> row, double time_since_start, double sojourn_time) const {
    return as<double>(rlnorm(1, std::exp(row[0]), std::exp(row[1])));
}
double GammaTransition::draw(std::vector<double> row, double time_since_start, double sojourn_time) const {
    return as<double>(rgamma(1, std::exp(row[1]), std::exp(row[0])));
}
double GompertzTransition::draw(std::vector<double> row, double time_since_start, double sojourn_time) const {
    return as<double>(rlnorm(1, std::exp(row[1]), row[0]));
}
double ExpTransition::draw(std::vector<double> row, double time_since_start, double sojourn_time) const {
    return as<double>(rexp(1, std::exp(row[0])));
}
double OldAgeTransition::draw(std::vector<double> row, double time_since_start, double sojourn_time) const {
    double time_to_death = 36525-(row[0] + time_since_start);
    return time_to_death;
}

// Factory method
std::unique_ptr<Transition> Transition::create_transition(std::string const& dist, int to, List params) {
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
