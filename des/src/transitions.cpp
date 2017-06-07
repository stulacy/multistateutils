#include <Rcpp.h>
#include "transitions.h"
using namespace Rcpp;

// Constructor
Transition::Transition(std::string const& name, int to, List in_params):
    name(name), to(to)  {
    // TODO Setup a vector of maps for each parameter
    //Rcpp::Rcout << "\nIn Transition constructor";
    //Rcpp::Rcout << "Params has " << in_params.size() << " entries\n";

    // Iterate over params
    for (int i = 0; i < in_params.size(); i++) {
        //Rcpp::Rcout << "Parameter:\t" << i << "\n";
        List param_list = as<List>(in_params[i]);
        //Rcpp::Rcout << "parameter list has " << param_list.size() << " entries\n";
        std::vector<int> indices = as<std::vector<int>>(param_list[0]);
        std::vector<double> coefs = as<std::vector<double>>(param_list[1]);

        //Rcpp::Rcout << "Indices:\t";
        //for (auto it : indices) {
        //    Rcpp::Rcout << it << ",";
        //}
        //Rcpp::Rcout << "\n";
//
        //Rcpp::Rcout << "Coefs:\t";
        //for (auto it : coefs) {
        //    Rcpp::Rcout << it << ",";
        //}
        //Rcpp::Rcout << "\n";

        // Turn 2 vectors into a vector of pairs
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

    // TODO Check that it's alll there
    //Rcpp:Rcout << "\nDisplaying Transition: " << name << "\n";
    //for (auto paramit : params) {
    //    Rcpp::Rcout << "New parameter\n~~~~~~~~~~~\n";
    //    for (auto coefit : paramit ) {
    //        Rcpp::Rcout << "Index: " << coefit.first << "\tCoef: " << coefit.second << "\n";
    //    }
    //}


};

double Transition::draw_event_time(Rcpp::NumericVector attributes, double time_since_start, double sojourn_time) const {
    double drawn_time;

    //Rcpp::Rcout << "\n\nIn draw_event_time with attributes: " << attributes << "\n~~~~~~~~~~~~~~~~~~~~~~~\n";
    std::vector<double> param_values;
    for (auto param_it : params) {
        //Rcpp::Rcout << "\nOn a new parameter: \n";
        double param_val = 0;

        for (auto coef_it: param_it) {
            //Rcpp::Rcout << "Index: " << coef_it.first << "\tCoef: " << coef_it.second;
            double coef_val = attributes[coef_it.first] * coef_it.second;
            //Rcpp::Rcout << "\tParam contrib: " << coef_val << "\n";

            param_val += coef_val;
        }

        //Rcpp::Rcout << "Param val: " << param_val << "\n";

        param_values.emplace_back(param_val);
    }

    // TODO Calculate parameters to be passed into draw function
    drawn_time = draw(param_values, time_since_start, sojourn_time);
    //Rcpp::Rcout << "Drawn time: " << drawn_time << "\n";
    return drawn_time;
}

double WeibullTransition::draw(std::vector<double> row, double time_since_start, double sojourn_time) const {
    return as<double>(rweibull(1, row[1], row[0]));
}
double LogNormalTransition::draw(std::vector<double> row, double time_since_start, double sojourn_time) const {
    return as<double>(rlnorm(1, row[0], row[1]));
}
double LogLogisticTransition::draw(std::vector<double> row, double time_since_start, double sojourn_time) const {
    return as<double>(rlnorm(1, row[0], row[1]));
}
double GammaTransition::draw(std::vector<double> row, double time_since_start, double sojourn_time) const {
    return as<double>(rgamma(1, row[1], row[0]));
}
double GompertzTransition::draw(std::vector<double> row, double time_since_start, double sojourn_time) const {
    return as<double>(rlnorm(1, row[1], row[0]));
}
double ExpTransition::draw(std::vector<double> row, double time_since_start, double sojourn_time) const {
    return as<double>(rexp(1, row[0]));
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
