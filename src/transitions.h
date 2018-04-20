#ifndef _TRANSITIONS_H
#define _TRANSITIONS_H

#include <Rcpp.h>
#include <memory>
using namespace Rcpp;

class Transition {
    public:
        Transition(std::string const&, int, List, std::vector<int>);
        static std::unique_ptr<Transition> create_transition(std::string const&, int, List, std::vector<int>);
        double draw_event_time(Rcpp::NumericVector, double) const;
        virtual double draw(std::vector<double>, double) const = 0;

        const std::string name;
        const int to;
        const std::vector<int> tcovs;
        std::vector< std::vector< std::pair<int, double> > > params;
};

class WeibullTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(std::vector<double>, double) const;
};

class LogNormalTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(std::vector<double>, double) const;
};

class LogLogisticTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(std::vector<double>, double) const;
};

class GammaTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(std::vector<double>, double) const;
};

class GompertzTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(std::vector<double>, double) const;
};

class ExpTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(std::vector<double>, double) const;
};

class OldAgeTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(std::vector<double>, double) const;
};

#endif