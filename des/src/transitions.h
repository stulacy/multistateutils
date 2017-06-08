#ifndef _TRANSITIONS_H
#define _TRANSITIONS_H

#include <Rcpp.h>
#include <memory>
using namespace Rcpp;

class Transition {
    public:
        Transition(std::string const&, int, NumericMatrix);
        static std::unique_ptr<Transition> create_transition(std::string const&, int, NumericMatrix);
        double draw_event_time(int, double, double) const;
        virtual double draw(NumericMatrix::ConstRow, double, double) const = 0;

        const std::string name;
        const int to;
        const NumericMatrix params;
};

class WeibullTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(NumericMatrix::ConstRow, double, double) const;
};

class LogNormalTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(NumericMatrix::ConstRow row, double, double) const;
};

class LogLogisticTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(NumericMatrix::ConstRow row, double, double) const;
};

class GammaTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(NumericMatrix::ConstRow row, double, double) const;
};

class GompertzTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(NumericMatrix::ConstRow row, double, double) const;
};

class ExpTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(NumericMatrix::ConstRow row, double, double) const;
};

class OldAgeTransition: public Transition {
    using Transition::Transition;
    public:
        double draw(NumericMatrix::ConstRow row, double, double) const;
};

#endif