#ifndef _TRANSITIONS_H
#define _TRANSITIONS_H

#include <Rcpp.h>
using namespace Rcpp;

class Transition {
    public:
        Transition(std::string const& name, int to, NumericMatrix params);
        static Transition *create_transition(std::string const& dist, int to, NumericMatrix params);
        float draw_event_time(int id) const;
        virtual float draw(NumericMatrix::ConstRow row) const = 0;

        const std::string name;
        const int to;
        const NumericMatrix params;
};

class WeibullTransition: public Transition {
    using Transition::Transition;
    public:
        float draw(NumericMatrix::ConstRow) const;
};

class LogNormalTransition: public Transition {
    using Transition::Transition;
    public:
        float draw(NumericMatrix::ConstRow row) const;
};

class ExpTransition: public Transition {
    using Transition::Transition;
    public:
        float draw(NumericMatrix::ConstRow row) const;
};

#endif