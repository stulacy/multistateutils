#ifndef _SIMULATION_H
#define _SIMULATION_H

#include <queue>
#include <tuple>
#include <vector>
#include <iostream>
#include "state.h"

class Event;

struct CompareTimes {
    bool operator() (const Event *left, const Event* right) const;
};

class Simulation {
    public:
        Simulation(List, IntegerMatrix, std::vector<double>);
        ~Simulation(void);
        void add_event(Event *);
        void add_history(std::tuple<int, int, double>);
        std::vector < std::tuple<int, int, double> > get_history();
        double get_sim_entry_time(int);
        double get_previous_state_entry_time(int);
        void run();
        unsigned int clock;
        State* get_state(int);
        double get_next_event_time();

    private:
        std::priority_queue<Event*, std::vector<Event *>, CompareTimes> event_queue;
        std::vector < std::tuple<int, int, double> > history;
        std::vector<State*> states;
};

#endif