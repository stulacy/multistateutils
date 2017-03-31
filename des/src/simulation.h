#ifndef _SIMULATION_H
#define _SIMULATION_H

#include <queue>
#include <tuple>
#include <vector>

class Event;
class State;

struct CompareTimes {
    bool operator() (const Event *left, const Event* right) const;
};

class Simulation {
    public:
        Simulation(std::vector<State*>, std::vector<float>);
        void add_event(Event *);
        void add_history(std::tuple<int, int, float>);
        std::vector<std::tuple<int, int, float> > get_history();
        void run();
        unsigned int time;
        State* get_state(int);
        float get_next_event_time();

    private:
        std::priority_queue<Event*, std::vector<Event *>, CompareTimes> event_queue;
        std::vector<std::tuple<int, int, float> > history;
        std::vector<State*> states;
};

#endif