#ifndef _SIMULATION_H
#define _SIMULATION_H

#include <queue>

class Event;

struct CompareTimes {
    bool operator() (const Event *left, const Event* right) const;
};

class Simulation {
    public:
        Simulation();
        void scheduleEvent(Event *);
        void run();
        unsigned int time;

    protected:
        std::priority_queue<Event*, std::vector<Event *>, CompareTimes> eventQueue;
};

#endif