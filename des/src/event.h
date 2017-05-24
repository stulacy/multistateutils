#ifndef _EVENT_H
#define _EVENT_H

class Simulation;

class Event {
    public:
        Event(int, int, double, double, double);
        const int state_entering;
        const double time;
        const int individual_id;
        const double sim_entry;
        double prev_state_entry;
        void processEvent(Simulation* sim);
};

#endif