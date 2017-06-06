#ifndef _EVENT_H
#define _EVENT_H

class Simulation;

class Event {
    public:
        Event(int, int, double, double, double);
        int state_entering;
        double time;
        int individual_id;
        double sim_entry;
        double prev_state_entry;
        void processEvent(Simulation* sim);
};

#endif