#ifndef _EVENT_H
#define _EVENT_H

class Simulation;

class Event {
    public:
        Event(int, int, double, double);
        int individual_id;
        int state_entering;
        double time;
        double sim_entry;
        void processEvent(Simulation* sim);
};

#endif