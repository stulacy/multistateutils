#ifndef _EVENT_H
#define _EVENT_H

class Simulation;

class Event {
    public:
        Event(int individual_id, int state, double time);
        const int state_entering;
        const double time;
        const int individual_id;
        void processEvent(Simulation* sim);
};

#endif