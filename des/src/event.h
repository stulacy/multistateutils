#ifndef _EVENT_H
#define _EVENT_H

class Simulation;

class Event {
    public:
        Event(int individual_id, int state, float time);
        const int state_entering;
        const float time;
        const int individual_id;
        void processEvent(Simulation* sim);
};

#endif