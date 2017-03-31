#ifndef _EVENT_H
#define _EVENT_H

class Simulation;

class Event {
    public:
        Event(int state, float time);
        const int state_entering;
        const float time;
        void processEvent(Simulation* sim);
};

#endif