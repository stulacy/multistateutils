#include "event.h"
#include "simulation.h"

// Constructor
Event::Event(int state, float time): state_entering(state), time(time) {}

void Event::processEvent(Simulation* sim) {
    // TODO Fill out

    // Add record of transition (time and state entering) to simulation

    // If entering non-transient state
        // determine next transition
        // Add this to simulation event queue
}

//struct CompareTimes {
//    bool operator() (const Event *left, const Event* right) const {
//        return left-> time > right-> time;
//    }
//};
//
