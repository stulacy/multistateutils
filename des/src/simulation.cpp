#include "simulation.h"
#include "event.h"
#include <queue>

Simulation::Simulation(): time(0), eventQueue() {}

void Simulation::run() {
    while (! eventQueue.empty()) {
        Event * nextEvent = eventQueue.top();
        eventQueue.pop();
        time = nextEvent->time;
        nextEvent->processEvent(this);
        delete nextEvent;
    }
}


void Simulation::scheduleEvent(Event * newEvent) {
    eventQueue.push(newEvent);
}

bool CompareTimes::operator() (const Event *left, const Event* right) const {
    return left-> time > right-> time;
}
