#include "simulation.h"
#include "event.h"
#include <queue>
#include <tuple>
#include <vector>
#include <iostream>

// TODO Somewhere need to setup states
Simulation::Simulation(std::vector<State*> states, std::vector<float> times): states(states), time(0), event_queue() {
    int id;
    std::vector<float>::iterator it;
    int first_state = 0; // assumption that everyone enters at state 0

    for (id=0, it = times.begin(); it != times.end(); ++it, ++id) {
        add_event(new Event(id, first_state, (*it)));
    }
}

void Simulation::run() {
    while (! event_queue.empty()) {
        Event * next_event = event_queue.top();
        event_queue.pop();
        time = next_event->time;
        //std::cout << "\nCurrent time: " << time << "\n";
        next_event->processEvent(this);
        delete next_event;
    }
}

float Simulation::get_next_event_time() {
    return event_queue.top()->time;
}


void Simulation::add_event(Event * newEvent) {
    event_queue.push(newEvent);
}


State* Simulation::get_state(int index) {
    return states[index];
}

void Simulation::add_history(std::tuple<int, int, float> curr_state) {
    history.push_back(curr_state);
}

std::vector<std::tuple<int, int, float> > Simulation::get_history() {
    return history;
}

bool CompareTimes::operator() (const Event *left, const Event* right) const {
    return left-> time > right-> time;
}
