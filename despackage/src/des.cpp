#include <Rcpp.h>
#include <limits.h>
#include <queue>
using namespace Rcpp;

class Transition {
    public:
        Transition(std::string const& name, int to, NumericMatrix params): name(name), to(to), params(params) {};
        static Transition *create_transition(std::string const& dist, int to, NumericMatrix params);
        float draw_event_time(int id) const;
        virtual float draw(NumericMatrix::ConstRow row) const = 0;

        const std::string name;
        const int to;
        const NumericMatrix params;
};

float Transition::draw_event_time(int id) const {
    return draw(params(id, _));
}

class WeibullTransition: public Transition {
    using Transition::Transition;
    public:
        float draw(NumericMatrix::ConstRow) const;
};

class LogNormalTransition: public Transition {
    using Transition::Transition;
    public:
        float draw(NumericMatrix::ConstRow row) const;
};

class ExpTransition: public Transition {
    using Transition::Transition;
    public:
        float draw(NumericMatrix::ConstRow row) const;
};

float WeibullTransition::draw(NumericMatrix::ConstRow row) const {
    return as<float>(rweibull(1, row[1], row[0]));
}

float LogNormalTransition::draw(NumericMatrix::ConstRow row) const {
    return as<float>(rlnorm(1, row[0], row[1]));
}

float ExpTransition::draw(NumericMatrix::ConstRow row) const {
    return as<float>(rexp(1, row[0]));
}

Transition *Transition::create_transition(std::string const& dist, int to, NumericMatrix params) {
    if (dist == "weibull") {
        return new WeibullTransition(dist, to, params);
    } else if (dist == "lognorm") {
        return new LogNormalTransition(dist, to, params);
    } else if (dist == "exp") {
        return new ExpTransition(dist, to, params);
    } else {
        // TODO Should raise error instead
        std::cout << "Error: Distribution choice '" << dist << "' not found in options. \n";

        return new WeibullTransition(dist, to, params);
    }

}

class State {
    public:
        const int num;
        State(int num): num(num) {};
        std::pair<int, float> get_next_transition(int);
        void add_transition(Transition*);
        bool is_transient() const;

    private:
        std::vector<Transition*> outgoing_transitions;
};

void State::add_transition(Transition* transition) {
    outgoing_transitions.push_back(transition);
    return;
}

bool State::is_transient() const {
    return (outgoing_transitions.size() > 0);
}

std::pair<int, float> State::get_next_transition(int id) {
    if (is_transient()) {
        int winning_state;
        float lowest_event_time;
        float this_event_time;

        lowest_event_time = INT_MAX; // TODO Anyway to avoid these defaults? Just to stop compiler warning that may never reach these values
        winning_state = -1;

        // Iterate over all transitions and obtain the event time
        for (std::vector<Transition*>::iterator it = outgoing_transitions.begin(); it != outgoing_transitions.end(); ++it) {
            this_event_time = (*it)->draw_event_time(id);
            std::cout << "Drawn event time: " << this_event_time << "\n";
            if (this_event_time < lowest_event_time) {
                lowest_event_time = this_event_time;
                std::cout << "This is new lowest event time \n";
                winning_state = (*it)->to;
                std::cout << "Set winning state to: " << winning_state << "\n";
            }

        }

        if (winning_state == -1) {
            // TODO Should raise error here
            std::cout << "Error: didn't find next state \n";
        }
        return std::pair<int, float> (winning_state, lowest_event_time);
    } else {
        // TODO This should really raise an error instead
        std::cout << "Error: Asked to get next transition for a sink state \n";
        return std::pair<int, float> (0, 0);
    }

}

// [[Rcpp::export]]
bool desCpp(List transitions, IntegerMatrix transmat) {
    std::cout << transmat << "\n";

    int nstates;
    int cell;
    List this_trans;
    std::string trans_name;
    NumericMatrix trans_params;

    nstates = transmat.nrow();
    std::vector<State*> state_objects(nstates);


    // TODO Put into separate function
    for (int source=0; source < nstates; source++) {
        // Instantiate new state object
        state_objects[source] = new State(source);

        for (int dest=0; dest < nstates; dest++) {
            cell = transmat(source, dest);
            if (cell == 0) { // No transition is indicated by 0 in transition matrix
                continue;
            }

            // Subset transitions list to obtain this transition
            this_trans = as<List>(transitions[cell-1]); // Remember to convert to 0-index
            trans_name  = as<std::string>(this_trans["name"]);
            trans_params = as<NumericMatrix>(this_trans["params"]);

            // Add this transition to the current states available ones
            state_objects[source]->add_transition(Transition::create_transition(trans_name, dest, trans_params));
        }
    }

    // Quickly summarise data
    for (std::vector<State*>::iterator it = state_objects.begin(); it != state_objects.end(); ++it) {
        std::cout << "\nOn state: " << (*it)->num << " and is transient = : " << (*it)->is_transient() << "\n\n";
        if ((*it)->is_transient()) {
            std::pair<int, float> next_trans = (*it)->get_next_transition(17);
            std::cout << "Next state and time for patient with id 17: " << next_trans.first << "," << next_trans.second << "\n";
        }


    }

    // For each source state create a new
    return(true);
}

class Event {

    public:
        Event(int state, float time): state_entering(state), time(time) {}
        const int state_entering;
        const float time;
        void processEvent(Simulation* sim);



};

class Simulation {
    public:
        Simulation(): time(0), eventQueue() {}

        void run();
        void scheduleEvent(Event * newEvent) {
            eventQueue.push(newEvent);
        }
        unsigned int time;

    protected:
        std::priority_queue<Event*, std::vector<Event *>, CompareTimes> eventQueue;
};



void Event::processEvent(Simulation* sim) {
    // TODO Fill out

    // Add record of transition (time and state entering) to simulation

    // If entering non-transient state
        // determine next transition
        // Add this to simulation event queue
}

struct CompareTimes {
    bool operator() (const Event *left, const Event* right) const {
        return left-> time > right-> time;
    }
};


void Simulation::run() {
    while (! eventQueue.empty()) {
        Event * nextEvent = eventQueue.top();
        eventQueue.pop();
        time = nextEvent->time;
        nextEvent->processEvent();
        delete nextEvent;
    }
}

