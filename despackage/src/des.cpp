#include <Rcpp.h>
using namespace Rcpp;

class Transition {
    public:
        static Transition *create_transition(std::string const& dist, int from, int to);
        virtual float draw_event_time(int id);
        Transition(int from, int to): from(from), to(to) {};

        // What data structure is appropriate for params?
    protected:
        int from;
        int to;
        int params;
};

class WeibullTransition: public Transition {
    using Transition::Transition;
    public:
        float draw_event_time(int id) {
        // TODO Obtain params

        //rweibull(1, params);

        return(2.2);
    }
};

class LogNormalTransition: public Transition {
    using Transition::Transition;
    public:
        float draw_event_time(int id) {
        // TODO Obtain params

        //rlognorm(1, params);

        return(2.2);
    }
};



Transition *Transition::create_transition(std::string const& dist, int from, int to) {
    if (dist == "weibull") {
        return new WeibullTransition(from, to);
    } else if (dist == "lognorm") {
        return new LogNormalTransition(from, to);
    } else {
        return new Transition(from, to);
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

    std::cout << "Num states: " << nstates << "\n";
    // Find all transitions in matrix
    for (int i=0; i < nstates; i++) {
        for (int j=0; j < nstates; j++) {
            cell = transmat(i, j);
            if (cell == 0) {
                continue;
            }
            std::cout << cell << "\n";

            // Subset transitions list to obtain this transition
            this_trans = as<List>(transitions[cell-1]); // Remember to convert to 0-index

            trans_name  = as<std::string>(this_trans["name"]);
            std::cout << "Name: " << trans_name << "\n";

            // TODO Instantiate transitions object
            //Transition* trans = Transition::create_transition(trans_name, 5, 3);

        }
    }
    return(true);
}

//class simulation {
//    public:
//        simulation(): time(0), eventQueue() {}
//
//        void run();
//        void scheduleEvent(event * newEvent) {
//            eventQueue.push(newEvent);
//        }
//        unsigned int time;
//
//    protected:
//        std::priority_queue<event*,
//                            std::vector<event *, std::allocator<event*>>,
//                            eventComparator> eventQueue;
//};
//
//void simulation::run() {
//    while (! eventQueue.empty()) {
//        event * nextEvent = eventQueue.top();
//        eventQueue.pop();
//        time = nextEvent->time;
//        nextEvent->processEvent();
//        delete nextEvent;
//    }
//}

