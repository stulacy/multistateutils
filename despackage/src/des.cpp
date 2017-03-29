#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool desCpp(List transitions, IntegerMatrix transmat) {
    std::cout << transmat << "\n";
    
    int nstates;
    int cell;
    List this_trans;
    StringVector trans_name;
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
            this_trans = transitions[cell];
            
            trans_name = this_trans["name"];
            std::cout << "Name: " << trans_name << "\n";
            
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

