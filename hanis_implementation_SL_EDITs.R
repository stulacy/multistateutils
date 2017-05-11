
###########################################
#  setting
###########################################


# 01. Global variables

util.init <- 0.7                # initial utility
npats <- 50000                  # number of patients
umult.hfrac <- 0.75
cost.hfrac <- 7000
cost.int <- 500                 # annual cost of intervention

# 02. Set options

dbg <- F
ind <- T 
if (ind==T){
    PatData <- vector("list", length=npats) # empty list with 50000 elements
}

###########################################
#  DES functions
###########################################

# 01. event list for "no intervention group"
InitEventList.noint <- function(){
    hfrac  <- rweibull(n=1, shape=4, scale=10)    # time to hip fracture
    death  <- rnorm(n=1, mean=12, sd=3)           # time to death
    output <- data.frame(
        
        evtname=c(                                # column 1: event name                     
            "hfrac",                        
            "death"
        ),
        
        evttime=c(                                # column 2: event time   
            hfrac,                          
            max(0, rnorm(n=1, mean=12, sd=3))     # Use max function prevents negative value
        )                                           
    )

    if (ind==T){
        this.PatData$noint$evtlist <<- output
    }
    return(output)
}

# 02. event list for "intervention group" (input is output from InitEventList.noint)
InitEventList.int <- function(input){ 
    output <- input
    
    # double the hfrac event time
    thisrow <- which(output$evtname=="hfrac")                
    output$evttime[thisrow] <- 2 * output$evttime[thisrow]  

    if (ind==T){
        this.PatData$int$evtlist <<- output       
    }                                             
    return(output)
}


# 03. generate next event & event time list
GetNxtEvt <- function(intervention=F){                  
    if (intervention==F){                        
        if (dim(evtlist.noint)[1]> 0){              #if nrow>0
            # TODO Why hardcode hipfracture as next event? This data frame isn't 
            # ordered so death could be next
            nextevt <- evtlist.noint$evtname[1]  
            nextevttime <- evtlist.noint$evttime[1] 
            
            if (dbg==T){print(evtlist.noint)}       
            evtlist.noint <<- evtlist.noint[-1,]    
            output <- list(evt=nextevt, evttime=nextevttime)
        } else {output <- NULL}
    } else {    
        if (dim(evtlist.int)[1]> 0){
            # TODO See above. Noticed issue where hfrac time = 16 and death = 8, but 
            # hfrac was chosen as nextevent since first row
            nextevt <- evtlist.int$evtname[1] 
            nextevttime <- evtlist.int$evttime[1] 
            
            if (dbg==T){print(evtlist.int)}         
            evtlist.int <<- evtlist.int[-1,]        
            output <- list(evt=nextevt, evttime=nextevttime)
        } else {
            output <- NULL
        }
    }
    
    return(output)
}


# 04. attach value to next event
ReactEvt <- function(thisevt, intervention){      
    
    evt <- thisevt$evt                  # Event type
    prevtime <- curtime                 # Previous event
    curtime <<- thisevt$evttime         # Time of next event
    
    if (intervention==F){
        if (evt=="death"){
            if(ind==T){this.PatData$noint$death <<- 1}
            
            # TODO Lots of analysis within the simulation. My code would analyse the cost effectiveness at the end,
            # with the simulation itself just providing the patient history
            thsday <<- thsday + curtime
            thsqaly <<- thsqaly + (curtime*utilmlt)             
            curtime <<- Inf             
            
        } else if (evt=="hfrac"){  
            if (ind==T){this.PatData$noint$nhip <<- this.PatData$noint$nhip + 1}
            
            thsday <<- thsday + curtime
            thsqaly <<- thsqaly + (curtime*utilmlt)
            thscost <<- thscost + 7000          
            utilmlt <<- utilmlt * umult.hfrac   
        } 
        
    } else {  
        if (evt=="death"){     
            if (ind==T){this.PatData$int$deathother <<- this.PatData$int$deathother + 1 }
            
            thsday <<- thsday + curtime
            thsqaly <<- thsqaly + (curtime*utilmlt)  
            thscost <<- thscost + curtime*cost.int
            
            curtime <<- Inf 
            
        } else if (evt=="hfrac"){
            if (ind==T){this.PatData$int$nhip <<- this.PatData$int$nhip + 1}
            
            thsday <<- thsday + curtime
            thsqaly <<- thsqaly + (curtime*utilmlt) 
            thscost <<- thscost + curtime*cost.int + 7000
            utilmlt <<- utilmlt * umult.hfrac   
        } 
    }
    return(NULL)    # set to return NULL object, so earlier operations are not returned by accident
}


# 05. Enclose simulation within a function
RunSim <- function(){
    
    tot.day.noint <<- 0 
    tot.day.int <<- 0   
    tot.day <<- 0       
    
    tot.qaly.noint <<- 0 
    tot.qaly.int <<- 0   
    tot.qaly <<- 0          
    
    tot.cost.noint <<- 0 
    tot.cost.int <<- 0   
    tot.cost <<- 0       
    
    # Outer loop, repeat for each patient
    for (i in 1:npats){
        
        if (ind==T){
            this.PatData <<- list(
                int=list(
                    nhip=0,
                    death=0
                ),
                noint=list(
                    nhip=0,
                    death=0
                )
            )
        }
        
        
        # Generate event data.frame - no intervention
        evtlist.noint <<- InitEventList.noint()
        
        # Generate event data.frame - intervention
        evtlist.int <<- InitEventList.int(evtlist.noint)
        
        #=================================
        # For the no intervention patient:
        #=================================
        curtime <<- 0
        utilmlt <<- 0.7     
        thsday <<- 0        # time for each patient
        thsqaly <<- 0       # qaly for each patient
        thscost <<- 0       # cost for each patient
        
        while(curtime < Inf){
            # Get next event, process, repeat
            Evt <- GetNxtEvt(intervention=F)
            
            if (is.null(Evt)==F){
                ReactEvt(Evt, intervention=F)
            } else {curtime <<- Inf} 
            
            if (ind==T){
                this.PatData$noint$thsday <<- thsday
                this.PatData$noint$thsqaly <<- thsqaly
                this.PatData$noint$thscost <<- thscost
            }
        }
        
        
        tot.day.noint <<- tot.day.noint + thsday
        tot.day <<- tot.day - thsday 
        tot.qaly.noint <<- tot.qaly.noint + thsqaly
        tot.qaly <<- tot.day - thsqaly
        tot.cost.noint <<- tot.cost.noint + thscost
        tot.cost <<- tot.day - thscost
        
        #==============================
        # for the intervention patient:
        #==============================
        curtime <<- 0       # reset curtime
        utilmlt <<- 0.7     # initial utility
        thsday <<- 0        # time for each patient
        thsqaly <<- 0       # qaly for each patient
        thscost <<- 0       # cost for each patient
        
        # if the event list has been emptied
        emptylist <<- F
        while(curtime < Inf){
            
            Evt <- GetNxtEvt(intervention=T)
            
            if (is.null(Evt)==F){
                ReactEvt(Evt, intervention=T)
            } else {curtime <<- Inf}
            
            if (ind==T){
                this.PatData$int$thsday <<- thsday
                this.PatData$int$thsqaly <<- thsqaly  
                this.PatData$int$thscost <<- thscost
            }
        } 
        
        if (ind==T){
            PatData[[i]] <<- this.PatData
        }
        
        tot.day.int <<- tot.day.int + thsday
        tot.day <<- tot.day + thsday
        tot.qaly.int <<- tot.qaly.int + thsqaly
        tot.qaly <<- tot.qaly + thsqaly
        tot.cost.int <<- tot.cost.int + thscost
        tot.cost <<- tot.cost + thscost
    }
}

######################################
# run simulation 
######################################

# 01. run the simulation
set.seed(1)                     # or any integer

print(system.time(
    RunSim()                        # run simulation
))

# 02. results
#tot.day.int/npats
#tot.qaly.int/npats
#tot.cost.int/npats
#tot.day.noint/npats
#tot.qaly.noint/npats
#tot.cost.noint/npats
#tot.day/npats
#tot.qaly/npats 
#tot.cost/npats



