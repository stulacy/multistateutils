# Create dummy values for testing simulation
set.seed(17)

library(flexsurv)
library(mstate)
data(ebmt3)
tmat <- trans.illdeath(c("diagnosis", "pr", "rfs"))

df_long <- msprep(time=c(NA, 'prtime', 'rfstime'),
                  status=c(NA, 'prstat', 'rfsstat'),
                  ebmt3,
                  tmat,
                  keep=c('age', 'dissub'))

# Create models
models <- list()
for (tr in 1:3) {

    if (tr == 1) {
        models[[tr]] <- flexsurvreg(Surv(time, status) ~ age,
                                    subset=(trans==tr),
                                    data=df_long,
                                    dist='weibull')
    } else if (tr == 2) {
        models[[tr]] <- flexsurvreg(Surv(time, status) ~ age + dissub,
                                    subset=(trans==tr),
                                    data=df_long,
                                    dist='weibull')

    } else if (tr == 3) {
        models[[tr]] <- flexsurvreg(Surv(time, status) ~ age + dissub,
                                    subset=(trans==tr),
                                    data=df_long,
                                    dist='weibull')
    }
}

ndata <- data.frame(sex=factor('M', levels=c('F', 'M')),
                     age=factor('20-40', levels=c('<=20', '20-40', '>40')),
                     dissub=factor('CML', levels=c('AML', 'ALL', 'CML')))


tmat <- mstate::trans.illdeath()

# This line runs the simulation. It has to be commented out to allow the package to build correctly.
foo <- predict_transitions(models, ndata, tmat, times=seq(3)*365.25, start_times=0)
foo

# Let's get pmatrix.simfs for comparison. Note how need to run this separately for each time point!
do.call('rbind', lapply(seq(3) * 365.25, function(t) {
    pmatrix.simfs(models, tmat, t=t, newdata=ndata)
}))


##### Predicting for multiple individuals at once
ndata <- data.frame(sex=factor(c('M', 'F'), levels=c('F', 'M')),
                     age=factor(c('20-40', '20-40'), levels=c('<=20', '20-40', '>40')),
                     dissub=factor(c('CML', 'AML'), levels=c('AML', 'ALL', 'CML')))


bar <- predict_transitions(models, ndata, tmat, times=seq(3)*365.25, N=10000)
bar

# NB: pmatrix.simfs doesn't like having multiple lines for newdata
do.call('rbind', lapply(seq(3) * 365.25, function(t) {
    pmatrix.simfs(models, tmat, t=t, newdata=ndata)
}))

##### Starting (conditional) time
# Finally note how we can change the conditional time,
# while pmatrix.simfs doesn't let you change it from zero.
car <- predict_transitions(models, ndata, tmat,
                           start_time = 100,
                           times=seq(3)*365.25, N=10000)
car


##### Testing time-dependent covariates
# Make a continuous value for age so can fit a model with age at state entry
num_gt_40 <- sum(ebmt3$age == '>40')
num_20_40 <- sum(ebmt3$age == '20-40')
num_lt_20 <- sum(ebmt3$age == '<=20')
ebmt3$age_cont <- 50.0
ebmt3$age_cont[ebmt3$age == '>40'] <- runif(num_gt_40, 40, 90)
ebmt3$age_cont[ebmt3$age == '20-40'] <- runif(num_20_40, 20, 40)
ebmt3$age_cont[ebmt3$age == '<=20'] <- runif(num_lt_20, 0, 20)

df_long_2 <- msprep(time=c(NA, 'prtime', 'rfstime'),
                    status=c(NA, 'prstat', 'rfsstat'),
                    ebmt3,
                    tmat,
                    keep=c('age_cont', 'dissub'))
# Convert time to be in years so can easily add age on
df_long_2$time_yrs <- df_long_2$time / 365.25
df_long_2$Tstart_yrs <- df_long_2$Tstart / 365.25

# Then create age at state entry column
df_long_2$age_entry <- df_long_2$age_cont + df_long_2$Tstart_yrs

# Create models
models_entry <- list()
for (tr in 1:3) {

    if (tr == 1) {
        models_entry[[tr]] <- flexsurvreg(Surv(time_yrs, status) ~ age_entry,
                                    subset=(trans==tr),
                                    data=df_long_2,
                                    dist='weibull')
    } else if (tr == 2) {
        models_entry[[tr]] <- flexsurvreg(Surv(time_yrs, status) ~ age_entry + dissub,
                                    subset=(trans==tr),
                                    data=df_long_2,
                                    dist='weibull')

    } else if (tr == 3) {
        # For the illness -> death transition use the state arrival time as well, which
        # is conveniently already held in Tstart
        models_entry[[tr]] <- flexsurvreg(Surv(time_yrs, status) ~ age_entry + dissub + Tstart_yrs,
                                    subset=(trans==tr),
                                    data=df_long_2,
                                    dist='weibull')
    }
}

# Notice how we want the age_entry and Tstart values to increment by the current clock time
# when a new state is entered. This is only applicable for intermediate states; 'pr' is the only
# such one in this dataset. Thus the transition from pr -> death has age increased by amount of
# time since diagnosis, and this information is also held in the 'Tstart' variable, which
# holds the amount of time since diagnosis
newdata_entry <- data.frame(sex=factor('M', levels=c('F', 'M')),
                      age_entry=50,
                      dissub=factor('CML', levels=c('AML', 'ALL', 'CML')),
                      Tstart_yrs = 0)

# I've had to add in the same 'tcovs' argument as pmatrix.simfs for this to work in RDES
predict_transitions(models_entry, newdata_entry, tmat, times=c(1, 3, 5), tcovs=c('age_entry', 'Tstart_yrs'))

# NB: Note how the pmatrix.simfs function doesn't take the tcovs into account at all!
# This looks to be an oversight on the part of Chris Jackson. As a result, it overestimates the risk of dying,
# whereas my method knows that a person who has survived longer since diagnosis has a reduced risk of dying
do.call('rbind', lapply(c(1, 3, 5), function(t) {
    pmatrix.simfs(models_entry, tmat, t=t, newdata=newdata_entry, tcovs = c('age_entry', 'Tstart_yrs'))
}))

###### Can we use this with different parametric distributions on each transition


###### Bootstrapping
ndata <- data.frame(sex=factor('M', levels=c('F', 'M')),
                     age=factor('20-40', levels=c('<=20', '20-40', '>40')),
                     dissub=factor('CML', levels=c('AML', 'ALL', 'CML')))

tmat <- mstate::trans.illdeath()


# This time is pretty close to 100x the time for one person, but let's see if we can improve it.
system.time(predict_transitions(models, ndata, tmat, times=seq(3)*365.25, start_times=0))
system.time(predict_transitions(models, ndata, tmat, times=seq(3)*365.25, start_times=0, ci=TRUE, M=100))
