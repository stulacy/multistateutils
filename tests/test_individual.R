# Create dummy values for testing simulation
set.seed(17)

library(flexsurv)
library(mstate)
data(ebmt3)
trans_mat <- trans.illdeath(c("diagnosis", "pr", "rfs"))
df_long <- msprep(time=c(NA, 'prtime', 'rfstime'),
                  status=c(NA, 'prstat', 'rfsstat'),
                  ebmt3,
                  trans_mat,
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

newdata = data.frame(sex=factor('M', levels=c('F', 'M')),
                     age=factor('20-40', levels=c('<=20', '20-40', '>40')),
                     dissub=factor('CML', levels=c('AML', 'ALL', 'CML')))


trans_mat <- mstate::trans.illdeath()

# This line runs the simulation. It has to be commented out to allow the package to build correctly.
foo <- predict_transitions(models, newdata, trans_mat, times=seq(3)*365.25, N=10000)

# Let's get pmatrix.simfs for comparison. Note how need to run this separately for each time point!
pmatrix.simfs(models, trans_mat, t=365, newdata=newdata)
pmatrix.simfs(models, trans_mat, t=730, newdata=newdata)
pmatrix.simfs(models, trans_mat, t=1095, newdata=newdata)


newdata = data.frame(sex=factor(c('M', 'F'), levels=c('F', 'M')),
                     age=factor(c('20-40', '20-40'), levels=c('<=20', '20-40', '>40')),
                     dissub=factor(c('CML', 'AML'), levels=c('AML', 'ALL', 'CML')))


bar <- predict_transitions(models, newdata, trans_mat, times=seq(3)*365.25, N=10000)
bar

# NB: pmatrix.simfs doesn't like having multiple lines for newdata
pmatrix.simfs(models, trans_mat, t=365, newdata=newdata)
pmatrix.simfs(models, trans_mat, t=730, newdata=newdata)
pmatrix.simfs(models, trans_mat, t=1095, newdata=newdata)

# Finally note how we can change the conditional time,
# while pmatrix.simfs doesn't let you change it from zero.
car <- predict_transitions(models, newdata, trans_mat, 
                           start_time = 100,
                           times=seq(3)*365.25, N=10000)
car

