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
foo <- predict_transitions(models, newdata, trans_mat, times=365, N=10000)
