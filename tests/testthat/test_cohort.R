library(testthat)

context("cohort")

library(mstate)
data(ebmt3)
tmat <- trans.illdeath()
long <- msprep(time=c(NA, 'prtime', 'rfstime'), 
               status=c(NA, 'prstat', 'rfsstat'), 
               data=ebmt3, 
               trans=tmat, 
               keep=c('age', 'dissub'))
library(flexsurv)
models <- lapply(1:3, function(i) {
    flexsurvreg(Surv(time, status) ~ age + dissub, data=long, dist='weibull')
})

test_that("cohort_simulation returns expected values", {
    sim <- cohort_simulation(models, ebmt3[, c('age', 'dissub')], tmat)
    expect_equal(ncol(sim), 5)
    expect_equal(nrow(sim) > nrow(ebmt3), TRUE)
    
    # Check have people in all 3 states
    end_states <- unname(table(sim$state))
    expect_equal(length(end_states), 3)
    expect_equal(all(end_states > 0), TRUE)
})

test_that("cohort_simulation guards work", {
    expect_error(cohort_simulation(list(a=5, b=2), ebmt3, tmat))
    expect_error(cohort_simulation(models, ebmt3[, c('age')], tmat))
    expect_error(cohort_simulation(models, ebmt3[, c('age', 'dissub')], tmat,
                                   agelimit=36525, agescale=365, agecol='agecont'))
    expect_error(cohort_simulation(models, ebmt3, tmat[-1, ]))
    expect_error(cohort_simulation(models, ebmt3, tmat[, -1]))
    
})

test_that("cohort_simulation age limit works", {
    sim <- cohort_simulation(models, ebmt3[, c('age', 'dissub')], tmat)
    expect_equal(max(sim$time) > 36525, TRUE)
    
    sim_wagelimit <- cohort_simulation(models, cbind(ebmt3[, c('age', 'dissub')],
                                                     agecont=runif(nrow(ebmt3), 20, 85)), 
                                       tmat,
                                       agelimit=36525, agescale=365.25, agecol = 'agecont')
    expect_equal(max(sim_wagelimit$time) <= 36525, TRUE)
    
})