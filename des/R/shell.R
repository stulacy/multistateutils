#run_simulation <- function(params) {
#    ret <- .Call("desCpp", params, package="despackage")
#    return(ret)
#}


# Create dummy values for testing simulation
set.seed(17)
num <- 10
transitions2 <- list(list(name='weibull',
                          coefs= list(list(c(1, 3, 4) - 1,
                                           c(7.52, 0.53, 0.32)
                                          ),
                                      list(c(0),
                                           c(2.3)
                                      )
                              )
                         ),
                    list(name='lnorm',
                         coefs= list(list(c(1, 3, 4) - 1,
                                           c(5.38, 0.72, 0.41)
                                          ),
                                      list(c(0),
                                           c(2.3)
                                    )
                              )
                    ),
                    list(name='exp',
                         coefs= list(list(c(1, 3, 4) - 1,
                                           c(8.91, 0.67, 0.54)
                                          )
                                    )
                    )
)

attributes <- data.frame(intercept=1,
                                sex.M = rbinom(num, 1, 0.5),
                                dis.AML = rbinom(num, 1, 0.8),
                                dis.CLL = 1,
                                age = abs(rnorm(num, 50, 10)))
attributes$dis.CLL = 1 - attributes$dis.AML
attributes <- as.matrix(attributes)


trans_mat2 <- mstate::trans.illdeath()
trans_mat2[is.na(trans_mat2)] <- 0
initial_times2 <- cumsum(rexp(num, rate=0.01))

#desCpp(transitions2, trans_mat2, attributes, initial_times2)

