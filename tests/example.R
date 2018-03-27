# Create dummy values for testing simulation
set.seed(17)
num <- 10
transitions <- list(list(name='weibull',
                         coefs= list(list(c(1, 3, 4) - 1, # These are the indices in the attributes matrix that these coefficients correspond to, 0-indexed.
                                          c(7.52, 0.53, 0.32) # These are the coefficient values
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


trans_mat <- mstate::trans.illdeath()
trans_mat[is.na(trans_mat)] <- 0              # Ensure that the transition matrix doesn't have NA values, replacing these with 0
initial_times <- cumsum(rexp(num, rate=0.01))  # Specify entry times into the simulation

# This line runs the simulation. It has to be commented out to allow the package to build correctly.
#desCpp(transitions, trans_mat, attributes, initial_times)
