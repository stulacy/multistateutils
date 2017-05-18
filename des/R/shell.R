#run_simulation <- function(params) {
#    ret <- .Call("desCpp", params, package="despackage")
#    return(ret)
#}


# Create dummy values for testing simulation
set.seed(17)
num <- 10
transitions2 <- list(list(name='weibull',
                          #params = matrix(c(abs(rnorm(num, mean=700)), abs(rnorm(num, mean=10))), nrow=num),
                          params = matrix(c(700, 10), nrow=num, ncol=2, byrow=T),
                          max=500
                         ),
                    list(name='lnorm',
                         #params = matrix(c(abs(rnorm(num, mean=6)), abs(rnorm(num, mean=0.2))), nrow=num),
                         params = matrix(c(6, 0.2), nrow=num, ncol=2, byrow=T),
                         max=600
                    ),
                    list(name='exp',
                         #params=matrix(abs(rnorm(num, mean=0.03, sd=0.0001)), nrow=num),
                         params = matrix(c(0.03), nrow=num),
                         max=300
                    )
)

trans_mat2 <- mstate::trans.illdeath()
trans_mat2[is.na(trans_mat2)] <- 0
initial_times2 <- cumsum(rexp(num, rate=0.01))

#foo <- desCpp(transitions2, trans_mat2, initial_times2)

