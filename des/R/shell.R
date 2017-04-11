#run_simulation <- function(params) {
#    ret <- .Call("desCpp", params, package="despackage")
#    return(ret)
#}


# Create dummy values for testing simulation
set.seed(3)
n <- 50000
transitions <- list(list(name='weibull',
                         params = matrix(c(abs(rnorm(n, mean=700)), abs(rnorm(n, mean=10))), nrow=n)
                         ),
                    list(name='lognorm',
                         params = matrix(c(abs(rnorm(n, mean=6)), abs(rnorm(n, mean=0.2))), nrow=n)
                    ),
                    list(name='exp',
                         params=matrix(abs(rnorm(n, mean=0.03, sd=0.0001)), nrow=n)
                    )
)

trans_mat <- mstate::trans.illdeath()
trans_mat[is.na(trans_mat)] <- 0
initial_times <- cumsum(rexp(n, rate=0.01))

#foo <- desCpp(transitions, trans_mat, initial_times)




