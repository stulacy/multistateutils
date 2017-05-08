#run_simulation <- function(params) {
#    ret <- .Call("desCpp", params, package="despackage")
#    return(ret)
#}


# Create dummy values for testing simulation
set.seed(22)
num <- 2204
transitions2 <- list(list(name='weibull',
                         params = matrix(c(abs(rnorm(num, mean=700)), abs(rnorm(num, mean=10))), nrow=num)
                         ),
                    list(name='lnorm',
                         params = matrix(c(abs(rnorm(num, mean=6)), abs(rnorm(num, mean=0.2))), nrow=num)
                    ),
                    list(name='exp',
                         params=matrix(abs(rnorm(num, mean=0.03, sd=0.0001)), nrow=num)
                    )
)

trans_mat2 <- mstate::trans.illdeath()
trans_mat2[is.na(trans_mat2)] <- 0
initial_times2 <- cumsum(rexp(num, rate=0.01))

#foo <- desCpp(transitions2, trans_mat2, initial_times2)

