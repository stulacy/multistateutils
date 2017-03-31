#run_simulation <- function(params) {
#    ret <- .Call("desCpp", params, package="despackage")
#    return(ret)
#}

# Create dummy values for testing simulation
transitions <- list(list(name='weibull',
                         params = matrix(c(abs(rnorm(20)), abs(rnorm(20, mean=5))), nrow=20)
                         ),
                    list(name='lognorm',
                         params = matrix(c(abs(rnorm(20)), abs(rnorm(20, mean=8))), nrow=20)
                    ),
                    list(name='exp',
                         params=matrix(abs(rnorm(20)), nrow=20)
                    )
)

trans_mat <- mstate::trans.illdeath()
trans_mat[is.na(trans_mat)] <- 0

desCpp(transitions, trans_mat)


