
<!-- README.md is generated from README.Rmd. Please edit that file -->
rdes
====

`rdes` provides an extension of the simulation options provided in the R package `flexsurv` for obtaining predicted outcomes from multi-state models with parametric transition hazards. It is designed to be used with semi-Markov multi-state models of healthcare data, but can be used for any system that can be discretised into the states being entered at observed time-points, with parametric families providing appropriate fits for these transition rates. It currently only has the ability for *estimating transition probabilities*, but additional features are in the pipeline, including:

-   Estimate length of stay
-   Provide full cohort simulation

It provides several benefits over the implementation in `flexsurv`:

-   Quicker to run
-   Can estimate multiple end times at once
-   The start time can be changed
-   Can estimate probabilities for multiple individuals at once
-   Allows the use of time-dependent covariates
-   Can handle a mixture of distributions

Examples of these features are provided below in *Examples*.

Installation
------------

You can install rdes from github with:

``` r
install.packages("devtools")  # install devtools if it isn't already
devtools::install_github("stulacy/RDES")
```

### Windows

Note that since the simulation engine is written in C++, Windows users will need to have `Rtools` installed, which can be obtained from <https://cran.r-project.org/bin/windows/Rtools/>, and then set to the right path with:

``` r
devtools::find_rtools()
```

Once `Rtools` is setup run `devtools::install_github("stulacy/RDES")` as above.

Examples
--------

This section will demonstrate how to use `rdes` for estimating transition probabilities from a multi-state model, and the additional features that it provides over using `flexsurv::pmatrix.simfs`.

### Setup

This guide assumes familiarity with multi-state modelling in R, this section in particular glosses over the details and just prepares models and data in order to demonstrate the features of `rdes`. If you are unfamiliar with multi-state modelling then I would recommend reading de Wreede, Fiocco, and Putter (2011) or the [`mstate` tutorial by Putter](https://cran.r-project.org/web/packages/mstate/vignettes/Tutorial.pdf).

For these examples the `ebmt3` data set from `mstate` will be used. This provides a simple illness-death model of patients following transplant. The initial state is patient having received transplantation, *pr* referring to platelet recovery (the 'illness'), with relapse-free-survival (*rfs*) being the only sink state.

``` r
library(mstate)
#> Loading required package: survival
data(ebmt3)
head(ebmt3)
#>   id prtime prstat rfstime rfsstat dissub   age            drmatch    tcd
#> 1  1     23      1     744       0    CML   >40    Gender mismatch No TCD
#> 2  2     35      1     360       1    CML   >40 No gender mismatch No TCD
#> 3  3     26      1     135       1    CML   >40 No gender mismatch No TCD
#> 4  4     22      1     995       0    AML 20-40 No gender mismatch No TCD
#> 5  5     29      1     422       1    AML 20-40 No gender mismatch No TCD
#> 6  6     38      1     119       1    ALL   >40 No gender mismatch No TCD
```

`mstate` provides a host of utility functions for working with multi-state models. For example, the `trans.illdeath()` function provides the required transition matrix for this state structure (`transMat` should be used when greater flexibility is required).

``` r
tmat <- trans.illdeath()
tmat
#>          to
#> from      healthy illness death
#>   healthy      NA       1     2
#>   illness      NA      NA     3
#>   death        NA      NA    NA
```

The final data preparation step is to form the data from a wide format (each row corresponding to a patient) to a long format, where each row represents a potential patient-transition. The `msprep` function from `mstate` handles this for us. We'll keep both the `age` and `dissub` covariates in this example.

``` r
long <- msprep(time=c(NA, 'prtime', 'rfstime'), 
               status=c(NA, 'prstat', 'rfsstat'), 
               data=ebmt3, 
               trans=tmat, 
               keep=c('age', 'dissub'))
head(long)
#> An object of class 'msdata'
#> 
#> Data:
#>   id from to trans Tstart Tstop time status age dissub
#> 1  1    1  2     1      0    23   23      1 >40    CML
#> 2  1    1  3     2      0    23   23      0 >40    CML
#> 3  1    2  3     3     23   744  721      0 >40    CML
#> 4  2    1  2     1      0    35   35      1 >40    CML
#> 5  2    1  3     2      0    35   35      0 >40    CML
#> 6  2    2  3     3     35   360  325      1 >40    CML
```

Clock-reset Weibull models will be fitted to these 3 transitions, which are semi-Markov models. Simulation is therefore needed to obtain transition probabilities as the Kolmogorov forward differential equation is no longer valid with the violation of the Markov assumption. We are going to assume that the baseline hazard isn't proportional between transitions and there are no shared transition effects for simplicity's sake.

``` r
library(flexsurv)
models <- lapply(1:3, function(i) {
    flexsurvreg(Surv(time, status) ~ age + dissub, data=long, dist='weibull')
})
```

### Estimating transition probabilities

Transition probabilities are defined as the probability of being in a state *j* at a time *t*, given being in state *h* at time *s*, as shown below where *X*(*t*) gives the state an individual is in at *t*. This is all conditional on the individual parameterised by their covariates and history, which for this semi-Markov model only influences transition probabilities through state arrival times.

*P*<sub>*h*, *j*</sub>(*s*, *t*)=Pr(*X*(*t*)=*j* | *X*(*s*)=*h*)

We'll estimate the transition probabilities of an individual with the covariates `age=20-40` and `dissub=AML` at 1 year after transplant.

``` r
newdata <- data.frame(age="20-40", dissub="AML")
```

The function that estimates transition probabilities is called `predict_transitions` and has a very similar interface to `flexsurv::pmatrix.simfs`. The parameters in the above equation have the following argument names:

-   *t* - `times` (must be supplied)
-   *s* - `start_times` (defaults to 0)
-   *h* - not specified as the probabilities are calculated for all states
-   *j* - not specified as the probabilities are calculated for all states

The code example below shows how to calculate transition probabilities for *t* = 365 (1 year) with *s* = 0; the transition probabilities for every state at 1 year after transplant given being in every state at transplant time. As with `pmatrix.simfs`, although all the probabilities for every pairwise combination of states are calculated, they are sometimes redundant. For example, *P*<sub>*h*, *j*</sub>(0, 365) where *h* = *j* = death is hardly a useful prediction.

``` r
library(rdes)
predict_transitions(models, newdata, tmat, times=365)
#>     age dissub start_time end_time start_state   healthy   illness
#> 1 20-40    AML          0      365     healthy 0.4689727 0.1960009
#> 2 20-40    AML          0      365     illness 0.0000000 0.6860803
#> 3 20-40    AML          0      365       death 0.0000000 0.0000000
#>       death
#> 1 0.3350264
#> 2 0.3139197
#> 3 1.0000000
```

Note that this gives very similar responses to `pmatrix.simfs`.

``` r
pmatrix.simfs(models, tmat, newdata=newdata, t=365)
#>         [,1]    [,2]    [,3]
#> [1,] 0.47134 0.19118 0.33748
#> [2,] 0.00000 0.68671 0.31329
#> [3,] 0.00000 0.00000 1.00000
```

Confidence intervals can be constructed in the same fashion as `pmatrix.simfs`, using draws from the multivariate-normal distribution of the parameter estimates.

``` r
predict_transitions(models, newdata, tmat, times=365, ci=TRUE, M=10)
#>     age dissub start_time end_time start_state healthy_est illness_est
#> 1 20-40    AML          0      365     healthy   0.4686388   0.1911846
#> 2 20-40    AML          0      365     illness   0.0000000   0.6842958
#> 3 20-40    AML          0      365       death   0.0000000   0.0000000
#>   death_est healthy_2.5% illness_2.5% death_2.5% healthy_97.5%
#> 1 0.3401765    0.4466409    0.1815267  0.3255840     0.4852701
#> 2 0.3157042    0.0000000    0.6730746  0.3019661     0.0000000
#> 3 1.0000000    0.0000000    0.0000000  1.0000000     0.0000000
#>   illness_97.5% death_97.5%
#> 1     0.1986574   0.3578461
#> 2     0.6980339   0.3269254
#> 3     0.0000000   1.0000000
```

Which gives rather different results to those obtained from `pmatrix.simfs` which seem to be too wide and the estimate value is far different to that obtained when run without CIs. I'm unsure why this is the case.

``` r
pmatrix.simfs(models, tmat, newdata=newdata, t=365, ci=TRUE, M=9)
#>           [,1]      [,2]      [,3]
#> [1,] 0.5555556 0.2222222 0.2222222
#> [2,] 0.0000000 0.7777778 0.2222222
#> [3,] 0.0000000 0.0000000 1.0000000
#> attr(,"lower")
#>           [,1]      [,2] [,3]
#> [1,] 0.2222222 0.0000000    0
#> [2,] 0.0000000 0.3333333    0
#> [3,] 0.0000000 0.0000000    1
#> attr(,"upper")
#>           [,1]      [,2]      [,3]
#> [1,] 0.7777778 0.4444444 0.6666667
#> [2,] 0.0000000 1.0000000 0.6666667
#> [3,] 0.0000000 0.0000000 1.0000000
#> attr(,"class")
#> [1] "fs.msm.est"
```

Note that on a single individual the speed-up isn't present, with `rdes` taking 4 times longer than `flexsurv`, although the difference between 1.2s and 0.3s isn't that noticeable in interactive work. The main benefit of `rdes` comes when estimating more involved probabilities, as will be demonstrated next.

``` r
library(microbenchmark)
microbenchmark("rdes"=predict_transitions(models, newdata, tmat, times=365),
               "flexsurv"=pmatrix.simfs(models, tmat, newdata=newdata, t=365), times=10)
#> Unit: milliseconds
#>      expr       min        lq      mean    median        uq       max
#>      rdes 1150.5473 1222.6471 1373.3478 1362.6924 1529.8049 1667.6759
#>  flexsurv  287.2617  303.9191  325.4198  323.5685  338.1202  380.6438
#>  neval cld
#>     10   b
#>     10  a
```

### Estimating probabilities at multiple times

Frequently, it is desirable to estimate transition probabilities at multiple values of *t*, in order to build up a picture of an individual's disease progression. `pmatrix.simfs` only allows a scalar for *t*, so estimating probabilities at multiple values requires manually iterating through the time-scale. In the example below we will calculate transition probabilities at yearly intervals for 9 years.

``` r
predict_transitions(models, newdata, tmat, times=seq(9)*365)
#>      age dissub start_time end_time start_state   healthy   illness
#> 1  20-40    AML          0      365     healthy 0.4664881 0.1928780
#> 2  20-40    AML          0      365     illness 0.0000000 0.6840082
#> 3  20-40    AML          0      365       death 0.0000000 0.0000000
#> 4  20-40    AML          0      730     healthy 0.3532633 0.2070860
#> 5  20-40    AML          0      730     illness 0.0000000 0.5915287
#> 6  20-40    AML          0      730       death 0.0000000 0.0000000
#> 7  20-40    AML          0     1095     healthy 0.2852604 0.2080053
#> 8  20-40    AML          0     1095     illness 0.0000000 0.5306363
#> 9  20-40    AML          0     1095       death 0.0000000 0.0000000
#> 10 20-40    AML          0     1460     healthy 0.2390992 0.2047480
#> 11 20-40    AML          0     1460     illness 0.0000000 0.4851973
#> 12 20-40    AML          0     1460       death 0.0000000 0.0000000
#> 13 20-40    AML          0     1825     healthy 0.2057272 0.1983334
#> 14 20-40    AML          0     1825     illness 0.0000000 0.4494665
#> 15 20-40    AML          0     1825       death 0.0000000 0.0000000
#> 16 20-40    AML          0     2190     healthy 0.1792694 0.1913593
#> 17 20-40    AML          0     2190     illness 0.0000000 0.4200813
#> 18 20-40    AML          0     2190       death 0.0000000 0.0000000
#> 19 20-40    AML          0     2555     healthy 0.1583470 0.1845849
#> 20 20-40    AML          0     2555     illness 0.0000000 0.3951598
#> 21 20-40    AML          0     2555       death 0.0000000 0.0000000
#> 22 20-40    AML          0     2920     healthy 0.1407817 0.1788897
#> 23 20-40    AML          0     2920     illness 0.0000000 0.3710391
#> 24 20-40    AML          0     2920       death 0.0000000 0.0000000
#> 25 20-40    AML          0     3285     healthy 0.1254346 0.1726750
#> 26 20-40    AML          0     3285     illness 0.0000000 0.3518226
#> 27 20-40    AML          0     3285       death 0.0000000 0.0000000
#>        death
#> 1  0.3406339
#> 2  0.3159918
#> 3  1.0000000
#> 4  0.4396507
#> 5  0.4084713
#> 6  1.0000000
#> 7  0.5067343
#> 8  0.4693637
#> 9  1.0000000
#> 10 0.5561528
#> 11 0.5148027
#> 12 1.0000000
#> 13 0.5959394
#> 14 0.5505335
#> 15 1.0000000
#> 16 0.6293713
#> 17 0.5799187
#> 18 1.0000000
#> 19 0.6570681
#> 20 0.6048402
#> 21 1.0000000
#> 22 0.6803285
#> 23 0.6289609
#> 24 1.0000000
#> 25 0.7018904
#> 26 0.6481774
#> 27 1.0000000
```

In `pmatrix.simfs` it is up to the user to manipulate the output to make it interpretable. Again, the probabilities agree with each other.

``` r
do.call('rbind', lapply(seq(9)*365, function(t) {
    pmatrix.simfs(models, tmat, newdata=newdata, t=t)
}))
#>          [,1]    [,2]    [,3]
#>  [1,] 0.46678 0.19556 0.33766
#>  [2,] 0.00000 0.68826 0.31174
#>  [3,] 0.00000 0.00000 1.00000
#>  [4,] 0.35336 0.20707 0.43957
#>  [5,] 0.00000 0.59502 0.40498
#>  [6,] 0.00000 0.00000 1.00000
#>  [7,] 0.28774 0.20751 0.50475
#>  [8,] 0.00000 0.53664 0.46336
#>  [9,] 0.00000 0.00000 1.00000
#> [10,] 0.23865 0.20370 0.55765
#> [11,] 0.00000 0.49079 0.50921
#> [12,] 0.00000 0.00000 1.00000
#> [13,] 0.20491 0.19849 0.59660
#> [14,] 0.00000 0.45223 0.54777
#> [15,] 0.00000 0.00000 1.00000
#> [16,] 0.17759 0.19201 0.63040
#> [17,] 0.00000 0.42195 0.57805
#> [18,] 0.00000 0.00000 1.00000
#> [19,] 0.15781 0.18373 0.65846
#> [20,] 0.00000 0.39903 0.60097
#> [21,] 0.00000 0.00000 1.00000
#> [22,] 0.13890 0.17927 0.68183
#> [23,] 0.00000 0.37531 0.62469
#> [24,] 0.00000 0.00000 1.00000
#> [25,] 0.12540 0.17372 0.70088
#> [26,] 0.00000 0.35274 0.64726
#> [27,] 0.00000 0.00000 1.00000
```

By removing this boilerplate code, the speed increase of `rdes` starts to show, with the calculation of 8 additional time-points only increasing the runtime by 61% from 1.2s to 2s, while `flexsurv` has a twelve-fold increase from 0.3s to 3.7s.

``` r
microbenchmark("rdes"=predict_transitions(models, newdata, tmat, times=seq(9)*365),
               "flexsurv"={do.call('rbind', lapply(seq(9)*365, function(t) {
                            pmatrix.simfs(models, tmat, newdata=newdata, t=t)}))
               }, times=10)
#> Unit: seconds
#>      expr      min       lq     mean   median       uq      max neval cld
#>      rdes 1.901076 1.936707 2.085239 2.067634 2.208502 2.335424    10  a 
#>  flexsurv 3.571404 3.768065 4.214759 3.998040 4.344338 5.455324    10   b
```

### Changing start time

`pmatrix.simfs` limits the user to using *s* = 0. In `rdes` this is fully customisable. For example, the call below shows estimates the 1-year transition probabilities conditioned on the individual being alive at 6 months (technically it also calculates the transition probabilities conditioned on being dead at 6 months in the third row, but these aren't helpful). Notice how the probabilities of being dead at 1 year have decreased as a result.

``` r
predict_transitions(models, newdata, tmat, times=365, start_times = 365/2)
#>     age dissub start_time end_time start_state   healthy    illness
#> 1 20-40    AML      182.5      365     healthy 0.8118644 0.07838118
#> 2 20-40    AML      182.5      365     illness 0.0000000 0.89866157
#> 3 20-40    AML      182.5      365       death 0.0000000 0.00000000
#>       death
#> 1 0.1097544
#> 2 0.1013384
#> 3 1.0000000
```

Multiple values of *s* can be provided, such as the quarterly predictions below.

``` r
predict_transitions(models, newdata, tmat, times=365, 
                    start_times = c(0.25, 0.5, 0.75) * 365)
#>     age dissub start_time end_time start_state   healthy    illness
#> 1 20-40    AML      91.25      365     healthy 0.7016978 0.11646227
#> 2 20-40    AML      91.25      365     illness 0.0000000 0.83461363
#> 3 20-40    AML      91.25      365       death 0.0000000 0.00000000
#> 4 20-40    AML     182.50      365     healthy 0.8140123 0.07553048
#> 5 20-40    AML     182.50      365     illness 0.0000000 0.89841624
#> 6 20-40    AML     182.50      365       death 0.0000000 0.00000000
#> 7 20-40    AML     273.75      365     healthy 0.9111550 0.03870743
#> 8 20-40    AML     273.75      365     illness 0.0000000 0.95209065
#> 9 20-40    AML     273.75      365       death 0.0000000 0.00000000
#>        death
#> 1 0.18183988
#> 2 0.16538637
#> 3 1.00000000
#> 4 0.11045727
#> 5 0.10158376
#> 6 1.00000000
#> 7 0.05013755
#> 8 0.04790935
#> 9 1.00000000
```

Finally, any combination of number of *s* and *t* can be specified provided that all *s* are less than *m**i**n*(*t*).

``` r
predict_transitions(models, newdata, tmat, times=seq(2)*365, 
                    start_times = c(0.25, 0.5, 0.75) * 365)
#>      age dissub start_time end_time start_state   healthy    illness
#> 1  20-40    AML      91.25      365     healthy 0.6991471 0.11710310
#> 2  20-40    AML      91.25      365     illness 0.0000000 0.83637233
#> 3  20-40    AML      91.25      365       death 0.0000000 0.00000000
#> 4  20-40    AML      91.25      730     healthy 0.5278468 0.16235224
#> 5  20-40    AML      91.25      730     illness 0.0000000 0.72484770
#> 6  20-40    AML      91.25      730       death 0.0000000 0.00000000
#> 7  20-40    AML     182.50      365     healthy 0.8139786 0.07651301
#> 8  20-40    AML     182.50      365     illness 0.0000000 0.90014379
#> 9  20-40    AML     182.50      365       death 0.0000000 0.00000000
#> 10 20-40    AML     182.50      730     healthy 0.6145430 0.13832271
#> 11 20-40    AML     182.50      730     illness 0.0000000 0.77940640
#> 12 20-40    AML     182.50      730       death 0.0000000 0.00000000
#> 13 20-40    AML     273.75      365     healthy 0.9114388 0.03807740
#> 14 20-40    AML     273.75      365     illness 0.0000000 0.95120984
#> 15 20-40    AML     273.75      365       death 0.0000000 0.00000000
#> 16 20-40    AML     273.75      730     healthy 0.6881242 0.11590980
#> 17 20-40    AML     273.75      730     illness 0.0000000 0.82235885
#> 18 20-40    AML     273.75      730       death 0.0000000 0.00000000
#>         death
#> 1  0.18374981
#> 2  0.16362767
#> 3  1.00000000
#> 4  0.30980099
#> 5  0.27515230
#> 6  1.00000000
#> 7  0.10950838
#> 8  0.09985621
#> 9  1.00000000
#> 10 0.24713425
#> 11 0.22059360
#> 12 1.00000000
#> 13 0.05048377
#> 14 0.04879016
#> 15 1.00000000
#> 16 0.19596598
#> 17 0.17764115
#> 18 1.00000000
```

Note that obtaining these additional probabilities does not increase the runtime of the function.

``` r
microbenchmark("time"=predict_transitions(models, newdata, tmat, 
                                          times=seq(2)*365, 
                                          start_times = c(0.25, 0.5, 0.75)*365),
               times=10)
#> Unit: seconds
#>  expr      min       lq     mean   median       uq      max neval
#>  time 1.643836 1.797277 1.847317 1.876238 1.901303 2.026394    10
```

### Multiple individuals

It's useful to be able to estimating transition probabilities for multiple individuals at once, for example to see how the outcomes differ for patients with different characteristics. `rdes` simply handles multiple rows supplied to `newdata`.

``` r
newdata_multi <- data.frame(age=c("20-40", ">40"), dissub=c("AML", "CML"))
```

``` r
predict_transitions(models, newdata_multi, tmat, times=365)
#>     age dissub start_time end_time start_state   healthy   illness
#> 1 20-40    AML          0      365     healthy 0.4681531 0.1956251
#> 2 20-40    AML          0      365     illness 0.0000000 0.6848053
#> 3 20-40    AML          0      365       death 0.0000000 0.0000000
#> 4   >40    CML          0      365     healthy 0.4301687 0.1995008
#> 5   >40    CML          0      365     illness 0.0000000 0.6526225
#> 6   >40    CML          0      365       death 0.0000000 0.0000000
#>       death
#> 1 0.3362218
#> 2 0.3151947
#> 3 1.0000000
#> 4 0.3703304
#> 5 0.3473775
#> 6 1.0000000
```

As with multiple times, `pmatrix.simfs` only handles a single individual at a time.

``` r
pmatrix.simfs(models, tmat, newdata=newdata_multi, t=365)
#> Error in basepar[i, ] <- add.covs(x[[i]], x[[i]]$res.t[x[[i]]$dlist$pars, : number of items to replace is not a multiple of replacement length
```

And the user has to manually iterate through each new individual they would like to estimate transition probabilities for.

``` r
do.call('rbind', lapply(seq(nrow(newdata_multi)), function(i) {
    pmatrix.simfs(models, tmat, newdata=newdata_multi[i, ], t=365)
}))
#>         [,1]    [,2]    [,3]
#> [1,] 0.47039 0.19116 0.33845
#> [2,] 0.00000 0.68555 0.31445
#> [3,] 0.00000 0.00000 1.00000
#> [4,] 0.42778 0.20008 0.37214
#> [5,] 0.00000 0.65354 0.34646
#> [6,] 0.00000 0.00000 1.00000
```

### Time-dependent covariates

The Markov assumption has already been violated by the use of a clock-reset time-scale, which is why we are using simulation in the first place. We can therefore add an other violation without it affecting our methodology. Owing to the use of clock-reset, the model does not take time-since-transplant into account for patients who have platelet recovery. This could be an important prognostic factor in that individual's survival. Similar scenarios are common in multi-state modelling, and are termed `state-arrival` times. We'll make a new set of models, where the transition from `pr` to `rfs` (transition 3) takes time-since-transplant into account. This information is already held in the `Tstart` variable produced by `msprep`.

``` r
models_arrival <- lapply(1:3, function(i) {
    if (i == 3) {
        flexsurvreg(Surv(time, status) ~ age + dissub + Tstart, data=long, dist='weibull')
    } else {
        
        flexsurvreg(Surv(time, status) ~ age + dissub, data=long, dist='weibull')
    }
})
```

Looking at the coefficient for this variable and it does seem to be prognostic for time-to-rfs.

``` r
models_arrival[[3]]
#> Call:
#> flexsurvreg(formula = Surv(time, status) ~ age + dissub + Tstart, 
#>     data = long, dist = "weibull")
#> 
#> Estimates: 
#>            data mean  est        L95%       U95%       se       
#> shape             NA   4.75e-01   4.58e-01   4.92e-01   8.64e-03
#> scale             NA   1.97e+03   1.53e+03   2.55e+03   2.59e+02
#> age20-40    4.76e-01   5.95e-02  -2.01e-01   3.20e-01   1.33e-01
#> age>40      3.28e-01  -4.25e-01  -7.03e-01  -1.47e-01   1.42e-01
#> dissubALL   2.07e-01  -2.37e-01  -4.90e-01   1.71e-02   1.29e-01
#> dissubCML   3.99e-01   3.34e-01   1.23e-01   5.45e-01   1.08e-01
#> Tstart      7.95e+00   3.27e-02   2.64e-02   3.90e-02   3.22e-03
#>            exp(est)   L95%       U95%     
#> shape             NA         NA         NA
#> scale             NA         NA         NA
#> age20-40    1.06e+00   8.18e-01   1.38e+00
#> age>40      6.54e-01   4.95e-01   8.63e-01
#> dissubALL   7.89e-01   6.13e-01   1.02e+00
#> dissubCML   1.40e+00   1.13e+00   1.73e+00
#> Tstart      1.03e+00   1.03e+00   1.04e+00
#> 
#> N = 5577,  Events: 2010,  Censored: 3567
#> Total time at risk: 2940953
#> Log-likelihood = -15286.67, df = 7
#> AIC = 30587.34
```

To estimate transition probabilities for models with state-arrival times, the variables needs to be included in `newdata` with an **initial value**, i.e. the value this variable has when the global clock is 0.

``` r
newdata_arrival <- data.frame(age="20-40", dissub="AML", Tstart=0)
```

Then in `predict_transitions` simply specify which variables in `newdata` are time-dependent, that is they increment at each transition along with the current clock value. This is particularly useful for modelling patient age at each state entry, rather than at the starting state. Notice how this slightly changes the probability of being in *death* from a person starting in *healthy* compared to the example below that omits the `tcovs` argument.

``` r
predict_transitions(models_arrival, newdata_arrival, tmat, times=365, tcovs='Tstart')
#>     age dissub Tstart start_time end_time start_state   healthy   illness
#> 1 20-40    AML      0          0      365     healthy 0.4737575 0.2175029
#> 2 20-40    AML      0          0      365     illness 0.0000000 0.6473591
#> 3 20-40    AML      0          0      365       death 0.0000000 0.0000000
#>       death
#> 1 0.3087395
#> 2 0.3526409
#> 3 1.0000000
```

``` r
predict_transitions(models_arrival, newdata_arrival, tmat, times=365)
#>     age dissub Tstart start_time end_time start_state  healthy   illness
#> 1 20-40    AML      0          0      365     healthy 0.472175 0.1809571
#> 2 20-40    AML      0          0      365     illness 0.000000 0.6447332
#> 3 20-40    AML      0          0      365       death 0.000000 0.0000000
#>       death
#> 1 0.3468678
#> 2 0.3552668
#> 3 1.0000000
```

This functionality is implemented in `pmatrix.simfs`, but the `tcovs` argument actually has no impact on the transition probabilities, as evidenced below.

``` r
pmatrix.simfs(models_arrival, tmat, newdata=newdata_arrival, t=365, tcovs='Tstart')
#>         [,1]    [,2]    [,3]
#> [1,] 0.46879 0.18320 0.34801
#> [2,] 0.00000 0.64802 0.35198
#> [3,] 0.00000 0.00000 1.00000
```

``` r
pmatrix.simfs(models_arrival, tmat, newdata=newdata_arrival, t=365)
#>         [,1]    [,2]    [,3]
#> [1,] 0.47052 0.18364 0.34584
#> [2,] 0.00000 0.64802 0.35198
#> [3,] 0.00000 0.00000 1.00000
```

### Mixture of distributions

Sometimes greater flexibility in the model structure is required, so that every transition isn't obliged to use the same distribution. This could be useful if any transitions have few observations and would benefit from a simpler model such as an exponential, or if there is a requirement to use existing models from literature. Furthermore, if prediction is the goal, then it could be the case that allowing different distributions for each transition offers better overall fit.

An example is shown below, where each transition uses a different distribution family.

``` r
models_mix <- lapply(1:3, function(i) {
    if (i == 1) {
        flexsurvreg(Surv(time, status) ~ age + dissub, data=long, dist='weibull')
    } else if (i == 2) {
        flexsurvreg(Surv(time, status) ~ age + dissub, data=long, dist='exp')
    } else {
        flexsurvreg(Surv(time, status) ~ age + dissub, data=long, dist='lnorm')
    }
})
```

`rdes` handles these cases with no problems; currently the following distributions are supported:

-   Weibull
-   Gamma
-   Exponential
-   Log-normal
-   Log-logistic
-   Gompertz

``` r
predict_transitions(models_mix, newdata, tmat, times=365)
#>     age dissub start_time end_time start_state   healthy   illness
#> 1 20-40    AML          0      365     healthy 0.5400573 0.2076133
#> 2 20-40    AML          0      365     illness 0.0000000 0.6504390
#> 3 20-40    AML          0      365       death 0.0000000 0.0000000
#>       death
#> 1 0.2523294
#> 2 0.3495610
#> 3 1.0000000
```

`pmatrix.simfs` does not seem to function correctly under these situations.

``` r
pmatrix.simfs(models_mix, tmat, newdata=newdata, t=365)
#>         [,1]    [,2]    [,3]
#> [1,] 0.25046 0.00043 0.74911
#> [2,] 0.00000 0.00000 1.00000
#> [3,] 0.00000 0.00000 1.00000
```

Upcoming features
-----------------

Future version of `rdes` will include the ability to estimate expected length of stay; similarly to how `predict_transitions` builds on `flexsurv::pmatrix.simfs`, this feature would extend `flexsurv::totlos.simfs`.

I also intend to release an interface to running a full cohort wide simulation, where the simulation entry (or incidence) function and patient characteristics may also be modelled, and the outcomes of interest are global measures, such as amount of total time spent in a particular state over a set time-frame. This was the original motivation for developing the simulation engine for its use in health economic evaluation.

There is currently a web-app (not currently publicly accessible but the [source code is on Github](https://github.com/stulacy/RDES-Shiny)) that provides a graphical interface for the entire multi-state modelling process and simulation process for a cohort simulation. I'd like to tidy this up and get it functioning with this new version of `rdes` and also provide an interface for individual level simulations, such as estimating transition probabilities.

References
----------

de Wreede, Liesbeth C, Marta Fiocco, and Hein Putter. 2011. “Mstate: An R Package for the Analysis of Competing Risks and Multi-State Models.” *Journal of Statistical Software* 38.
