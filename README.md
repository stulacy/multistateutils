
<!-- README.md is generated from README.Rmd. Please edit that file -->
multistateutils
===============

`multistateutils` provides a number of useful functions for analyzing parametric multi-state models. In this way it does not aim to supplement the modelling strategies found in `mstate`, `msm`, or `flexsurv`, but rather provide tools for subsequent analysis. It is designed to be used with semi-Markov multi-state models of healthcare data, but can be used for any system that can be discretized into the states being entered at observed time-points, with parametric families providing appropriate fits for these transition rates.

It currently provides several features:

-   Estimate transition probabilities
-   Estimate length of stay
-   Draw predicted pathways through the state space with dynamic prediction

Examples of these features are provided below in *Examples*.

Installation
------------

You can install `multistateutils` from github with:

``` r
install.packages("devtools")  # install devtools if it isn't already
devtools::install_github("stulacy/multistateutils")
```

### Windows

Note that since the simulation engine is written in C++, Windows users will need to have `Rtools` installed, which can be obtained from <https://cran.r-project.org/bin/windows/Rtools/>, and then set to the right path with:

``` r
devtools::find_rtools()
```

Once `Rtools` is setup run `devtools::install_github("stulacy/multistateutils")` as above.

Examples
--------

This section provides a brief overview of the features in `multistateutils`, please see the *Examples* vignette for a thorough description.

### Setup

In these examples we'll use the `ebmt3` data set provided in the `mstate` package. It describes recovery after bone marrow transplant. We'll model it with an illness-death model, with *transplant* as the starting state, platelet recovery *pr* as the illness state, and relapse-free survival *rfs* as the death state.

``` r
library(mstate)
#> Loading required package: survival
data(ebmt3)
tmat <- trans.illdeath()                             # Form transition matrix
long <- msprep(time=c(NA, 'prtime', 'rfstime'),      # Convert data to long
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

`multistateutils` is designed for applied modelling with an overall aim of prediction, particularly in health-related areas. For this reason it is designed to work with parametric transition models built with `flexsurv`. The example below fits a Weibull model to each transition using the `age` and `dissub` covariates.

``` r
library(flexsurv)
models <- lapply(1:3, function(i) {
    flexsurvreg(Surv(time, status) ~ age + dissub, data=long, dist='weibull')
})
```

Estimating transition probabilities
-----------------------------------

Transition probabilities are defined as the probability of being in a state *j* at a time *t*, given being in state *h* at time *s*, as shown below where *X*(*t*) gives the state an individual is in at *t*. This is all conditional on the individual parameterised by their covariates and history, which for this semi-Markov model only influences transition probabilities through state arrival times.

*P*<sub>*h*, *j*</sub>(*s*, *t*)=Pr(*X*(*t*)=*j* | *X*(*s*)=*h*)

We'll estimate the transition probabilities of an individual with the covariates `age=20-40` and `dissub=AML` at 1 year after transplant.

``` r
newdata <- data.frame(age="20-40", dissub="AML")
```

``` r
library(multistateutils)
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

This functionality is already provided in `flexsurv::pmatrix.simfs`, but it is limited to assessing only one individual at a time, with *s* = 0

``` r
pmatrix.simfs(models, tmat, newdata=newdata, t=365)
#>         [,1]    [,2]    [,3]
#> [1,] 0.47134 0.19118 0.33748
#> [2,] 0.00000 0.68671 0.31329
#> [3,] 0.00000 0.00000 1.00000
```

`predict_transitions` on the other hand can estimate transition probabilities for:

-   Multiple individuals
-   Multiple values of *t*
-   Multiple values of *s*
-   Any combination of the above, all from the same simulation
-   A mixture of distributions for the transition models

The ability to generate probabilities by varying *s* and *t* within a single simulation makes it very efficient for generating dynamic predictions.

``` r
predict_transitions(models, newdata, tmat, times=c(1, 2)*365, 
                    start_times = c(0.25, 0.75) * 365)
#>      age dissub start_time end_time start_state   healthy    illness
#> 1  20-40    AML      91.25      365     healthy 0.6985782 0.11827607
#> 2  20-40    AML      91.25      365     illness 0.0000000 0.83396061
#> 3  20-40    AML      91.25      365       death 0.0000000 0.00000000
#> 4  20-40    AML      91.25      730     healthy 0.5242891 0.16199645
#> 5  20-40    AML      91.25      730     illness 0.0000000 0.72123949
#> 6  20-40    AML      91.25      730       death 0.0000000 0.00000000
#> 7  20-40    AML     273.75      365     healthy 0.9110012 0.03866656
#> 8  20-40    AML     273.75      365     illness 0.0000000 0.95100475
#> 9  20-40    AML     273.75      365       death 0.0000000 0.00000000
#> 10 20-40    AML     273.75      730     healthy 0.6837145 0.11511125
#> 11 20-40    AML     273.75      730     illness 0.0000000 0.82027680
#> 12 20-40    AML     273.75      730       death 0.0000000 0.00000000
#>         death
#> 1  0.18314573
#> 2  0.16603939
#> 3  1.00000000
#> 4  0.31371445
#> 5  0.27876051
#> 6  1.00000000
#> 7  0.05033220
#> 8  0.04899525
#> 9  1.00000000
#> 10 0.20117429
#> 11 0.17972320
#> 12 1.00000000
```

Length of stay
--------------

Similarly, the length of stay functionality provided by `flexsurv::totlos.simfs` has also been extended to allow for estimates at multiple time-points, states, and individuals to be calculated simultaneously. As shown below, the function parameters are very similar and the estimates are very close to those produced by `totlos.simf`.

``` r
length_of_stay(models, 
               newdata=newdata,
               tmat, times=365.25*3,
               start=1)
#>         t start_state   age dissub  healthy  illness    death
#> 1 1095.75     healthy 20-40    AML 486.1548 206.5854 403.0098
```

``` r
totlos.simfs(models, tmat, t=365.25*3, start=1, newdata=newdata)
#>        1        2        3 
#> 485.7349 208.9578 401.0573
```

Again, the advantage of this implementation is that estimates can be produced from multiple conditions without needing to rerun the simulation.

``` r
length_of_stay(models, 
               newdata=data.frame(age=c(">40", ">40"),
                                  dissub=c('CML', 'AML')),
               tmat, times=c(1, 3, 5)*365.25)
#>         t start_state age dissub  healthy   illness     death
#> 1  365.25     healthy >40    AML 129.8734  42.52023  71.47468
#> 2  365.25     healthy >40    CML 138.9012  38.72228  64.76035
#> 3 1095.75     healthy >40    AML 263.7792 143.23360 324.59198
#> 4 1095.75     healthy >40    CML 295.5594 137.13389 294.45822
#> 5 1826.25     healthy >40    AML 344.5806 234.46875 640.29199
#> 6 1826.25     healthy >40    CML 395.5353 231.76231 584.62158
```

State flow diagram
------------------

Another feature in `multistateutils` is a visualization of a predicted pathway through the state transition model, calculated using *dynamic prediction* and provided in the function `plot_predicted_pathway`. It estimates state occupancy probabilities at discrete time-points and displays the flow between them in the manner of a Sankey diagram as shown below.

The output of this function is an interactive HTML widget that can be manipulated to layout the diagram to better suit your needs. Unfortunately widgets can't be embedded in GitHub READMEs so the image below is just a screenshot, but have a look at the version in the vignette for an working example.

*P*<sub>*h*, *j*</sub>(*s*, *t*)=Pr(*X*(*t*)=*j* | *X*(*s*)=*h*)

``` r
time_points <- seq(0, 10, by=2) * 365.25
plot_predicted_pathway(models, tmat, newdata, time_points, 'healthy')
```

![](vignettes/state_pathway.png)

Upcoming features
-----------------

I intend to release an interface to running a full cohort wide simulation, where the simulation entry (or incidence) function and patient characteristics may also be modelled, and the outcomes of interest are global measures, such as amount of total time spent in a particular state over a set time-frame. This was the original motivation for developing the simulation engine for its use in health economic evaluation.

There is currently a web-app (not currently publicly accessible but the [source code is on Github](https://github.com/stulacy/RDES-Shiny)) that provides a graphical interface for the entire multi-state modelling process and simulation process for a cohort simulation. I'd like to tidy this up and get it functioning with this new version of `multistateutils` and also provide an interface for individual level simulations, such as estimating transition probabilities.

References
----------
