# RDES

A fast implementation of discrete event simulation for R with parametric transition hazards. It is designed to be used with multi-state models of healthcare data, but can be used for system that can be discretised into the arrival into states at certain time-points, with parametric families providing appropriate fits for these transition rates. 
This package has not been designed for widespread use, rather it has been developed for internal use after I found existing simulation engines were too slow (such as `flexsurv::pmatrix.simfs`), or not flexible enough for my needs. 
As a result, it is not heavily documented, although if there is sufficient interest I will tidy it up and release it publicly. 
Currently, it is mostly used as the simulation engine behind a web-app written in Shiny that [provides a graphical interface for the entire multi-state modelling process.](https://github.com/stulacy/RDES-Shiny)

# Installation

Install the package from an R instance with `devtools::install_github('stulacy/RDES')`. 

If you are interested in using this simulation please get in touch with me (`stuart.lacy@gmail.com`), as firstly you may need some assistance since the package isn't that well documented, but I'd also be very interested to hear any feedback to help make it more user-friendly so I can publicly release it. Currently the only documentation is in *tests/example.R*, which provides an example call.
