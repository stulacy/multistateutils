---
title: 'Estimating statistics from multi-state models using simulation with multistateutils'
tags:
  - R
  - multi-state modelling
  - statistics
  - discrete event simulation
  - health economic modelling
authors:
 - name: Stuart Lacy
   orcid: 0000-0002-8570-7528
   affiliation: "1"
affiliations:
 - name: Department of Health Sciences, University of York, UK
   index: 1
date: 24 July 2018
bibliography: paper.bib
---

# Summary

`multistateutils` provides functionality for analysing multi-state models using Discrete Event Simulation (DES) in R.

DES is a method of exploring the behaviour of dynamic systems by discretising trajectories into a series of state transitions that occur at specific times.
There exists general-purpose DES software packages in several programming languages, such as `simmer` (R) [@simmer] or `simPy` (Python) [@matloff2008introduction] that can be applied to many systems regardless of the application area.
Multi-state modelling is an area of biostatistics that is concerned with the extension of survival analysis to the case where there are multiple time-to-event transitions of interest, such as a patient experiencing various treatments on their disease pathway[@meira2009multi].
There are several measures of interest that can be generated from a multi-state model, such as the probability of being in a given state at a certain time, or the estimated time spent in a given state, which can be obtained analytically provided certain assumptions - most notably the Markov property - are met; however, simulation provides a more flexible alternative and can be used to extract any desired summary statistic.

`multistateutils` is designed for the specific use case of multi-state modelling in biostatistics and provides a means for estimating statistics of interest from a multi-state model by simulation without exposing users to DES specific terminology.
The functions in this package operate on parametric statistical models of transition times from the widely-used `flexsurv` [@flexsurv] package, allowing the user to fit transition models in a familiar environment before inputting them directly into the simulation, rather than having to learn the parameterisation of a general purpose DES toolbox.
The primary functionality provided by `multistateutils` is an interface to running discrete event simulation over a cohort of patients, thereby providing the simulated outcomes from which users can estimate any required statistics. 
In addition, for two common statistics - transition probabilities and length of stay estimates - there are functions that will set up and run the simulation and return the estimates directly.

The simulation engine is written in C++ to provide an efficient implementation of a DES.
The source code has been archived in Zenodo with the DOI [@zenodo].

# References
