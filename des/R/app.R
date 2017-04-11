library(shiny)
library(dplyr)
library(mstate)
library(tidyr)
library(ggplot2)
library(DiagrammeR)
library(cowplot)
library(flexsurv)
library(parallel)
library(jsonlite)
library(profvis)

ui <- navbarPage("Discrete Event Simulation",
                 source(file.path("ui", "loaddata.R"), local=T)$value,
                 source(file.path("ui", "states.R"), local=T)$value,
                 source(file.path("ui", "attributes.R"), local=T)$value,
                 source(file.path("ui", "transitions.R"), local=T)$value,
                 source(file.path("ui", "comparison.R"), local=T)$value,
                 source(file.path("ui", "simulation.R"), local=T)$value
)


server <- function(input, output, session) {
                 source(file.path("server", "static.R"), local=T)$value
                 source(file.path("server", "utils.R"), local=T)$value
                 source(file.path("server", "loaddata.R"), local=T)$value
                 source(file.path("server", "states.R"), local=T)$value
                 source(file.path("server", "attributes.R"), local=T)$value
                 source(file.path("server", "transitions.R"), local=T)$value
                 source(file.path("server", "comparison.R"), local=T)$value
                 source(file.path("server", "simulation.R"), local=T)$valu
}

shinyApp(ui=ui, server=server)