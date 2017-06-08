if (!require(shiny)) install.packages("shiny")
if (!require(dplyr)) install.packages("dplyr")
if (!require(mstate)) install.packages("mstate")
if (!require(tidyr)) install.packages("tidyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(DiagrammeR)) install.packages("DiagrammeR")
if (!require(cowplot)) install.packages("cowplot")
if (!require(flexsurv)) install.packages("flexsurv")
if (!require(parallel)) install.packages("parallel")
if (!require(jsonlite)) install.packages("jsonlite")
if (!require(data.table)) install.packages("data.table")
if (!require(profvis)) install.packages("profvis")
if (!require(matrixStats)) install.packages("matrixStats")
require(des)

ui <- navbarPage("Discrete Event Simulation",
                 source(file.path("ui", "loaddata.R"), local=T)$value,
                 source(file.path("ui", "states.R"), local=T)$value,
                 source(file.path("ui", "attributes.R"), local=T)$value,
                 source(file.path("ui", "transitions.R"), local=T)$value,
                 source(file.path("ui", "comparison.R"), local=T)$value,
                 source(file.path("ui", "simulation.R"), local=T)$value
)

source(file.path("server", "static.R"), local=T)$value

server <- function(input, output, session) {
                 source(file.path("server", "utils.R"), local=T)$value
                 source(file.path("server", "loaddata.R"), local=T)$value
                 source(file.path("server", "states.R"), local=T)$value
                 source(file.path("server", "attributes.R"), local=T)$value
                 source(file.path("server", "transitions.R"), local=T)$value
                 source(file.path("server", "comparison.R"), local=T)$value
                 source(file.path("server", "simulation.R"), local=T)$value
}

shinyApp(ui=ui, server=server)