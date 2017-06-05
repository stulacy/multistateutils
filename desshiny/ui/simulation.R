tabPanel("Simulation",
     sidebarLayout(
         sidebarPanel(
             h3("Simulation parameters"),
             textInput("entryrate", "Entry rate (number/unit of time)"),
             selectInput("terminationcriteria", "Termination Criteria",
                         choices=c("Select one"="", "Time limit", "Individual limit")),
             uiOutput("termcriteriadiv"),
             hr(),
             h4("Individual horizon"),
             uiOutput("individualhorizon"),
             hr(),
             sliderInput("simslider", "Number of simulations", min=1, max=10000, value=1),
             actionButton("runmultiplesimbutton", "Run simulation(s)"),
             uiOutput("savebuttons")
         ),
         mainPanel(
             h3("Run simulation"),
             uiOutput("simendstates")
         )
    )
)