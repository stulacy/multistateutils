tabPanel("Attributes",
     sidebarLayout(
         sidebarPanel(
             h3("Individual attributes"),
             HTML(paste("Prior distributions for individual level attributes can be assigned to the simulation, thereby allowing transition probabilities to vary across individuals.",
                        "These distributions can be obtained from an uploaded CSV file from the previous step, or can be specified below in the <strong>Simulate</strong> section.",
                        "Mixtures of empirical and simulated prior distributions can be used.")),
             br(),
             br(),
             HTML(paste("Please note that currently only <strong>fixed-time</strong> attributes are permitted.",
                        "It is for this reason that we allow the subsetting of the uploaded data set to a particular transition.")),
             br(),
             hr(),
             h3("Empirical covariates"),
             uiOutput("loadattributes"),
             hr(),
             h3("Simulated covariates"),
             uiOutput("simattributes")
         ),
         mainPanel(
             uiOutput("sampledistheader"),
             plotOutput("plotarea")
         )
     )
)