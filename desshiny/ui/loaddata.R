tabPanel("Upload Data",
     sidebarLayout(
         sidebarPanel(
            h3("Upload data"),
            HTML("<h4><strong>NB: This step is entirely optional</strong></h4>"),
            HTML(paste("Upload a CSV file containing patient level data on transition times and attribute values.",
                    "If specified, then models of transition times and individual level attributes can be based on this data.",
                    "If not provided then these values will have to be completely specified.")),
            br(),
            HTML(paste("Please organise the data in long format, similar to that required by the <code>mstate</code> package, with a row per possible transition.",
                    "Each row should include patient level covariates as well as the standard time and status indicator for that transition.",
                    "A column indicating the transition each row belongs to would also be very useful")),
            hr(),
            fileInput("covarinput", "Select CSV file",
                   accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            uiOutput("specifycols")
         ),
         mainPanel(
             h3("Data Peak"),
             uiOutput("datatable"),
             h3("Attribute information"),
             uiOutput("rawattrinfo")
         )
    )
)

