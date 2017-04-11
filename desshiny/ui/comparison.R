tabPanel("Event Time Distributions",
    sidebarLayout(
        sidebarPanel(
            h3("Transition time distributions"),
            uiOutput("newdataarea")
        ),
        mainPanel(
            h3("Event time distributions"),
            plotOutput("plottingeventdraws")
        )
    )
)
