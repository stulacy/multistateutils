tabPanel("Transitions",
     sidebarLayout(
         sidebarPanel(
             h3("Transition Probabilities"),
             uiOutput("probsheader"),
             uiOutput("seltransprobs"),
             uiOutput("transprobsbuttons"),
             hr(),
             uiOutput("addtransarea")
         ),
         mainPanel(
             h3("Transition probabilities"),
             uiOutput("transprobssummary"),
             uiOutput("aicdiv")
         )
    )
)