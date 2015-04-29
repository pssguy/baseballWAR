
shinyUI(fluidPage(


  titlePanel("Cumulative WAR by Age"),


    mainPanel(
      inputPanel(
        selectInput("team","Select team. Choose Players to display their cumulative WAR by Age",teamChoice)
      ),
      fluidRow(
        column(6,DT::dataTableOutput('career')),
               column(6, ggvisOutput("plot"))
        )
      
    )

))
