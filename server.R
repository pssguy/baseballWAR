

shinyServer(function(input, output) {

#   output$career <- DT::renderDataTable({
#     career <- teamValues %>% 
#       group_by(Name) %>% 
#       arrange(desc(Age)) %>% 
#       slice(1) %>% 
#       select(Name,Age,WAR=cumVal) %>% 
#       DT::datatable(rownames = checkboxRows(.), escape = -1,
#                 ,options= list(paging = FALSE, searching = FALSE,info=FALSE, order = list(list(3, 'desc')))) 
#     
# 
# })
  
  theData <- reactive({
    print(input$team)
 df <- data.frame(allTeamValues %>% 
      filter(teamID==input$team) %>%            
      group_by(Name) %>% 
      arrange(desc(Age)) %>% 
      slice(1) %>% 
      select(Name,Age,WAR=cumVal) %>%
        ungroup() %>%
      arrange(desc(WAR)))
 info=list(df=df)
 return(info)
 
  })
  
  
  output$career <- DT::renderDataTable({
    theData()$df %>% 
      DT::datatable(rownames = checkboxRows(., checked=c(1:5)), escape = -1,
                    ,options= list(paging = FALSE, searching = FALSE,info=FALSE, order = list(list(3, 'desc')))) 
    
    
  })
 
#   # this works
# #   teamValues %>% 
# #     group_by(Name) %>% 
# #     ggvis(~Age,~cumVal, stroke=~Name) %>% 
# #     layer_lines() %>%
# #     bind_shiny("plot")
#   
# #   output$plot <- renderPlot({
# #   teamValues %>% 
# #     group_by(Name) %>% 
# #     ggvis(~Age,~cumVal, stroke=~Name) %>% 
# #     layer_lines() %>%
# #     bind_shiny()
# #   })
#   
#   
 #  output$plot <- renderPlot({
  observe({
     print("input$career_selected")
     print(input$career_selected)
      if (is.null(input$career_selected)) {
        s <- c(1:5)
      } else {
      
      s = input$career_selected
      }
      # selected Shane Victorino, Mike Napoli, Koji Uehara
      print(s) #[1] 23 16 15
#       print(str(theData()))
#       print(theData())
#       print("rownames")
#       print(row.names(theData()))
      print(str(s))
      print("length")
      print(length(s))
      
      players <- theData()$df[s,]$Name
      print(players)
      
    #  selected <- data()[data()$row.]
      #print(career())
      if (length(s)) {
#         df <- teamValues[s,]
#         print(df)
    allTeamValues %>% 
          filter(Name %in% players) %>%
      group_by(Name) %>% 
      ggvis(~Age,~cumVal, stroke=~Name) %>% 
      layer_lines() %>%
      bind_shiny('plot')
    }
    })
  
  
  
})
