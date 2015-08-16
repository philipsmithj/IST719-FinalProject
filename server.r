library(shiny)


shinyServer(
  function(input, output){
    output$selYear<-renderText(input$year)
    output$selStates<-renderText(input$states)
    output$selCat<- renderText(input$catGroup)
    output$thisPlot <- renderPlot(userPlot(input$states, input$year, input$catGroup))
    output$natPlot<- renderPlot(natlComPlot(input$year, input$catGroup))
    output$overPlot<- renderPlot(overviewPlot)
    output$timePlot<- renderPlot(overTime)
    output$popPlot<- renderPlot(popMap(input$sYear))
    output$stackPlot<-renderPlot(stacked(input$sYear,input$withPop, input$checkGroup))
    
    #ouput$threeD<-renderWebGL(td)
  }
)