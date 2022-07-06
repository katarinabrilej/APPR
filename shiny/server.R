library(shiny)

shinyServer(function(input, output) {
  
  output$prvi_stolpec <- renderPlot({
    graf.namen <- ggplot(namen_priseljevanja%>%
                           filter(namen == input$prvi_stolpec)) +
      aes(x=leto, y=stevilo, fill=drzava) +
      coord_flip() +
      labs( x='Leto', y = 'Delež gospodinjstev') + 
      geom_col(position="dodge") +
      theme_dark() +
      scale_fill_brewer(palette = "BrBG") 
    #scale_x_continuous(breaks = seq(2011, 2019, by=4))
    print(graf.namen)
  })
  
  
  output$drugi_stolpec <- renderPlot({
    graf.izobrazba <- ggplot(izobrazba_izseljeni%>%
                           filter(izobrazba == input$drugi_stolpec)) +
      aes(x=leto, y=stevilo) + 
      geom_point(aes(col=spol, size=1)) +
      facet_grid(vec ~ .) +
      labs( x='Leto', y = 'Delež gospodinjstev') + 
      theme_grey() +
      scale_fill_brewer(palette = "BrBG") 
      scale_x_continuous(breaks = seq(2011, 2020, by=2))
      print(graf.izobrazba)
  })
  
})

