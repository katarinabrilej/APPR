library(shiny)
shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Priseljeno prebivalstvo glede na namen selitve",
             titlePanel(""),
             sidebarPanel(
               selectInput(inputId = "prvi_stolpec",
                           label = "Izberi namen",
                           choices = unique(namen_priseljevanja$namen))),
             mainPanel(plotOutput("prvi_stolpec"))),
    
    
    tabPanel("Izseljeno prebivalstvo glede na stopnjo izobrazbe",
             titlePanel(""),
             sidebarPanel(
               selectInput(inputId = "drugi_stolpec",
                           label = "Izberi vrsto izobrazbe",
                           choices = unique(izobrazba_izseljeni$izobrazba))),
             mainPanel(plotOutput("drugi_stolpec"))),
    
  )             
))