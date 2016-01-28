library(shiny)
library(ggplot2)
library(mvtnorm)

shinyUI(fluidPage(
  
  #Application Title
  titlePanel("Discrimination Function"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("meanSolApp",
        label = h3("Solvency Mean for Approved"),
        value = 1),
      numericInput("meanSolDen",
                   label = h3("Solvency Mean for Denied"),
                   value = 1),
      numericInput("sdSolApp",
                   label = h3("Solvency Standard Deviation for Approved"),
                   value = 1),
      numericInput("sdSolDen",
                   label = h3("Solvency Standard Deviation for Denied"),
                   value = 1),
      numericInput("meanPIApp",
                   label = h3("PI Mean for Approved"),
                   value = 1),
      numericInput("meanPIDen",
                   label = h3("PI Mean for Denied"),
                   value = 1),
      numericInput("sdPIApp",
                   label = h3("PI Standard Deviation for Approved"),
                   value = 1),
      numericInput("sdPIDen",
                   label = h3("PI Standard Deviation for Denied"),
                   value = 1)
      ),
    mainPanel(
      plotOutput("data_plot", height = "500px"),
      div(tableOutput("confusion_matrix"), style = "font-size:100%")
    )
    )
  )
)