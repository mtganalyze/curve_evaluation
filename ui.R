
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com





library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(magrittr)

shinyUI(fluidPage(

  # Application title
  titlePanel("Curve Evaluator"),
  
  # Set the mana curve
  h2("Mana Curve:"),
  plotOutput("manacurve", width = "70%"),
  
  textOutput("landcount"),
  tags$head(tags$style("#landcount{color: navy;
                                   font-size: 20px;
                                   font-style: bold;
                                   }")),
  h3(" "),
  mainPanel(
   inputPanel(
      numericInput("d1", label = h4("1-drops:"), value = 2, width = "80px"),
      numericInput("d2", label = h4("2-drops:"), value = 4, width = "80px"),
      numericInput("d3", label = h4("3-drops:"), value = 5, width = "80px"),
      numericInput("d4", label = h4("4-drops:"), value = 5, width = "80px")
    ),
    inputPanel(
      numericInput("d5", label = h4("5-drops:"), value = 4, width = "80px"),
      numericInput("d6", label = h4("6-drops:"), value = 2, width = "80px"),
      numericInput("d7", label = h4("7-drops:"), value = 0, width = "80px"),
      numericInput("d8", label = h4("8-drops:"), value = 0, width = "80px")
    ),
    
 
  # Evaluate the curve
    actionButton("submit", label = "Evaluate Curve"), 
  
  # Show evaluation of curve
    h3("Curve Evaluation:"),
    plotOutput("evaluation", width = "110%"),
  
  # Show execute curve  
    h3("Mean Executed Curve:"),
    plotOutput("execurve"),
    
  
  # documentation
    h2("Links and documentation:"),
    a("MTGANALYZE BLOG: on curve evaluation", href= "https://mtganalyze.github.io/post/curve_considerations/" , target = "_blank")

  ))
)
