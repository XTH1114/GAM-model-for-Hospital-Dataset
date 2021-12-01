######## R version: 4.0.3
######## Author: Tinghui Xu

# library all the needed packages simoutaneously----------------------------------------------------

library(shiny)
library(tidyverse)
library(magrittr)
library(gam)
library(knitr)
library(shinythemes)



# data part ---------------------------------------------------------------

data <- read.csv("sample.csv",encoding = "utf-8")
variable.abbreviation <-colnames(data)[20:43]

# Shiny part --------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("flatly"),
  sidebarLayout(
    sidebarPanel(
      h2("Analysis using Polynomial Curve and GAM"),
      h4("Data Filtering"),
      sliderInput("age_range","Age Difference (Months)",value = c(-50,250),min = -100, max = 300),
      selectInput("variable","Variable Names",variable.abbreviation,multiple = F),
      p("Please click this button at the first time!"),
      actionButton("action","Filter!",icon = icon("hand-point-right"),class = "btn-success"),
      h4("Polynomial Curve Fitting"),
      numericInput("polydegree","Degree of Polynomial",value = 3, min = 1, max = 10),
      h4("Plot Parameters"),
      numericInput("grid_interval","Number of grids",value = 10, min = 0,step = 3)),
    mainPanel(
      textOutput("sample_number"),
      plotOutput("poly"),
      plotOutput("gam")
    )
  )
)



