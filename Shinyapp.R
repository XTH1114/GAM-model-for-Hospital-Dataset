######## R version: 4.0.3
######## Author: Tinghui Xu

# install required packages -----------------------------------------------

list_of_packages <- c("shiny","tidyverse","magrittr","gam","knitr","shinythemes")
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages))install.packages(missing_packages)


# library all the needed packages simoutaneously----------------------------------------------------

invisible(sapply(list_of_packages,library,character.only = T))


# Or library them one by one ----------------------------------------------

# library(shiny)
# library(tidyverse)
# library(magrittr)
# library(shinythemes)
# library(gam)
# library(knitr)


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

server <- function(input, output, session) {
  
  data1 <- eventReactive(input$action,{data %>% filter(age_difference_month<=input$age_range[2]&age_difference_month>=input$age_range[1])})
  variable <- eventReactive(input$action,{input$variable})
  min1 <- eventReactive(input$action,{
    input$age_range[1]
  })
  max1 <- eventReactive(input$action,{
    input$age_range[2]
  })
  grid_number <- reactive({input$grid_interval})
  output$poly <- renderPlot({
    x <- seq(min1(),max1(),length.out = 300)
    data.pred <- data.frame(age_difference_month = x)
    mod1 <- lm(as.formula(paste0(variable(),"~poly(age_difference_month",",",input$polydegree,")")),data = data1())
    
    y <- predict(mod1,data.pred)
    plot(x,y,type = "l",xaxt = "n",xlim = c(min1(),max1()),lwd = 2,xlab = "Age Difference in Month",ylab = variable())
    par(xaxp = c(min1(),max1(),grid_number()))
    axis(1,at = NULL)
    par(xaxp = c(min1(),max1(),grid_number()))
    grid()
    par(xaxp = c(min1(),max1(),floor(grid_number()/2)))
    grid(ny = NA,col = 2,lwd = 1.7)
    par(xaxp = c(0,0,1))
    grid(ny = NA,col = "blue",lwd = 2)
    title(main = paste0("Polynomial Curve for ",variable()))
  })
  output$gam <- renderPlot({
    gam_analysis <- gam(as.formula(paste0(variable(),"~s(age_difference_month)")),data = data1())
    plot.Gam(gam_analysis,xlab = "Age Difference in Month",xlim = c(min1(),max1()),lwd = 2,se = T,rugplot = F,xaxt = "n")
    par(xaxp = c(min1(),max1(),grid_number()))
    axis(1,at = NULL)
    par(xaxp = c(min1(),max1(),grid_number()))
    grid()
    par(xaxp = c(min1(),max1(),floor(grid_number()/2)))
    grid(ny = NA,col = 2,lwd = 1.7)
    par(xaxp = c(0,0,1))
    grid(ny = NA,col = "blue",lwd = 2)
    title(main = paste0("GAM for ",variable()))
  })
  
  output$sample_number <- renderText({
    paste0("The number of the filtered observations is  ",data1()%>%nrow,".")
  })
  
}

shinyApp(ui, server)


