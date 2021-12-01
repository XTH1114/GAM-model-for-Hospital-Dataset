######## R version: 4.0.3
######## Author: Tinghui Xu

# library all the needed packages simoutaneously----------------------------------------------------

library(shiny)
library(tidyverse)
library(magrittr)
library(gam)
library(knitr)
library(shinythemes)

data <- read.csv("sample.csv",encoding = "utf-8")
variable.abbreviation <-colnames(data)[20:43]


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