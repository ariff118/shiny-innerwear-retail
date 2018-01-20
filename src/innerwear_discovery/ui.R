#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

df <- read_csv("../data/cleaned/cleaned_shiny.csv")
# str(df)
df$category <- as.factor(df$category)
df$color_group <- as.factor(df$color_group)
df$brand <- as.factor(df$brand)
df$review_count <- as.integer(df$review_count)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Innerwear and Swimwear E-commerce Market"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput('brandInput', 'Select Brand',
                         choices = c(""),
                         selected = c("")),
      
      selectInput('categoryInput', 'Select Product Category',
                  choices = c(""),
                  multiple = TRUE),
      
      selectInput('colorInput', 'Select Product Color',
                  choices = c(""),
                  multiple = TRUE),
      
      sliderInput("ratingInput", "Average Rating Range",
                   min = 1,
                   max = 10,
                   value = c(4.5, 5), sep = ""),
      
      sliderInput("priceInput", "Price Range",
                  min = 1,
                  max = 100,
                  value = c(20, 50), sep = ""),
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
