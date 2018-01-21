#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# read in data and set up important packages for this Shiny App
library(shiny)
library(tidyverse)

df <- read_csv("../data/cleaned/cleaned_shiny.csv")

#===================#

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Innerwear and Swimwear E-commerce Market"),
   
   # Features
   sidebarLayout(
     sidebarPanel(
       uiOutput('brandOutput'),
       
       uiOutput('categoryOutput'),
       
       uiOutput('colorInput'),
       
       sliderInput("ratingInput", "Average Rating Range",
                   min = 1,
                   max = 10,
                   value = c(3, 5), sep = ""),
       
       sliderInput("priceInput", "Price Range",
                   min = 1,
                   max = 100,
                   value = c(20, 50), sep = ""),
       
       radioButtons("orderbyInput", "Order by",
                    choices = c("Brand", "Price", "Percentage Sales"))
       
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("coolPlot"),
       br(), br(),
       dataTableOutput("results")
     )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  filtered <- reactive({
    df %>% 
      filter(brand %in% c(input$brandInput),
             category %in% c(input$categoryInput),
             color_group %in% c(input$colorInput),
             rating >= input$ratingInput[1],
             rating <= input$ratingInput[2],
             price >= input$priceInput[1],
             price <= input$priceInput[2]
      )
  })
   
  output$brandOutput <- renderUI({
     selectInput('brandInput', 'Select Brand',
                        sort(unique(df$brand_name)),
                        selected = c("Macys-Hanky Panky", "Amazon-Hanky Panky"),
                        multiple = TRUE)
   })
  
  output$categoryOutput <- renderUI({
    selectInput('categoryInput', 'Select Product Category',
                sort(unique(df$category)),
                selected = c("bra", "panties"),
                multiple = TRUE)
  })
  
  output$colorOutput <- renderUI({
    selectInput('colorInput', 'Select Product Color',
                sort(unique(df$color_group)),
                selected = c("red", "dragonfruit", "bright marine"),
                multiple = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

