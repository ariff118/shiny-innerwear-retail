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

df <- read_csv("cleaned_shiny.csv")

#===================#

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Innerwear and Swimwear E-commerce Market"),
   
   # Features
   sidebarLayout(
     sidebarPanel(
       uiOutput('brandOutput'),
       checkboxInput("allbrandInput", "Include all brands", value = TRUE),
       
       uiOutput('categoryOutput'),
       checkboxInput("allcategoryInput", "Include all categories", value = FALSE),
       
       uiOutput('colorOutput'),
       
       sliderInput("ratingInput", "Average Rating Range",
                   min = 1,
                   max = 5,
                   value = c(2, 5), sep = ""),
       
       sliderInput("priceInput", "Price Range",
                   min = 1,
                   max = 620,
                   value = c(50, 100), sep = "")
       
       # radioButtons("orderbyInput", "Order by",
       #              choices = c("Brand", "Price", "Percentage Sales"))
       
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       plotOutput("total_product"),
       br(), br(),
       plotOutput("price_range"),
       br(), br(),
       plotOutput("average_rating"),
       br(), br(),
       # plotOutput("total_reviews"),
       # br(), br(),
       dataTableOutput("filtered_data")
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # set renderUI for interactive input selection
  output$brandOutput <- renderUI({
    selectInput('brandInput', 'Select Brand',
                sort(unique(df$brand)),
                selected = c("Amazon-Wacoal", "Macys-Wacoal", "Macys-Calvin Klein", "Amazon-Calvin Klein"),
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
                selected = c("red", "black", "nude", "multiple colors"),
                multiple = TRUE)
  })
  
  
  # filter data for visualization
  filtered <- reactive({
    if(input$allbrandInput){
      df %>% 
        filter(category %in% c(input$categoryInput),
               color_group %in% c(input$colorInput),
               rating >= input$ratingInput[1],
               rating <= input$ratingInput[2],
               price_converted >= input$priceInput[1],
               price_converted <= input$priceInput[2])
    }
    else if (input$allcategoryInput){
      df %>% 
        filter(brand %in% c(input$brandInput),
               color_group %in% c(input$colorInput),
               rating >= input$ratingInput[1],
               rating <= input$ratingInput[2],
               price_converted >= input$priceInput[1],
               price_converted <= input$priceInput[2])
    }
    else if ( (input$allcategoryInput) & (input$allbrandInput)){
      df %>% 
        filter(color_group %in% c(input$colorInput),
               rating >= input$ratingInput[1],
               rating <= input$ratingInput[2],
               price_converted >= input$priceInput[1],
               price_converted <= input$priceInput[2])
    }
    else {
      df %>% 
        filter(brand %in% c(input$brandInput),
               category %in% c(input$categoryInput),
               color_group %in% c(input$colorInput),
               rating >= input$ratingInput[1],
               rating <= input$ratingInput[2],
               price_converted >= input$priceInput[1],
               price_converted <= input$priceInput[2])
    }
  })
  
  ar <- reactive({
    filtered() %>% 
    group_by(brand) %>%
    summarise(avg_rating = mean(rating))
  })
  
  output$total_product <- renderPlot(
    {p1 <- ggplot(filtered(),
                 aes(brand, fill = brand)) +
          geom_bar() + 
          labs(title = "Total Product Per Brand",
               y = "products") +
          scale_color_brewer(name = "", palette = "Set1") + 
          coord_flip() + 
          theme_bw()
    return(p1)
    }
  )
  
  output$price_range <- renderPlot(
    {p2 <- ggplot(filtered(),
                 aes(brand, price_converted, color = brand)) +
      geom_boxplot() + 
      labs(title = "Price Range",
           y = "USD") + 
      scale_color_brewer(name = "", palette = "Set1") + 
      coord_flip() + 
      theme_bw()
    return(p2)
    }
  )
  
  output$average_rating <- renderPlot(
    {p3 <- ggplot(ar(), aes(brand, avg_rating, fill = brand)) +
          geom_bar(stat = "identity") +
          scale_color_brewer(name = "", palette = "Set1") +
          labs(title = "Average Rating", y = "average rating") + 
          coord_flip() + 
          theme_bw()
     return(p3)
    }
  )
  
  output$filtered_data <- renderDataTable(filtered(),
                                          options = list(pageLength = 5))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

