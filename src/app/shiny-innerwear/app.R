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
df <- df %>% 
  rename(mrp = mrp_converted, price = price_converted, color = color_group)

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
                   max = 300,
                   value = c(50, 100), sep = "")
       
       # radioButtons("orderbyInput", "Order by",
       #              choices = c("Brand", "Price", "Percentage Sales"))
       
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       downloadButton("tot_prod_graph", "Download graph"),
       plotOutput("total_product"),
       br(), br(),
       downloadButton("price_graph", "Download graph"),
       plotOutput("price_range"),
       br(), br(),
       downloadButton("rating_graph", "Download graph"),
       plotOutput("average_rating"),
       br(), br(),
       # plotOutput("total_reviews"),
       # br(), br(),
       downloadButton("download_data", "Download data"),
       dataTableOutput("filtered_data")
     )
   )
)


#===================#

# Define server logic required to draw a histogram
server <- function(input, output){
  
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
                sort(unique(df$color)),
                selected = c("red", "black", "nude", "multiple colors"),
                multiple = TRUE)
  })
  
  
  # filter data for visualization
  filtered <- reactive({
    # if(input$allbrandInput){
    #   df %>% 
    #     filter(category %in% c(input$categoryInput),
    #            color_group %in% c(input$colorInput),
    #            rating >= input$ratingInput[1],
    #            rating <= input$ratingInput[2],
    #            price >= input$priceInput[1],
    #            price <= input$priceInput[2])
    # }
    # else if (input$allcategoryInput){
    #   df %>% 
    #     filter(brand %in% c(input$brandInput),
    #            color_group %in% c(input$colorInput),
    #            rating >= input$ratingInput[1],
    #            rating <= input$ratingInput[2],
    #            price >= input$priceInput[1],
    #            price <= input$priceInput[2])
    # }
    # else if ( (input$allcategoryInput) & (input$allbrandInput)){
    #   df %>% 
    #     filter(color_group %in% c(input$colorInput),
    #            rating >= input$ratingInput[1],
    #            rating <= input$ratingInput[2],
    #            price >= input$priceInput[1],
    #            price <= input$priceInput[2])
    # }
    # else {
      df %>% 
        filter(brand %in% c(input$brandInput),
               category %in% c(input$categoryInput),
               color %in% c(input$colorInput),
               rating >= input$ratingInput[1],
               rating <= input$ratingInput[2],
               price >= input$priceInput[1],
               price <= input$priceInput[2])
    # }
  })
  
  # when choose "Include all categories", update SelectInput to include all categories
  # observe({
  #   if (input$allcategoryInput){
  #     updateSelectInput(session,
  #                       inputId = "categoryInput",
  #                       sort(unique(df$category)),
  #                       selected = c(unique(df$category))
  #                       )
  #   }
  # })
  # 
  # when choose "Include all brands", update SelectInput to include all brands
  # observe({
  #   if (input$allbrandInput){
  #     updateSelectInput(session,
  #                       inputId = "brandInput",
  #                       sort(unique(df$brand)),
  #                       selected = c(unique(df$brand))
  #                       )
  #   }
  # })
  
  # create graph for total options
  tot <- reactive({
    filtered() %>% 
      group_by(brand, category) %>%
      summarise(tot_product = n())
  })
  
  totprodplot <- reactive(
    {p1 <- ggplot(tot(),
                 aes(brand, fill = category)) +
          geom_bar() + 
          labs(title = "Total Options Per Brand",
               y = "options") +
          # scale_color_brewer(name = "", palette = "Set1") + 
          coord_flip() + 
          theme_bw()
    return(p1)
    }
  )
  
  output$total_product <- renderPlot(
    {print(totprodplot())}
  )
  
  # create graph for price range
  priceplot <- reactive(
    {p2 <- ggplot(filtered(),
                 aes(brand, price, group = category, color = category)) +
      geom_boxplot() + 
      labs(title = "Price Range",
           y = "USD") + 
      # scale_color_brewer(name = "", palette = "Set1") + 
      coord_flip() + 
      theme_bw()
    return(p2)
    }
  )
    
  output$price_range <- renderPlot(
    {print(priceplot())}
  )
  
  # create graph for average rating
  ar <- reactive({
    filtered() %>% 
      group_by(brand) %>%
      summarise(avg_rating = mean(rating))
  })
  
  avgplot <- reactive(
    {p3 <- ggplot(ar(), aes(brand, avg_rating, fill = brand)) +
          geom_bar(stat = "identity") +
          scale_color_brewer(name = "", palette = "Set1") +
          labs(title = "Average Rating", y = "average rating") + 
          coord_flip() + 
          theme_bw()
     return(p3)
    }
  )
  
  output$average_rating <- renderPlot({
    print(avgplot())
  })
  
  output$filtered_data <- renderDataTable(filtered(),
                                          options = list(pageLength = 5))
  
 
  # download graph and data
  ## total options graph
  output$tot_prod_graph <- downloadHandler(
    filename = "total_options_per_brand.png",
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 150, units = "in")
      ggsave(filename = file, plot = totprodplot(), device = device)
    }
  )
  
  ## price range graph
  output$price_graph <- downloadHandler(
    filename = "price_rangd_per_brand.png",
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 150, units = "in")
      ggsave(filename = file, plot = priceplot(), device = device)
    }
  )
  
  ## average rating graph
  output$rating_graph <- downloadHandler(
    filename = "avg_rating_per_brand.png",
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 150, units = "in")
      ggsave(filename = file, plot = avgplot(), device = device)
    }
  )
  
  ## dowload filtered data
  output$download_data <- downloadHandler(
    filename = "innerwear_filtered.csv",
    content = function(file) {
      write.csv(filtered(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

