
load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"))
#movies_codebook <- read_csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies_codebook.csv")
lib2 <- c('stringi','stringr','hash','reshape','plyr','dplyr','tidyr',
          'gridExtra','data.table','lattice','latticeExtra',
          'ggplot2','sqldf','haven','readr','shiny','DT','tools')

if (length(setdiff(lib2, rownames(installed.packages()))) > 0) {
  suppressMessages(suppressPackageStartupMessages((install.packages(setdiff(lib2, rownames(installed.packages())),repos = "http://cran.us.r-project.org"))))
}

# set  working directory
if(!getwd()=="O:\\Projects\\Multiple_Diseases\\DataVisualization_Song\\02Data")
  setwd("O:\\Projects\\Multiple_Diseases\\DataVisualization_Song\\02Data")

#load library


library(shiny)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(haven)

daily <- read_sas('.\\sas\\dayvalue.sas7bdat')
#head(daily)

day35 <- daily[-2<=daily$day & daily$day<=35,]

zvalue <- day35 %>% group_by(cate) %>% mutate(zvalue=scale(daily_value))



# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # App title
  titlePanel("AML patient vital/Lab", windowTitle = "AML"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      h3("Plotting"),      # Third level header: Plotting
      
      # Select variable for x-axis 
      selectInput(inputId = "x", 
                  label = "x-axis:",
                  choices = c("Days from intervention" = "day"), 
                
                  selected = "day"),
      
      # Select variable for y-axis 
      selectInput(inputId = "y", 
                  label = "Values seletion:",
                  choices = c("Original values" = "daily_value", 
                              "Standardized values" = "zvalue"
                         ), 
                  selected = "daily_value"),
      br(),
      
      selectInput(inputId = "facet", 
                  label = "present by: :",
                  choices = c("Study id" = "study_id", 
                              "Category" = "cate"
                  ), 
                  selected = "study_id"),
   
      
      # Enter text for plot title
     # textInput(inputId = "plot_title", 
      #          label = "Plot title", 
      #          placeholder = "Enter text to be used as plot title"),
      
      hr(),
      
      h3("Selections"),    # Third level header: Subsetting
      
      # Select which types of movies to plot
      checkboxGroupInput(inputId = "selected_type",
                         label = "Select Lab/Vital:",
                        choices = c('ALT' = "alt", 
                                                'Creatinine' = "cr", 
                                                'WBC' = "wbc", 
                                                'Temperature' = "temperature", 
                                                'Bilirubin' = "bil",
                                                'Heart rate'='heart_rate',
                                                'Respiratory rate' ='resp_rate',
                                                'Sodium'='na',
                                                'Platelets'='plt'
                         ),
                         selected = "wbc"),
      
      hr(),
      
      # Show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      # Built with Shiny by RStudio
      br(), br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         ".")
      
    ),
    
    # Output:
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  id = "tabsetpanel",
                  tabPanel(title = "Plot", 
                           plotOutput(outputId = "lineplot"),
                           br(),
                           h5(textOutput("description"))),
                  tabPanel(title = "Data", 
                           br(),
                           DT::dataTableOutput(outputId = "moviestable")),
                  tabPanel("Codebook", 
                           br(),
                           dataTableOutput(outputId = "codebook"))
                  
      )
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected title types
  value_selected <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    req(input$y)
  
    filter(zvalue, cate %in% input$selected_type)

 
    })
  
  #value_selected <- rename(zvalue,value=input$y)
 
   
  
  # x and y as reactive expressions
 # x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  yr <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  # Create scatterplot object the plotOutput function is expecting 
  output$lineplot <- renderPlot({
  ggplot(value_selected(), aes_string(x = input$x, y=input$y)) +
      
      geom_line(aes(group=cate,colour=cate)) + 
      labs(y=yr()) +
 
    facet_wrap(.~study_id)
  })
 
  
  # Print data table if checked
  output$moviestable <- DT::renderDataTable(
    {
      DT::datatable(data = value_selected()[, 1:5], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  
  # Display data table tab only if show_data is checked
  observeEvent(input$show_data, {
    if(input$show_data){
      showTab(inputId = 'tabsetpanel', target = 'Data' , select = T)
    } else {
      hideTab(inputId = 'tabsetpanel', target = 'Data')
    }
  })
  
  # Render data table for codebook
  output$codebook <- renderDataTable({
    datatable(data = zvalue,
              options = list(pageLength = 10, lengthMenu = c(10,15,10,10,10)), 
              rownames = FALSE)
  })
  
}

# Create Shiny app object
shinyApp(ui = ui, server = server)