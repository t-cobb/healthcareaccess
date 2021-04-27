library(tidyverse)
library(primer.data)
library(shinythemes)
library(shiny)


# source(file = "exploreextract.R")
# fit_4 <- read_rds("accessapp/clean_data/fit_4.rds")
# tbl_fit_4 <- read_rds("accessapp/clean_data/tbl_fit_4.rds")
# median_costs <- read_rds("accessapp/clean_data/median_costs.rds")

# Define UI for application 
ui <- navbarPage(
    
    tabsetPanel(
        tabPanel("Healthcare Expenditures in the US",
                 titlePanel("Cost of Care"),
                 plotOutput("median_plot")),
                 
        tabPanel("Model",
                 titlePanel(""),
                 gt_output("table1"),
                 h3("Posteriors of Geographic Barriers to Care"),
                 plotOutput("fit_4_posterior_whereYes"),
                 plotOutput("fit_4_posterior_farYes")),
        
        tabPanel("Discussion", 
                 titlePanel("Discussion"),
                 h3("This app is a work in progress"),
                 p("My name is Trevor Cobb, and I'm interested in working to fix the broken US healthcare system. But any reasonable attempt a problem solving must begin with a thorough
                   understanding of the challenges. This app explores barriers to care and longitudinal healthcare spending data in the US."),
                 uiOutput("link"))
        
    ))

# Define server logic required to draw a histogram

server <- function(input, output) {
    
    output$link <- renderUI({
        tags$a(href="https://github.com/t-cobb/healthcareaccess", "Here is the link to this repo")
    })
    
    output$table1 <- render_gt({
        tbl_fit_4
    })
    
    output$median_plot <- renderPlot({
        median_plot
    })
    
    output$fit_4_posterior_farYes <- renderPlot ({
        fit_4_posterior_farYes
    })
    
    output$fit_4_posterior_whereYes <- renderPlot ({
        fit_4_posterior_whereYes
    })    
}

# Run the application 
shinyApp(ui = ui, server = server)
