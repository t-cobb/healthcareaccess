library(tidyverse)
library(primer.data)
library(shinythemes)
library(shiny)
library(ggthemes)


# read in cleaned data 
# full data manipulation and wrangling can be found in my repo: "exploreextract" r script

access_trends <- read_rds(file = "clean_data/access_trends.rds")
median_costs <- read_rds(file = "clean_data/median_costs.rds")

# Define UI for application 

ui <- fluidPage(theme = shinytheme("superhero"),
    
    navbarPage("Healthcare Costs and Barriers to Care in the US",
    
    tabsetPanel(
        tabPanel("Access & Expenditures",
                 h2("Which barriers to care impact the most people?"),
                 p("Data From The Medical Expenditure Panel Survey (MEPS)", 
                   style = "font-size:18px;"),
                 
                 br(),
                 br(),
            
                 mainPanel(
                     selectInput(inputId = "barrier",
                                 label = "Select a barrier to care to see it in the plot below:",
                                 choices = c(unique(access_trends$barriers)),
                                 selected = c("language", "far"),
                                 multiple = TRUE),
                     plotOutput("access_plot")),
                 
                 # h2("How has the cost of care changed over time?"),
                 # p("Mapping out-of-pocket expenses, direct payments 
                 #   and total healthcare expenditures per person.",
                 #   style = "font-size:18px;"),
                 
                 br(),
                 br(),
                 
                     selectInput(inputId = "type",
                                 label = "Select a payment type to see it in the plot below:",
                                 choices = c(unique(median_costs$type)),
                                 selected = "self_pay",
                                 multiple = TRUE),
                     plotOutput("median_plot")),
                 
        tabPanel("Model",
                 titlePanel(""),
                 p(strong("Regression Model Equation:")),
                 withMathJax('$$ (log)total_i = \\beta_0 + \\beta_1selfpay_i + 
                        \\beta_2where_i + \\beta_3docmoved_i + \\beta_4far_i +
                        \\beta_5directpay*docmoved_i + 
                           \\varepsilon_i $$'),
                 br(),
                 br(),
              #   gt_output("table1"),
                  img(src = "tbl_fit_4.png", height = "60%", width = "60%",
                  style = "display: block; margin-left: auto; margin-right: auto;"),
                br(),
                 br(),
                 h4("Posteriors of Geographic Barriers to Care"),
                 br(),
                 br(),
                 img(src = "fit_4_posterior_whereYes.png", height = "60%", width = "60%",
                     style = "display: block; margin-left: auto; margin-right: auto;"),
                 br(),
                 br(),
                 br(),
                 img(src = "fit_4_posterior_farYes.png", height = "60%", width = "60%",
                     style = "display: block; margin-left: auto; margin-right: auto;")),
        
        
        tabPanel("Discussion", 
                 titlePanel("Discussion"),
                 h4("The Medical Expenditure Panel Survey (MEPS), the data source for this project, 
                   provides harmonized microdata from the longitudinal survey of U.S. health care expenditures and utilization.
                   "),
                 p("Source: Lynn A. Blewett, Julia A. Rivera Drew, Risa Griffin and Kari C.W. Williams. IPUMS Health Surveys: Medical Expenditure Panel Survey, Version 1.1 [dataset]. Minneapolis, MN: IPUMS, 2019.
                    https://doi.org/10.18128/D071.V1.1"),
                 uiOutput("link"),
                 br(),
                 br(),
                 br(),
                 sidebarPanel(
                     h3("About Me"),
                     h4("Trevor Cobb, MDE Candidate at Harvard GSD & SEAS"),
                     p("I'm interested in social determinants of health, and in working to fix the broken US healthcare system. 
                        But any reasonable attempt at problem solving must begin with
                        a thorough exploration of the challenges. 
                       This is part of my attempt to build the skills necessary to understand"))
                 
                 )))
)

server <- function(input, output) {
    
    output$link <- renderUI({
        tags$a(href="https://github.com/t-cobb/healthcareaccess", "Here is the link to this repo")
    })
    
    output$access_plot <- renderPlot(
        access_trends %>%
            filter(barriers %in% input$barrier) %>%
            ggplot(aes(x = YEAR, 
                       y = count, 
                       color = barriers)) +
            geom_point() +
            geom_line() +
            labs(title = "Specific Barriers to Healthcare Access",
                 subtitle = "'No Insurance' and 'No Doctor Needed' most reported",
                 x = NULL,
                 y = "People impacted",
                 color = "Barrier",
                 caption = "Source: IPUMS") +
            theme_light()
        
        )
    
    output$median_plot <- renderPlot(
        median_costs %>%
            filter(type %in% input$type) %>%
            ggplot(aes(x = YEAR, 
                       y = cost, 
                       color = type)) +
            geom_point() +
            geom_line() +
            labs(title = "Cost of Medical Care in the United States Per Person (Median)",
                 subtitle = "Healthcare costs continue to rise",
                 x = NULL,
                 y = "$USD",
                 color = "Type",
                 caption = "Source: IPUMS") +
            theme_light()
    )
    
    # output$table1 <- render_gt({
    #     tbl_fit_4
    # })
    # 
}

# Run the application 
shinyApp(ui = ui, server = server)
