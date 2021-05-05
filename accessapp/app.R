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
        tabPanel("Access",
                 h3("Which barriers to care impact the most people?"),
                 p("Data From The Medical Expenditure Panel Survey (MEPS)", 
                   style = "font-size:18px;"),
                 
                 br(),
            
                 mainPanel(
                     selectInput(inputId = "barrier",
                                 label = "Select a barrier to care to see it in the plot below:",
                                 choices = c(unique(access_trends$barriers)),
                                 selected = c("language", "far"),
                                 multiple = TRUE),
                     plotOutput("access_plot")),
                 
                 sidebarPanel(
                     h4("Why no usual source of care?"),
                     p("Noinsurance: No health insurance"),
                     p("Language: Speak a different language"),
                     p("Where: Doesn't know where to go"),
                     p("Far: Care too far away or inconvenient"),
                     p("Doc_moved: Previous doctor moved or is unavailable"),
                     p("Dislike_doc: Doesn't like doctors"),
                     p("Noneed_doc: Doesn't need doctor"),
                     p("Jobrelated: Reason related to job"),
                     p("Other: Other reason given"))
                     
                     ),
        
        tabPanel("Cost",
                 h3("How has the cost of care changed over time?"),
                 p("Mapping out-of-pocket expenses, direct payments
                   and total healthcare expenditures per person.",
                   style = "font-size:18px;"),
                 
                 br(),
                 
                 selectInput(inputId = "type",
                             label = "Select a payment type to see it in the plot below:",
                             choices = c(unique(median_costs$type)),
                             selected = "self_pay",
                             multiple = TRUE),
                 plotOutput("median_plot")),
        
        tabPanel("Model",
                 h3("Examining the relationship between cost and access"),
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
        
        
        tabPanel("About", 
                 h3("Motivation"),
                 p("Communities with difficulty accessing medical services have higher morbidity rates. Which barriers to care impact the most people? Do these same communities end up paying more for care? 
                 What is the correlation between specific barriers and out-of-pocket expenditures or total health care costs? These were a few of the questions
                 which motivated this investigation."),
                 p("There is much work to be done to address the social determinants of health in the United States, and to 
                   deisgn a system that enables equitable access to health care services. Although this analysis stops short of 
                   suggesting a specific course of action, my hope is that the data surfaced here will encourage further interrogation
                   into the relationship between financial incentives and barriers to care."),
                 h4("About the Data"),
                 p("The data for this project was sourced from the Medical Expenditure Panel Survey (MEPS), which 
                   provides harmonized microdata from a longitudinal survey of U.S. health care expenditures and utilization.
                   "),
                 p("Source: Lynn A. Blewett, Julia A. Rivera Drew, Risa Griffin and Kari C.W. Williams. IPUMS Health Surveys: Medical Expenditure Panel Survey, Version 1.1 [dataset]. Minneapolis, MN: IPUMS, 2019."),
                 uiOutput("link"),
                 br(),
                 sidebarPanel(
                     h4("About"),
                     p(strong("Trevor Cobb, Master of Design Engineering, Harvard")),
                     p("I'm a product manager, design strategist and venture builder. I blend human-centered-design and qualitative insight with 
                       data science to identify opportunities for impactful innovation."))
                 )))
)

# define server side logic 

server <- function(input, output) {
    
    output$link <- renderUI({
        tags$a(href="https://github.com/t-cobb/healthcareaccess", "Find the data and source code for this project in my Guthub repo:")
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
