library(tidyverse)
library(primer.data)
library(shinythemes)
library(shiny)
library(ggthemes)


# read in cleaned data 
# full data manipulation and wrangling can be found in my repo in the "exploreextract" r script

access_trends <- read_rds(file = "clean_data/access_trends.rds")
median_costs <- read_rds(file = "clean_data/median_costs.rds")

# Define UI for application 

ui <- fluidPage(theme = shinytheme("superhero"),
    
    navbarPage("Healthcare Costs and Barriers to Care in the US",
    
    # First tab starts here
               
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
                 
    # sidebar that defines all the variables 
    
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
        
    # Second tab starts here 
    
        tabPanel("Cost",
                 h3("How has the cost of care changed over time?"),
                 p("Following are the key variables examined in the plot below:"),
                 p("Direct Pay captures the sum of direct payments for care provided during the year
                    including out-of-pocket payments and payments by private insurance, Medicaid, Medicare, and other sources."),
                 p("Total Pay is the sum of fully established charges for care received during the year, excluding those for prescribed medicines."),
                 p("Self Pay captures the total annual out-of-pocket expenses not including insurance or medications."),
                 
                 br(),
                 
                 selectInput(inputId = "type",
                             label = "Select a payment type to see it in the plot below:",
                             choices = c(unique(median_costs$type)),
                             selected = "self_pay",
                             multiple = TRUE),
                 plotOutput("median_plot")),
        
    # Third tab starts here 
    
        tabPanel("Model",
                 h3("What is the relationship between cost and specific access barriers?"),
                 p("I merged cost and access datasets and designed a predictive model to investigate how different barriers to care
                   impact total annual health care costs. The model presented here targets geographic barriers to care. 
                   I discovered the most significant predictive correlation between the variables 'far' and 'where'. My results suggest that 
                   individuals who cite being too far as the reason for not having a usual source of care will have a total cost of care
                   that is 25% higher than average, on an annual basis. Those who do not know where to go for care will have 15% higher costs on average.
                   "),
                
                 h4("Regression Model Equation:"),
                
                 withMathJax('$$ (log)total_i = \\beta_0 + \\beta_1selfpay_i + 
                        \\beta_2where_i + \\beta_3docmoved_i + \\beta_4far_i +
                        \\beta_5directpay*docmoved_i + 
                           \\varepsilon_i $$'),
              #   gt_output("table1"),
                br(),
              
                  h4("Table for Predictive Regression Model"),
                  img(src = "tbl_fit_4.png", height = "40%", width = "40%",
                      style = "display: block; margin-left: auto; margin-right: auto;"),
                 
                 br(),
                 br(),
                 h4("Posteriors of Geographic Barriers to Care"),
                 br(),
                 br(),
                 img(src = "fit_4_posterior_whereYes.png", height = "40%", width = "40%",
                     style = "display: block; margin-left: auto; margin-right: auto;"),
                 br(),
                 br(),
                 br(),
                 img(src = "fit_4_posterior_farYes.png", height = "40%", width = "40%",
                     style = "display: block; margin-left: auto; margin-right: auto;"),
                 
              ),
        
    # fourth tab starts here
        
        tabPanel("About", 
                 h3("Motivation"),
                 p("Communities with difficulty accessing medical services have higher morbidity rates. Which barriers to care have the greatest impact? Do these same communities end up paying more for care? 
                 What is the correlation between specific barriers, and out-of-pocket expenditures or total health care costs? These were a few of the questions
                 which motivated this investigation."),
                 p("Much work remains to address the social determinants of health in the United States, and to 
                   design a system that enables equitable access to health care services. Although this analysis stops short of 
                   suggesting a specific course of action, my hope is that the data surfaced here will encourage further interrogation
                   into the relationship between economic factors and barriers to care."),
                 h4("About the Data"),
                 p("The data for this project was sourced from the Medical Expenditure Panel Survey (MEPS), which 
                   provides harmonized microdata from a longitudinal survey of U.S. health care expenditures and utilization.
                   "),
                 p("Source: Lynn A. Blewett, Julia A. Rivera Drew, Risa Griffin and Kari C.W. Williams. IPUMS Health Surveys: Medical Expenditure Panel Survey, Version 1.1 [dataset]. Minneapolis, MN: IPUMS, 2019."),
                 uiOutput("link"),
                 br(),
                 sidebarPanel(
                     h4("About Me"),
                     p(strong("Trevor Cobb, Master of Design Engineering, Harvard")),
                     p("I'm a product manager, design strategist and venture builder. I blend human-centered-design and qualitative insight with 
                       data science to identify opportunities for impactful innovation."))
                 )))
)

# define server side logic 

server <- function(input, output) {
    
    output$link <- renderUI({
        tags$a(href="https://github.com/t-cobb/healthcareaccess", "Find the data and source code for this project in my GutHub repo:")
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
