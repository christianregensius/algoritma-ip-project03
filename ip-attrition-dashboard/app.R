options(scipen = 1)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(glue)
library(dplyr)
library(tidyverse)
library(scales)


#### Read Data ####
data_attrition <- read_csv("data_input/data-attrition.csv")
# glimpse(data_attrition)
# head(data_attrition)

career_per_dept <- data_attrition %>%
  mutate(mutation_rate = years_in_current_role/years_at_company, 
         promotion_rate = years_since_last_promotion/years_at_company, 
         mutation_rate = replace_na(mutation_rate, 0), 
         promotion_rate = replace_na(promotion_rate, 0), 
         percent_salary_hike = percent_salary_hike/100)

#### Web Dashboard Component ####

ui <- fluidPage(
  dashboardPage(
    title = "Dashboard Attrition",
    skin = "blue",
    dashboardHeader(
      title = "Company Attrition Status"
    ),
    dashboardSidebar(
      sidebarMenu(
        
        menuItem(text = "Chart", 
                 tabName = "chart", 
                 icon = icon(name = "chart-area", 
                             lib = "font-awesome")),
        
        
        menuItem(text = "Data Table",
                 tabName = "data",
                 icon = icon(name = "user", 
                             lib = "font-awesome"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          # First tab
          tabName = "chart",
          fluidRow(
            box(
              title = "Choose your preferences", 
              status = "primary", 
              width = 4,
              height = "500",
              solidHeader = TRUE,
              selectInput(inputId = "dept", 
                          label = "Choose department:",
                          choices = unique(career_per_dept$department), 
                          selected = "sales"),
              
              sliderInput(inputId = "monthly_income",
                          label = "Minimum monthly income?",
                          min = 0,
                          max = max(career_per_dept$monthly_income),
                          value = max(career_per_dept$monthly_income))
            ),
            box(
              title = "Plot Karir",
              width = 4, 
              height = "500",
              status = "primary",
              solidHeader = TRUE,
              plotlyOutput(outputId = "plot_career")
            ),
            
            box(
              title = "Plot Summary",
              width = 4, 
              height = "500",
              status = "primary",
              solidHeader = TRUE,
              plotlyOutput(outputId = "plot_summary")
            )
          )
        ),
        
        #Second Tab
        tabItem(
          tabName = "data",
          fluidRow(
            box(
              dataTableOutput(outputId = "table_career"),
              width = 10
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$plot_career <- renderPlotly({
    career_dynamic <- career_per_dept %>% 
      filter(department == input$dept) %>%
      filter(monthly_income <= input$monthly_income)
    
    plot_career <- ggplot(career_dynamic, 
                          aes(x=career_dynamic$mutation_rate,
                              y=career_dynamic$promotion_rate,
                              text = paste("Mutation Ratio:", mutation_rate, "<br>",
                                           "Promotion Ratio:", promotion_rate))) +
      geom_point()
    ggplotly(plot_career, tooltip = "text")
  })
  
  output$plot_summary <- renderPlotly({
    
    summary_attrition <- data_attrition %>% 
      group_by(department, attrition) %>% 
      summarise(employee_count = sum(employee_count)) %>% 
      filter(department==input$dept)
    
    plot_summary <- ggplot(summary_attrition, 
                           aes(x=reorder(summary_attrition$department, summary_attrition$employee_count), 
                               y=summary_attrition$employee_count)) +
      geom_col() +
      labs(x = "Department",
           y = "Number of employee")+
      geom_text(aes(label=summary_attrition$employee_count), 
                nudge_y = 0.05*max(summary_attrition$employee_count))
  })
  %>% 
  output$table_career <- renderDataTable({
    datatable(career_per_dept, options = list(lengthMenu = c(6, 10, 20),
                                   scrollX = TRUE))
  })
  
}

shinyApp(ui, server)