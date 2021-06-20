## load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)
library(dplyr)
library(janitor)
library(forcats)
#read data
covid <- read_csv("AH_Cumulative_Provisional_COVID-19_Death_Counts_by_Place_of_Death_and_Age_Group_from_2_1_2020_to_7_18_2020.csv")
covid %>%
  clean_names()
covid

# ui User Interface

ui <- fluidPage(
  
tags$a(href = "https://catalog.data.gov/dataset/provisional-covid-19-death-counts-by-place-of-death-and-age-group-57294/resource/aa19e359-06bc-4b9b-ad31-ddee01b390fe", "Find the source of data here."),
  navbarPage(title = "comparison",
             tabPanel("Covid death",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "state",
                            label = "Select state",
                            choices = covid$State
                          )
                        ),
                        mainPanel(
                          plotlyOutput(outputId = "death_plot")
                        )
                      )),
             tabPanel("Place of death", 
                   sidebarLayout(
                     sidebarPanel(
                       selectInput(
                         inputId = "age",
                         label = "Select age group",
                         choices = covid$Age_group),
                       
                       selectInput(
                         inputId = "state2",
                         label = "Select State",
                         choices = covid$State
                 )
               ),
               mainPanel(
                 plotlyOutput(outputId = "percent")
               )
             )))
)


# Server
server <- function(input, output) {
  output$death_plot <- renderPlotly({
    p <- covid %>%
      filter(State == input$state) %>%
      group_by(Age_group) %>%
      ggplot(aes(x = All_Deaths_involving_COVID_19, y = Age_group)) +
      geom_col(na.rm = TRUE, fill = "red", alpha = 0.7) +
      labs(
        title = glue("Cumulative death of, {input$state}, by age group"), x = "All Deaths involving COVID 19",
        y = "Age group"
      ) +
      theme_minimal()
  })
  
  output$percent <- renderPlotly({
    d <- covid %>%
      filter(Age_group == input$age) %>%
      filter(State == input$state2) %>%
      mutate(covidpercent = (All_Deaths_involving_COVID_19/Deaths_from_All_Causes)*100 ) %>%
      group_by(Place_of_death) %>%
      ggplot(aes(x = covidpercent, y = Place_of_death)) +
      geom_col(na.rm = TRUE, fill = "red", alpha = 0.7) +
      labs(
        title = glue("Place of death {input$Age} by age group"), x = "COVID 19 death percent of all death"
      ) +
      theme_minimal()
  })
}

# Shiny App


shinyApp(ui, server)
