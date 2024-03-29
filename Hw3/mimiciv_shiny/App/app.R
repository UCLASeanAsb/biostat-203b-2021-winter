#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(tidyverse)
icu_coh <-readRDS(str_c("/home/asburysean/biostat-203b-2021-winter/"
                        ,"Hw3/mimiciv_shiny/icu_cohort.rds"))
# Define UI for application that draws a boxplot
ui <- fluidPage(tabsetPanel(
    tabPanel("Race & Lab Values",
             # Application title
             # Sidebar with a slider input for number of bins
             sidebarLayout(
                 sidebarPanel(
                     # X var
                     checkboxGroupInput("eth_var",
                                        label =  "Ethnicty",
                                        choices = c("BLACK/AFRICAN AMERICAN", 
                                                    "UNKNOWN","WHITE", 
                                                    "HISPANIC/LATINO",
                                                    "ASIAN", "OTHER",
                                                    "UNABLE TO OBTAIN",
                                               "AMERICAN INDIAN/ALASKA NATIVE"),
                                        selected = "WHITE"),
                     # Y var
                     selectInput("lab_var",
                                 label = "Lab Value",
                                 choices = c( "bicarbonate", "chloride", 
                                              "creatinine", "glucose", 
                                              "magnesium", "potassium", 
                                              "sodium", "hematocrit", "wbc",
                                              "lactate", "heart_rate",
                                         "non_invasive_blood_pressure_systolic",
                                             "non_invasive_blood_pressure_mean",
                                              "respiratory_rate",
                                              "temperature_fahrenheit",
                                             "arterial_blood_pressure_systolic",
                                              "arterial_blood_pressure_mean"),
                                 selected = "wbc")),
                 # Histogram of all variables
                 mainPanel(plotOutput("labPlot"),
                           verbatimTextOutput("summary"))
                 )
    ),
    tabPanel("All data points",
             # X variable
             selectInput("x_var",
                         label = "Data Categories",
                         choices = c("Admission", "Demographics",
                                     "Lab Data"),
                         selected = "Admission"),
             selectInput("y_var",
                         label = "Data point of interst",
                         choices = c("first_careunit", "last_careunit",
                                     "age_adm", "intime", "outtime", "los",
                                     "admittime", "dischtime",
                                     "admission_type", "admission_location",
                                     "discharge_location","edregtime",
                                     "edouttime", "insurance", "language",
                                     "marital_status", "ethnicity", "gender",
                                     "anchor_age", "anchor_year", "bicarbonate",
                                     "anchor_year_group", "dod",
                                     "chloride", "creatinine",
                                     "glucose", "magnesium",
                                     "potassium", "sodium",
                                     "hematocrit", "wbc",
                                     "lactate", "heart_rate",
                                     "non_invasive_blood_pressure_systolic",
                                     "non_invasive_blood_pressure_mean",
                                     "respiratory_rate",
                                     "temperature_fahrenheit"),
                         selected = "first_careunit"),
             mainPanel(plotOutput("histoplot"),
                       verbatimTextOutput("summary2")))
))
# Define server logic required to draw a boxplot
server <- function(input, output, session) {
    output$summary <- renderPrint({
        summary(icu_cohort[[input$lab_var]])
 })
    
    output$summary2 <- renderPrint({
      if(input$y_var %in% c("first_careunit", "last_careunit",
                            "insurance", "language", "marital_status",
                            "ethnicity", "gender",
                            "admission_type", "admission_location",
                            "discharge_location")){
        table(icu_cohort[[input$y_var]])
      } else {
        summary(icu_cohort[[input$y_var]])
      }
    })
    
    
observeEvent(input$x_var, {
    if (input$x_var=="Admission") {
        updateSelectizeInput(session, input = "y_var",
                                 choices = c("first_careunit", "last_careunit",
                                             "age_adm", "intime",
                                             "outtime", "los",
                                             "admittime", "dischtime",
                                              "admission_type",
                                             "admission_location",
                                             "discharge_location",
                                             "edregtime", "edouttime"))
        }
    else if (input$x_var=="Demographics"){
        updateSelectizeInput(session, input = "y_var",
                              choices = c("insurance", "language",
                                          "marital_status", "ethnicity", 
                                          "gender", "anchor_age", 
                                          "anchor_year"))
        }
    else if (input$x_var=="Lab Data") {
        updateSelectizeInput(session, input = "y_var",
                            choices = c("bicarbonate",
                                        "anchor_year_group", "dod",
                                        "chloride", "creatinine",
                                        "glucose", "magnesium",
                                        "potassium", "sodium",
                                        "hematocrit", "wbc",
                                        "lactate", "heart_rate",
                                        "non_invasive_blood_pressure_systolic",
                                        "non_invasive_blood_pressure_mean",
                                        "respiratory_rate",
                                        "temperature_fahrenheit"))
        }
    })
output$labPlot <- renderPlot({
      icur <- subset(icu_coh, ethnicity==input$eth_var)
      ggplot(icur, aes(x=ethnicity, color=ethnicity)) +
      geom_boxplot(aes_string(y=input$lab_var)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
output$histoplot <- renderPlot({
    if(input$y_var %in% c("first_careunit", "last_careunit", "insurance", 
                          "language", "marital_status", "ethnicity", "gender", 
                          "admission_type", "admission_location", 
                          "discharge_location")) 
        {
          ggplot(data = icu_coh,mapping = aes_string(x = input$y_var)) +
          geom_bar(stat = "count") +
          geom_text(stat = 'count', aes(label=..count..), vjust=0) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
    else if(input$y_var %in% c("bicarbonate",
                                "anchor_year_group", "dod",
                                "age_adm", "intime",
                                "outtime", "los", 
                                "admittime", "dischtime",
                                "edregtime", "edouttime",
                                "chloride", "creatinine",
                                "glucose", "magnesium",
                                "potassium", "sodium",
                                "hematocrit", "wbc",
                                "lactate", "heart_rate",
                                "non_invasive_blood_pressure_systolic",
                                "non_invasive_blood_pressure_mean",
                                "respiratory_rate", 
                                "temperature_fahrenheit",
                                "anchor_age", "anchor_year"))
    {
      ggplot(data = icu_coh,mapping = aes_string(x = input$y_var)) +
      geom_boxplot(aes_string(y=input$y_var)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
        }
    })
}
# Run the application
shinyApp(ui = ui, server = server)