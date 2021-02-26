#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
icu_coh <- readRDS("/home/asburysean/biostat-203b-2021-winter/Hw3/mimiciv_shiny/icu_cohort.rds")
# Define UI for application that draws a scatterplot
ui <- fluidPage(

    # Application title
    titlePanel("ICU Cohort Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # X var
            checkboxGroupInput("eth_var",
                        label =  "Ethnicty",
                        choices = c("BLACK/AFRICAN AMERICAN", "UNKNOWN",                      
                                    "WHITE", "HISPANIC/LATINO",              
                                    "ASIAN", "OTHER",                        
                                    "UNABLE TO OBTAIN",
                                    "AMERICAN INDIAN/ALASKA NATIVE"),
                        selected = NULL),
            # Y var
            checkboxGroupInput("lab_var",
                               label = "Lab Value",
                               choices = c("chloride", "creatinine",
                                           "glucose", "magnesium",
                                           "potassium", "sodium",
                                           "hematocrit", "wbc",
                                           "lactate", "heart_rate",
                                         "non_invasive_blood_pressure_systolic",
                                           "non_invasive_blood_pressure_mean",
                                           "respiratory_rate",
                                           "temperature_fahrenheit",
                                           "arterial_blood_pressure_systolic",
                                           "arterial_blood_pressure_mean"),
                               selected = NULL)
            
        ),

        # Show a scatterplot
        mainPanel(plotOutput("labPlot")
        )
    ))


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$labPlot <- renderPlot({
       subset(icu_coh, ethnicity==input$eth_var)
        ggplot(icu_coh, aes(x=input$eth_var, y=input$lab_var, color=ethnicity)) +
            geom_boxplot()
        # generate bins based on input$bins from ui.R

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
