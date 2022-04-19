#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(purrr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(shinydashboard)
library(readxl)

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "Rocky shore data"),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
            uiOutput('species')
    ),

    # Show a plot of the generated distribution
    dashboardBody(
       plotlyOutput("freqPlotly")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # data <- tibble(
    #         zone = c("Lower", "Lower", "Middle", "Middle", "Upper", "Upper", "Upper"),
    #         species = c("species_a","species_a","species_a", "species_a","species_a","species_a", "species_a"),
    #         frequency = c(7, 8, 4, 5, 2, 1, 8)
    #     )
    data_path <- "./data/Day2RockyShoreTransect.xlsx"
    data <- read_excel(data_path, sheet = 2) %>% 
        mutate(zone = factor(zone, levels = c("Lower", "Middle", "Upper", "Splash")))
    
    species_names <- reactive({
        data$species
    })
    
    output$species <- renderUI({
            selectInput('species', 'Select species', species_names(), selected = 1)
        })
    
    output$freqPlotly <- renderPlotly({
            if (!is_empty(input$species)){
                
                subtitle_species <- unique(data$CommonName[data$species == input$species])
                plot <- data %>% 
                    filter(species == input$species) %>% 
                    ggplot(aes(zone, frequency, group=1)) +
                    stat_summary(fun.y = sum, geom = "line") +
                    geom_point(stat = 'summary', fun.y = sum, size = 5, aes(colour = zone)) +
                    scale_colour_viridis_d(direction = -1,
                                           option = "C") +
                    scale_y_continuous(limits = c(0,13)) +
                    labs(title = input$species,
                         x = "Zone",
                         y = "Frequency") +
                    theme_pubr() +
                    theme(
                        plot.title = element_text(hjust = 0.5, face="bold.italic"),
                        axis.title = element_text(face="bold"),
                        legend.position = "none"
                        )
                ggplotly(plot) %>% # Adding title and subtitle here
                    layout(title = list(text = paste0(input$species,
                                                      '<br>',
                                                      '<sup>',
                                                      subtitle_species,
                                                      '</sup>')))
            }
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
