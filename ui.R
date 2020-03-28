#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

all.choices <- list("Alaska" = "AK", "Alabama" = "AL", "Arkansas" = "AR","American Samoa" = "AS", "Arizona" = "AZ", 
                    "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "District of Columbia" = "DC",
                    "Delaware" = "DE", "Florida" = "FL", "Georgia" = "GA", "Guam" = "GU", "Hawaii" = "HI", "Iowa" = "IA",
                    "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN", "Kansas" = "KS", "Kentucky" = "KY", 
                    "Louisiana" = "LA", "Massachusetts" = "MA", "Maryland" = "MD", "Maine" = "ME", "Michigan" = "MI", 
                    "Minnesota" = "MN", "Missouri" = "MO", "Northern Mariana Islands" = "MP", "Mississippi" = "MS", 
                    "Montana" = "MT", "North Carolina" = "NC", "North Dakota" = "ND", "Nebraska" = "NE", "New Hampshire" = "NH", 
                    "New Jersey" = "NJ", "New Mexico" = "NM", "Nevada" = "NV", "New York" = "NY", "Ohio" = "OH", 
                    "Oklahoma" = "OK", "Oregon" = "OR", "Pennsylvania" = "PA", "Puerto Rico" = "PR", "Rhode Island" = "RI", 
                    "South Carolina" = "SC", "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", 
                    "U.S. Minor Outlying Islands" = "UM", "Utah" = "UT", "Virginia" = "VA", "U.S. Virgin Islands" = "VI",
                    "Vermont" = "VT", "Washington" = "WA", "Wisconsin" = "WI", "West Virginia" = "WV", "Wyoming" = "WY"
)

all.series <- list('Confirmed Positive' = 'positive', 
                   'Confirmed Negative' = 'negative', 
                   'Pending Tests' = 'pending', 
                   'Hospitalized' = 'hospitalized', 
                   'Deaths' = 'death', 
                   'Total Tests' = 'totalTestResults')

all.transformations <- list('None' = 'none', 
                            'Log 10' = 'log10', 
                            'Natural log' = 'log', 
                            'Square root' = 'sqrt')

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("COVID-19 State Tracker Dashboard"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            pickerInput("stateChoice", h3("States"), 
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE,
                        choices = all.choices,
                        selected = all.choices),
            pickerInput("highlightSet", h3("Highlight"), 
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE,
                        choices = all.choices,
                        selected = NULL),
            pickerInput("seriesChoice", h3("Data"), 
                        options = list(`actions-box` = TRUE),
                        choices = all.series,
                        selected = all.series[1]),
            pickerInput("transformation", h3("Transformation"), 
                        options = list(`actions-box` = TRUE),
                        choices = all.transformations,
                        selected = all.transformations[1])
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("casesPlot"),
            hr(),
            'Data from: http://covidtracking.com/'
        )
    )
))
