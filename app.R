#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(cowplot)
library(colorspace)
library(scales)
library(shinycssloaders)
library(sf)
library(albersusa)

options(spinner.color="#3e5fff")

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
                   'Daily Increase in Positives' = 'positiveIncrease',
                   'Confirmed Negative' = 'negative', 
                   'Daily Increase in Negatives' = 'negativeIncrease',
                   'Hospitalized' = 'hospitalized', 
                   'Daily Increase in Hospitalizations' = 'hospitalizedIncrease',
                   'Deaths' = 'death', 
                   'Daily Increase in Deaths' = 'deathIncrease',
                   'Total Tests' = 'totalTestResults',
                   'Daily Increase in Tests' = 'totalTestResultsIncrease',
                   'Pending Tests' = 'pending')

all.transformations <- list('None' = 'none', 
                            'Log10' = 'log10', 
                            'Natural log' = 'log', 
                            'Square root' = 'sqrt')

state.df <- read_csv('http://covidtracking.com/api/states/daily.csv')
state.df$dateChecked <- date(state.df$dateChecked)
state.df$date <- ymd(state.df$date)

us_sf <- usa_sf("laea")

colors <- hue_pal()(length(unique(state.df$state)))
colors.list <- list()
sapply(1:length(unique(state.df$state)), function(x) colors.list[unique(state.df$state)[x]] <<- colors[x])

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("COVID-19 US State Tracker Dashboard"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 4,
                     div(style = 'margin-top: -10px; margin-bottom: 0px',
                         fluidRow(
                             column(6,
                                    pickerInput("stateChoice", 
                                                h4("States"), 
                                                options = list(`actions-box` = TRUE),
                                                multiple = TRUE,
                                                choices = all.choices,
                                                selected = all.choices
                                    )
                             ),
                             column(6,
                                    pickerInput("highlightSet", 
                                                h4("Highlights"), 
                                                options = list(`actions-box` = TRUE),
                                                multiple = TRUE,
                                                choices = all.choices,
                                                selected = NULL)
                             )
                         ),
                         fluidRow(
                             column(6,
                                    pickerInput("seriesChoice", 
                                                h4("Data"), 
                                                options = list(`actions-box` = TRUE),
                                                choices = all.series,
                                                selected = all.series[1])),
                             column(6,
                                    pickerInput("transformation", 
                                                h4("Transform"), 
                                                options = list(`actions-box` = TRUE),
                                                choices = all.transformations,
                                                selected = all.transformations[1])
                             )
                         ),
                         fluidRow(
                             column(6,
                                    radioButtons("align", 
                                                 h4("Align"), 
                                                 choices = list('Yes' = T, 'No' = F),
                                                 selected = list('No' = F)
                                    )
                             ),
                             column(6,
                                    numericInput("num_align", 
                                                 h4("Align Number"), 
                                                 value = 0)
                             ) 
                         ),
                         fluidRow(
                             column(6,
                                    radioButtons("facet", 
                                                 h4("Facet"), 
                                                 choices = list('Yes' = T, 'No' = F),
                                                 selected = list('No' = F)
                                    )
                             )
                         ),
                         fluidRow(
                             column(12, align = 'center',
                                    actionButton("do", "Build Charts")
                             )
                         )
                     )
        ),
        # Show a plot of the generated distribution
        mainPanel(width = 8,
                  plotOutput("casesPlot", height = 475) %>% withSpinner()
        )
    ),
    hr(),
    
    plotOutput('mapPlot', height = 500) %>% withSpinner(),
    
    hr(),
    
    strong("Explanation:"),
    
    "To construct the plot and map click the 'Build Charts' button. 
    The 'Align' option will align each state with Day 0 as the 
    first day that each had at least 'Align Number' number of 
    the variable you selected. The 'Facet' option will split each 
    state into its own subplot; be careful with it because it could
    take some time to render. The map shows the most recent day's data.
    The transformation you choose will be applied to both the plot and the map.
    Data from: http://covidtracking.com/"
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    inputData <- eventReactive(input$do, {
        these.data = list(state = input$stateChoice,
                          series = input$seriesChoice,
                          transformation = input$transformation,
                          highlights = input$highlightSet,
                          align = input$align,
                          num_align = input$num_align,
                          facet = input$facet)
    })
    
    renderCases <- function(these.data) {
        
        s <- these.data$series
        
        local.df <- state.df[state.df$state %in% these.data$state, ]
        
        if(as.logical(these.data$align)) {
            start_dates <- local.df %>% 
                group_by(state) %>% 
                summarise(start_date = min(date[.data[[s]] >= as.numeric(these.data$num_align)], na.rm = TRUE))
            local.df <- local.df[order(local.df$state), ][unlist(sapply(1:nrow(start_dates), function(x) local.df$date[local.df$state == start_dates$state[x]] >= start_dates$start_date[x])), ]
            local.df <- local.df[local.df$state %in% start_dates$state[!is.na(day(start_dates$start_date))], ]
            local.df <- local.df %>% group_by(state) %>% mutate(date = date - min(date))
        }
        
        local.colors <- unlist(colors.list[unique(local.df$state)])
        if(length(local.colors) == 0){local.colors <- colors[1]}
        
        highlights <- these.data$highlights
        if(length(highlights) > 0) {
            sapply(names(local.colors), function(x) if(!(x %in% highlights)) {local.colors[x] <<- '#DEDEDE'})
        }
        
        point.size <- 3.5
        line.size <- 1.1
        
        local.df[!(local.df$state %in% highlights), ] %>%
            ggplot(aes(x = date, 
                       y = .data[[s]], 
                       color = state,
                       fill = state)) -> p
        
        if(as.logical(these.data$facet)) {
            p <- p + facet_wrap(vars(state)) +
                theme_minimal_hgrid(9, rel_small = 1) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1),
                      legend.position = "right",
                      legend.justification = "left",
                      legend.text = element_text(size = 9),
                      legend.box.spacing = unit(0, "pt"),
                      legend.title = element_blank(),
                      panel.spacing.x = unit(0.75, "lines")
                ) 
            point.size <- 1.5
            line.size <- 1.0
        }
        
        if(!as.logical(these.data$facet)) {
            p <- p + theme_minimal_hgrid(12, rel_small = 1) +
                theme(legend.position = "right",
                      legend.justification = "left",
                      legend.text = element_text(size = 9),
                      legend.box.spacing = unit(0, "pt"),
                      legend.title = element_blank()
                ) 
        }
        
        p <- p + geom_line(size = line.size, 
                           alpha = 0.25) +
            geom_point(size = point.size, 
                       alpha = 0.5) +
            xlab('') +
            scale_color_manual(
                name = NULL,
                values = local.colors
            ) +
            scale_fill_manual(
                name = NULL,
                values = local.colors
            ) +
            guides(
                color = guide_legend(
                    nrow = 28,
                    override.aes = list(
                        linetype = c(rep(0, length(unique(local.df$state)))),
                        shape = c(rep(21, length(unique(local.df$state))))
                    )
                )
            )
        
        p <- p +
            geom_line(data = local.df[local.df$state %in% highlights, ],
                      aes(x = date,
                          y = .data[[s]],
                          color = state),
                      size = line.size,
                      alpha = 0.5) +
            geom_point(data = local.df[local.df$state %in% highlights, ],
                       aes(x = date,
                           y = .data[[s]],
                           color = state, fill = state),
                       size = point.size,
                       alpha = 0.75)
        
        ## Animation disabled for now
        # if(as.logical(these.data$animate)) {        
        #     local.df[!(local.df$state %in% highlights), ] %>%
        #         ggplot(aes(x = date, 
        #                    y = .data[[s]], 
        #                    color = state,
        #                    fill = state,
        #                    group = state)) + 
        #         geom_line(size = 1.1, 
        #                   alpha = 0.25) +
        #         geom_point(size = 3.5, 
        #                    alpha = 0.5) +
        #         transition_reveal(date) +
        #         xlab('') +
        #         scale_color_manual(
        #             name = NULL,
        #             values = local.colors
        #         ) +
        #         scale_fill_manual(
        #             name = NULL,
        #             values = local.colors
        #         ) +
        #         guides(
        #             color = guide_legend(
        #                 nrow = 28
        #             )
        #         ) +
        #         theme_minimal_hgrid(12, rel_small = 1) +
        #         theme(
        #             legend.position = "right",
        #             legend.justification = "left",
        #             legend.text = element_text(size = 9),
        #             legend.box.spacing = unit(0, "pt"),
        #             legend.title = element_blank()
        #         ) -> p
        # }
        
        if(these.data$transformation != 'none')
            p <- p + scale_y_continuous(trans = these.data$transformation)
        
        if(as.logical(these.data$align))
            p <- p + xlab(paste('Days since alignment number'))
        
        if(s == 'positive')
            p <- p + ylab('Number of COVID-19 positive tests')
        if(s == 'negative')
            p <- p + ylab('Number of COVID-19 negative tests')
        if(s == 'pending')
            p <- p + ylab('Number of COVID-19 pending tests')
        if(s == 'hospitalized')
            p <- p + ylab('Number of COVID-19 hospitalized patients')
        if(s == 'death')
            p <- p + ylab('Number of COVID-19 deaths')
        if(s == 'totalTestResults')
            p <- p + ylab('Number of COVID-19 tests run')
        if(s == 'positiveIncrease')
            p <- p + ylab('Daily increase in number of COVID-19 positive tests')
        if(s == 'negativeIncrease')
            p <- p + ylab('Daily increase in number of COVID-19 negative tests')
        if(s == 'hospitalizedIncrease')
            p <- p + ylab('Daily increase in number of COVID-19 hospitalized patients')
        if(s == 'deathIncrease')
            p <- p + ylab('Daily increase in number of COVID-19 deaths')
        if(s == 'totalTestResultsIncrease')
            p <- p + ylab('Daily increase in number of COVID-19 tests run')
        
        ## Animation disabled for now
        #if(as.logical(these.data$animate)) { 
        #    p <- animate(p)
        #}
        
        print(p)
    }
    
    renderMap <- function(these.data) {
        s <- these.data$series
        
        most_recent_hash <- (state.df %>% 
                                 group_by(state) %>% 
                                 summarise(most_recent_hash = hash[.data$date == max(.data$date)]))$most_recent_hash
        
        todays.state.df <- state.df[state.df$hash %in% most_recent_hash, ]
        todays.state.df <- todays.state.df[match(as.character(us_sf$iso_3166_2), todays.state.df$state), ]
        us_sf <- bind_cols(us_sf, todays.state.df)
        
        if(s == 'positive')
            this.legend.title <- 'Number of COVID-19 positive tests'
        if(s == 'negative')
            this.legend.title <- 'Number of COVID-19 negative tests'
        if(s == 'pending')
            this.legend.title <- 'Number of COVID-19 pending tests'
        if(s == 'hospitalized')
            this.legend.title <- 'Number of COVID-19 hospitalized patients'
        if(s == 'death')
            this.legend.title <- 'Number of COVID-19 deaths'
        if(s == 'totalTestResults')
            this.legend.title <- 'Number of COVID-19 tests run'
        if(s == 'positiveIncrease')
            this.legend.title <- "Today's increase in number of COVID-19 positive tests"
        if(s == 'negativeIncrease')
            this.legend.title <- "Today's increase in number of COVID-19 negative tests"
        if(s == 'hospitalizedIncrease')
            this.legend.title <- "Today's increase in number of COVID-19 hospitalized patients"
        if(s == 'deathIncrease')
            this.legend.title <- "Today's increase in number of COVID-19 deaths"
        if(s == 'totalTestResultsIncrease')
            this.legend.title <- "Today's increase in number of COVID-19 tests run"
        
        p <- ggplot(us_sf, aes(fill = .data[[s]])) + 
            geom_sf(color = "white") +
            theme_map(10) +
            theme(
                legend.title.align = 0.5,
                legend.text.align = 0.5,
                legend.justification = c(0, 0),
                legend.position = c(0.6, 0.0)
            ) +
            labs(fill = this.legend.title)
        
        if(these.data$transformation == 'none')
            p <- p +
            scale_fill_continuous_sequential(
                palette = "Blues", 
                rev = TRUE,
                na.value = "grey60",
                guide = guide_colorbar(
                    direction = "horizontal",
                    label.position = "bottom",
                    title.position = "top",
                    barwidth = grid::unit(4.0, "in"),
                    barheight = grid::unit(0.2, "in")
                )
            )
        
        if(these.data$transformation == 'log10')
            p <- p +
            scale_fill_continuous_sequential(
                trans = 'log10',
                palette = "Blues", 
                rev = TRUE,
                na.value = "grey60",
                guide = guide_colorbar(
                    direction = "horizontal",
                    label.position = "bottom",
                    title.position = "top",
                    barwidth = grid::unit(4.0, "in"),
                    barheight = grid::unit(0.2, "in")
                )
            )
        
        if(these.data$transformation == 'log')
            p <- p +
            scale_fill_continuous_sequential(
                trans = 'log',
                palette = "Blues", 
                rev = TRUE,
                na.value = "grey60",
                guide = guide_colorbar(
                    direction = "horizontal",
                    label.position = "bottom",
                    title.position = "top",
                    barwidth = grid::unit(4.0, "in"),
                    barheight = grid::unit(0.2, "in")
                )
            )
        
        if(these.data$transformation == 'sqrt')
            p <- p +
            scale_fill_continuous_sequential(
                trans = 'sqrt',
                palette = "Blues", 
                rev = TRUE,
                na.value = "grey60",
                guide = guide_colorbar(
                    direction = "horizontal",
                    label.position = "bottom",
                    title.position = "top",
                    barwidth = grid::unit(4.0, "in"),
                    barheight = grid::unit(0.2, "in")
                )
            )
        
        print(p)
    }
    
    output$casesPlot <- renderPlot(renderCases(these.data = inputData()))
    output$mapPlot <- renderPlot(renderMap(these.data = inputData()))
}

# Run the application 
shinyApp(ui = ui, server = server)
