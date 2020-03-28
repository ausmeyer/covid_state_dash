#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
library(countrycode)
library(ggrepel)
library(tidycensus)
library(scales)
library(grid)

state.df <- read_csv('http://covidtracking.com/api/states/daily.csv')

colors <- hue_pal()(length(unique(state.df$state)))
colors.list <- list()
sapply(1:length(unique(state.df$state)), function(x) colors.list[unique(state.df$state)[x]] <<- colors[x])

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    inputData <- reactive({
        list(state = input$stateChoice,
             series = input$seriesChoice,
             transformation = input$transformation,
             highlights = input$highlightSet)
    })
    
    renderCases <- function() {
        local.df <- state.df[state.df$state %in% inputData()$state, ]
        
        local.colors <- unlist(colors.list[unique(local.df$state)])
        if(length(local.colors) == 0){local.colors <- colors[1]}
        
        highlights <- inputData()$highlights
        if(length(highlights) > 0) {
            sapply(names(local.colors), function(x) if(!(x %in% highlights)) {local.colors[x] <<- '#DEDEDE'})
        }
        
        p <- ggplot() + 
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
            ) +
            theme_minimal_hgrid(12, rel_small = 1) +
            theme(
                legend.position = "right",
                legend.justification = "left",
                legend.text = element_text(size = 9),
                legend.box.spacing = unit(0, "pt"),
                legend.title = element_blank()
            )
        
        s <- inputData()$series
        
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
        
        if( inputData()$transformation == 'none' ){
            p <- p +
                geom_line(data = local.df[!(local.df$state %in% highlights), ], 
                          aes(x = dateChecked, y = .data[[s]], color = state), 
                          size = 1.1, 
                          alpha = 0.25) +
                geom_point(data = local.df[!(local.df$state %in% highlights), ], 
                           aes(x = dateChecked, y = .data[[s]], color = state, fill = state), 
                           size = 3.5, 
                           alpha = 0.5) +
                geom_line(data = local.df[local.df$state %in% highlights, ], 
                          aes(x = dateChecked, y = .data[[s]], color = state), 
                          size = 1.1, 
                          alpha = 0.5) +
                geom_point(data = local.df[local.df$state %in% highlights, ],
                           aes(x = dateChecked, y = .data[[s]], color = state, fill = state), 
                           size = 3.5, 
                           alpha = 0.75)
        }
        if( inputData()$transformation == 'log10' ){
            p <- p +
                geom_line(data = local.df[!(local.df$state %in% highlights), ], 
                          aes(x = dateChecked, y = log10(.data[[s]]), color = state), 
                          size = 1.1, 
                          alpha = 0.25) +
                geom_point(data = local.df[!(local.df$state %in% highlights), ], 
                           aes(x = dateChecked, y = log10(.data[[s]]), color = state, fill = state), 
                           size = 3.5, 
                           alpha = 0.5) +
                geom_line(data = local.df[local.df$state %in% highlights, ], 
                          aes(x = dateChecked, y = log10(.data[[s]]), color = state), 
                          size = 1.1, 
                          alpha = 0.5) +
                geom_point(data = local.df[local.df$state %in% highlights, ],
                           aes(x = dateChecked, y = log10(.data[[s]]), color = state, fill = state), 
                           size = 3.5, 
                           alpha = 0.75)
        }
        if( inputData()$transformation == 'log' ){
            p <- p +
                geom_line(data = local.df[!(local.df$state %in% highlights), ], 
                          aes(x = dateChecked, y = log(.data[[s]]), color = state), 
                          size = 1.1, 
                          alpha = 0.25) +
                geom_point(data = local.df[!(local.df$state %in% highlights), ], 
                           aes(x = dateChecked, y = log(.data[[s]]), color = state, fill = state), 
                           size = 3.5, 
                           alpha = 0.5) +
                geom_line(data = local.df[local.df$state %in% highlights, ], 
                          aes(x = dateChecked, y = log(.data[[s]]), color = state), 
                          size = 1.1, 
                          alpha = 0.5) +
                geom_point(data = local.df[local.df$state %in% highlights, ],
                           aes(x = dateChecked, y = log(.data[[s]]), color = state, fill = state), 
                           size = 3.5, 
                           alpha = 0.75)
        }
        if( inputData()$transformation == 'sqrt' ){
            p <- p + 
                geom_line(data = local.df[!(local.df$state %in% highlights), ], 
                          aes(x = dateChecked, y = sqrt(.data[[s]]), color = state), 
                          size = 1.1, 
                          alpha = 0.25) +
                geom_point(data = local.df[!(local.df$state %in% highlights), ], 
                           aes(x = dateChecked, y = sqrt(.data[[s]]), color = state, fill = state), 
                           size = 3.5, 
                           alpha = 0.5) +
                geom_line(data = local.df[local.df$state %in% highlights, ], 
                          aes(x = dateChecked, y = sqrt(.data[[s]]), color = state), 
                          size = 1.1, 
                          alpha = 0.5) +
                geom_point(data = local.df[local.df$state %in% highlights, ],
                           aes(x = dateChecked, y = sqrt(.data[[s]]), color = state, fill = state), 
                           size = 3.5, 
                           alpha = 0.75)
        }
        
        show(p)
    }
    output$casesPlot <- renderPlot(renderCases())

})
