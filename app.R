#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggiraph)
library(tidyverse)
library(lubridate)
library(cowplot)
library(lemon)
library(colorspace)
library(scales)
library(shinycssloaders)
library(sf)
library(albersusa)
library(directlabels)

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

all.transformations <- list('Linear' = 'none', 
                            'Log10' = 'log10')

state.df <- read_csv('http://covidtracking.com/api/states/daily.csv')
state.df$dateChecked <- date(state.df$dateChecked)
state.df$date <- ymd(state.df$date)

us_sf <- usa_sf("laea")

colors <- viridis_pal()(length(unique(state.df$state)))
colors.list <- list()
sapply(1:length(unique(state.df$state)), function(x) colors.list[unique(state.df$state)[x]] <<- colors[x])

doubling_time <- function(N0, d0, ts) {
    N0 * 2 ^ (ts / d0)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    
    # Application title
    titlePanel("COVID-19 US State Tracker Dashboard"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 4,
                     div(style = 'margin-top: -15px; margin-bottom: -15px',
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
                                                h4("y-axis"), 
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
                             ),
                             column(6,
                                    radioButtons("exponentials", 
                                                 h4("Doubling"), 
                                                 choices = list('Yes' = T, 'No' = F),
                                                 selected = list('No' = F)
                                    )
                             )
                             
                         ),
                     )
        ),
        # Show a plot of the generated distribution
        mainPanel(width = 8,
                  tabsetPanel(type = "tabs",
                              tabPanel("Basic Plot", plotOutput("casesPlotPNG", height = 500) %>% withSpinner()),
                              tabPanel("Interactive Plot", girafeOutput("casesPlotSVG") %>% withSpinner()),
                              tabPanel('Map', plotOutput("mapPlot", height = 500) %>% withSpinner())
                  )
        )
    ),
    hr(),
    
    strong("Explanation:"),
    
    "Charts will build automatically 1.5 seconds after changing any parameter.
    The 'Align' option will align each state with Day 0 as the 
    first day that each had at least 'Align Number' number of 
    the variable you selected. The 'Facet' option will split each 
    state into its own subplot; be careful with it because it could
    take some time to render. The map shows the most recent day's data.
    The y-axis transformation chosen will be applied to both the plot and the map. 
    The doubling time guides (called 'Doubling') can only be selected when 'Align' is selected, 
    Facet' is unselected and the Log10 y-axis is selected. The doubling time guides 
    display as 7 dashed lines; the steepest line is doubling every 1 day and the 
    flattest is doubling every 7 days. They are calculated with Day 0 being the mean of
    Day 0 for all currently selected data. If Day 0 had zero cases, 1 case is substituted to calculate the 
    doubling guide. Note, aligning correctly will significantly improve interpretability of the doubling guides.
    The Interactive SVG might take longer to build, but will have increasing interactive features.",
    
    br(),br(),
    
    "Data from: ",
    a("http://covidtracking.com/", href="http://covidtracking.com/"),
    
    br(),
    
    "My very ugly code available at: ",
    a("https://github.com/ausmeyer/covid_state_dash", href="https://github.com/ausmeyer/covid_state_dash"),
    
    br(),br()
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    renderCasesPNG <- function(these.data) {
        
        s <- these.data$series
        
        local.df <- state.df[state.df$state %in% these.data$state, ]
        
        if(as.logical(these.data$align)) {
            
            start_dates <- local.df %>% 
                group_by(state) %>% 
                summarise(start_date = min(date[.data[[s]] >= as.numeric(these.data$num_align)], na.rm = TRUE))
            
            if(nrow(start_dates) > 1)
                local.df <- local.df[order(local.df$state), ][unlist(sapply(1:nrow(start_dates), function(x) local.df$date[local.df$state == start_dates$state[x]] >= start_dates$start_date[x])), ]
            
            local.df <- local.df[local.df$state %in% start_dates$state[!is.na(day(start_dates$start_date))], ]
            
            start <- median(local.df[[s]][local.df$date == min(local.df$date)])
            
            if(start == 0)
                start <- 1
            
            date_seq <- 0:(max(local.df$date) - min(local.df$date))
            ys <- lapply(c(2, 3, 5, 7), function(x) doubling_time(start, x, date_seq))
            
            exp.df <- tibble(date = rep(min(local.df$date) + days(date_seq), 4),
                             y = unlist(ys),
                             ds = c(rep('2 days', length(date_seq)),
                                    rep('3 days', length(date_seq)),
                                    rep('5 days', length(date_seq)),
                                    rep('7 days', length(date_seq))))
            
            exp.df$date <- exp.df$date - min(local.df$date)
            local.df <- local.df %>% group_by(state) %>% mutate(date = date - min(date))
        }
        
        local.colors <- unlist(colors.list[unique(local.df$state)])
        if(length(local.colors) == 0){local.colors <- colors[1]}
        
        highlights <- these.data$highlights
        if(length(highlights) > 0) {
            sapply(names(local.colors), function(x) if(!(x %in% highlights)) {local.colors[x] <<- '#DEDEDE'})
        }
        
        p <- ggplot() +
            ylim(min(local.df[[s]], na.rm = T), max(local.df[[s]], na.rm = T))
        
        # define base sizes
        base.size <- 14
        point.size <- 3.5
        line.size <- 1.25
        font.size <- 16
        
        if(as.logical(these.data$exp)) {
            p <- p + geom_line(data = exp.df,
                               aes(x = date, 
                                   y = y, 
                                   group = ds), 
                               color = 'gray',
                               alpha = 0.8,
                               size = line.size * 0.9,
                               linetype = "dashed") +
                annotate("text",
                         x = max(exp.df$date) * 0.99,
                         y = max(exp.df$y[exp.df$ds == '1 day']),
                         label = "doubling every day",
                         size = 6,
                         hjust = 1,
                         vjust = 0,
                         color = 'gray',
                         alpha = 1) +
                annotate("text",
                         x = max(exp.df$date),
                         y = max(exp.df$y[exp.df$ds == '2 days']),
                         label = "doubling every 2 days",
                         size = 5.5,
                         hjust = 1,
                         vjust = -0.25,
                         color = 'gray',
                         alpha = 1) +
                annotate("text",
                         x = max(exp.df$date),
                         y = max(exp.df$y[exp.df$ds == '3 days']),
                         label = "doubling every 3 days",
                         size = 5,
                         hjust = 1,
                         vjust = -0.25,
                         color = 'gray',
                         alpha = 1) +
                annotate("text",
                         x = max(exp.df$date),
                         y = max(exp.df$y[exp.df$ds == '5 days']),
                         label = "doubling every 5 days",
                         size = 4.5,
                         hjust = 1,
                         vjust = -0.25,
                         color = 'gray',
                         alpha = 1) +
                annotate("text",
                         x = max(exp.df$date),
                         y = max(exp.df$y[exp.df$ds == '7 days']),
                         label = "doubling every 7 days",
                         size = 4,
                         hjust = 1,
                         vjust = -0.25,
                         color = 'gray',
                         alpha = 1)
        }
        
        if(these.data$transformation != 'none')
            p <- p + scale_y_continuous(trans = these.data$transformation)
        
        if(as.logical(these.data$facet)) {
            point.size <- rescale(length(unique(local.df$state)), 
                                  to = c(1.5, point.size), 
                                  from = c(length(unique(state.df$state)), 1))
            line.size <- rescale(length(unique(local.df$state)), 
                                 to = c(1, line.size), 
                                 from = c(length(unique(state.df$state)), 1))
            font.size.adjust <- rescale(length(unique(local.df$state)), 
                                        to = c(10, font.size), 
                                        from = c(length(unique(state.df$state)), 1))
            
            p <- p + facet_rep_wrap(~state, scales = "fixed", repeat.tick.labels = FALSE) +
                theme_minimal_hgrid(font.size.adjust, rel_small = 1, color = 'white') +
                theme(panel.background = element_rect(fill = 'grey95'),
                      axis.line.x = element_line(color = 'black'),
                      axis.ticks.x = element_line(color = "black"),
                      legend.position = "right",
                      legend.justification = "left",
                      legend.text = element_text(size = base.size),
                      legend.box.spacing = unit(0, "pt"),
                      legend.title = element_blank(),
                      panel.spacing.x = unit(0.75, "lines"),
                      axis.text.x = element_text(angle = 90, vjust = 0.5),
                      axis.title = element_text(size = font.size),
                      axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
                      axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))
                ) 
        }
        
        if(!as.logical(these.data$facet)) {
            p <- p + theme_minimal_hgrid(base.size, rel_small = 1) +
                theme(legend.position = "right",
                      legend.justification = "left",
                      legend.text = element_text(size = base.size),
                      legend.box.spacing = unit(0, "pt"),
                      legend.title = element_blank(),
                      axis.title = element_text(size = font.size),
                      axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
                      axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))
                ) 
        }
        
        plottable.df <- local.df[!(local.df$state %in% highlights), ]
        p <- p +
            geom_line(data = plottable.df,
                      aes(x = date, 
                          y = .data[[s]], 
                          color = state),
                      size = line.size, 
                      alpha = 0.25) + 
            geom_point(data = plottable.df,
                       aes(x = date, 
                           y = .data[[s]], 
                           color = state,
                           fill = state),
                       size = point.size, 
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
        
        if(length(highlights) > 0) {
            highlights.df <- local.df[local.df$state %in% highlights, ]
            p <- p + geom_line(data = highlights.df,
                               aes(x = date,
                                   y = .data[[s]],
                                   color = state),
                               size = line.size,
                               alpha = 0.5) + 
                geom_point(data = highlights.df,
                                       aes(x = date, 
                                           y = .data[[s]], 
                                           color = state,
                                           fill = state),
                                       size = point.size,
                                       alpha = 0.75)
        }
        
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
        
        return(p)
    }
    
    renderCasesSVG <- function(these.data) {
        
        s <- these.data$series
        
        local.df <- state.df[state.df$state %in% these.data$state, ]
        
        if(as.logical(these.data$align)) {
            start_dates <- local.df %>% 
                group_by(state) %>% 
                summarise(start_date = min(date[.data[[s]] >= as.numeric(these.data$num_align)], na.rm = TRUE))
            
            if(nrow(start_dates) > 1)
                local.df <- local.df[order(local.df$state), ][unlist(sapply(1:nrow(start_dates), function(x) local.df$date[local.df$state == start_dates$state[x]] >= start_dates$start_date[x])), ]
            
            local.df <- local.df[local.df$state %in% start_dates$state[!is.na(day(start_dates$start_date))], ]
            
            start <- median(local.df[[s]][local.df$date == min(local.df$date)])
            if(start == 0)
                start <- 1
            
            date_seq <- 0:(max(local.df$date) - min(local.df$date))
            ys <- lapply(c(2, 3, 5, 7), function(x) doubling_time(start, x, date_seq))
            
            exp.df <- tibble(date = rep(min(local.df$date) + days(date_seq), 4),
                             y = unlist(ys),
                             ds = c(rep('2 days', length(date_seq)),
                                    rep('3 days', length(date_seq)),
                                    rep('5 days', length(date_seq)),
                                    rep('7 days', length(date_seq))))
            
            exp.df$date <- exp.df$date - min(local.df$date)
            local.df <- local.df %>% group_by(state) %>% mutate(date = date - min(date))
        }
        
        local.colors <- unlist(colors.list[unique(local.df$state)])
        if(length(local.colors) == 0){local.colors <- colors[1]}
        
        highlights <- these.data$highlights
        if(length(highlights) > 0) {
            sapply(names(local.colors), function(x) if(!(x %in% highlights)) {local.colors[x] <<- '#DEDEDE'})
        }
        
        p <- ggplot() +
            ylim(min(local.df[[s]], na.rm = T), max(local.df[[s]], na.rm = T))
        
        # define base sizes
        base.size <- 22
        point.size <- 6.5
        line.size <- 2.2
        font.size <- 28
        ano.size <- 8
        
        if(as.logical(these.data$exp)) {
            
            xval <- as.numeric(max(exp.df$date))
            
            yval1 <- (log10(max(exp.df$y[exp.df$ds == '1 day'])) - log10(min(exp.df$y))) * 2.6
            yval2 <- log10(max(exp.df$y[exp.df$ds == '2 days'])) - log10(min(exp.df$y))
            
            #print(atan(yval1 / xval) * 180 / pi)
            
            p <- p + geom_line(data = exp.df,
                               aes(x = date, 
                                   y = y, 
                                   group = ds), 
                               color = 'gray',
                               alpha = 0.8,
                               size = line.size * 0.9,
                               linetype = "dashed") +
                annotate("text",
                         x = xval,
                         y = max(exp.df$y[exp.df$ds == '1 day']),
                         label = "doubling every day",
                         size = ano.size,
                         hjust = 1,
                         vjust = 0,
                         #angle = atan(yval1 / xval) * 180 / pi,
                         color = 'gray',
                         alpha = 1) +
                annotate("text",
                         x = xval,
                         y = max(exp.df$y[exp.df$ds == '2 days']),
                         label = "doubling every 2 days",
                         size = ano.size * 0.95,
                         hjust = 1,
                         vjust = 0,
                         color = 'gray',
                         alpha = 1) +
                annotate("text",
                         x = max(exp.df$date),
                         y = max(exp.df$y[exp.df$ds == '3 days']),
                         label = "doubling every 3 days",
                         size = ano.size * 0.9,
                         hjust = 1,
                         vjust = 0,
                         color = 'gray',
                         alpha = 1) +
                annotate("text",
                         x = max(exp.df$date),
                         y = max(exp.df$y[exp.df$ds == '5 days']),
                         label = "doubling every 5 days",
                         size = ano.size * 0.85,
                         hjust = 1,
                         vjust = 0,
                         color = 'gray',
                         alpha = 1) +
                annotate("text",
                         x = max(exp.df$date),
                         y = max(exp.df$y[exp.df$ds == '7 days']),
                         label = "doubling every 7 days",
                         size = ano.size * 0.8,
                         hjust = 1,
                         vjust = 0,
                         color = 'gray',
                         alpha = 1)
        }
        
        if(these.data$transformation != 'none')
            p <- p + scale_y_continuous(trans = these.data$transformation)
        
        if(as.logical(these.data$facet)) {
            point.size <- rescale(length(unique(local.df$state)), 
                                  to = c(1.5, point.size), 
                                  from = c(length(unique(state.df$state)), 1))
            line.size <- rescale(length(unique(local.df$state)), 
                                 to = c(1, line.size), 
                                 from = c(length(unique(state.df$state)), 1))
            font.size.adjust <- rescale(length(unique(local.df$state)), 
                                        to = c(10, font.size), 
                                        from = c(length(unique(state.df$state)), 1))
            
            p <- p + facet_rep_wrap(~state, scales = "fixed", repeat.tick.labels = FALSE) +
                theme_minimal_hgrid(font.size.adjust, rel_small = 1, color = 'white') +
                theme(panel.background = element_rect(fill = 'grey95'),
                      axis.line.x = element_line(color = 'black'),
                      axis.ticks.x = element_line(color = "black"),
                      legend.position = "right",
                      legend.justification = "left",
                      legend.text = element_text(size = base.size),
                      legend.box.spacing = unit(0, "pt"),
                      legend.title = element_blank(),
                      panel.spacing.x = unit(0.75, "lines"),
                      axis.text.x = element_text(angle = 90, vjust = 0.5),
                      axis.title = element_text(size = font.size),
                      axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
                      axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))
                ) 
        }
        
        if(!as.logical(these.data$facet)) {
            p <- p + theme_minimal_hgrid(base.size, rel_small = 1) +
                theme(legend.position = "right",
                      legend.justification = "left",
                      legend.text = element_text(size = base.size),
                      legend.box.spacing = unit(0, "pt"),
                      legend.title = element_blank(),
                      axis.title = element_text(size = font.size),
                      axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
                      axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))
                ) 
        }
        
        tooltip.label <- ''
        if(s == 'positive')
            tooltip.label <- 'cases:'
        if(s == 'negative')
            tooltip.label <- 'negative tests:'
        if(s == 'pending')
            tooltip.label <- 'pending tests:'
        if(s == 'hospitalized')
            tooltip.label <- 'hospitalized'
        if(s == 'death')
            tooltip.label <- 'deaths'
        if(s == 'totalTestResults')
            tooltip.label <- 'total tests:'
        if(s == 'positiveIncrease')
            tooltip.label <- 'positive increase:'
        if(s == 'negativeIncrease')
            tooltip.label <- 'negative increase:'
        if(s == 'hospitalizedIncrease')
            tooltip.label <- 'hospitalized increase:'
        if(s == 'deathIncrease')
            tooltip.label <- 'death increase:'
        if(s == 'totalTestResultsIncrease')
            tooltip.label <- 'total tests increase:'
        
        tooltip.func <- function(dat) {
            this.list <- unlist(lapply(1:nrow(dat), function(i) paste('state:', dat$state[i], '\n', 
                                                                      'day:', dat$date[i], '\n',
                                                                      tooltip.label, as.character(dat[[s]][i]))
                                       )
                                )
            return(this.list)
        }
        
        plottable.df <- local.df[!(local.df$state %in% highlights), ]
        p <- p +
            geom_line(data = plottable.df,
                      aes(x = date, 
                          y = .data[[s]], 
                          color = state),
                      size = line.size, 
                      alpha = 0.25) + 
            geom_point_interactive(data = plottable.df,
                                   aes(x = date, 
                                       y = .data[[s]], 
                                       color = state,
                                       fill = state,
                                       tooltip = tooltip.func(plottable.df),
                                       data_id = state),
                                   size = point.size, 
                                   alpha = 0.5)+ 
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
        
        if(length(highlights) > 0) {
            highlights.df <- local.df[local.df$state %in% highlights, ]
            p <- p + geom_line(data = highlights.df,
                               aes(x = date,
                                   y = .data[[s]],
                                   color = state),
                               size = line.size,
                               alpha = 0.5) + 
                geom_point_interactive(data = highlights.df,
                                       aes(x = date, 
                                           y = .data[[s]], 
                                           color = state,
                                           fill = state,
                                           tooltip = tooltip.func(highlights.df),
                                           data_id = state),
                                       size = point.size,
                                       alpha = 0.75)
        }
        
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
        
        return(p)
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
                legend.position = c(0.4, 0.0)
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
        
        return(p)
    }
    
    inputData <- reactive({
        these.data = list(state = input$stateChoice,
                          series = input$seriesChoice,
                          transformation = input$transformation,
                          highlights = input$highlightSet,
                          align = input$align,
                          num_align = input$num_align,
                          facet = input$facet,
                          exp = input$exponentials,
                          output = input$output)
    }) %>% debounce(1500)
    
    observe({
        inputData()
        if(as.logical(input$facet) | input$transformation == 'none' | !as.logical(input$align)) {
            updateRadioButtons(session, "exponentials",
                               selected = list('No' = F))
        }
    })
    
    observe(shinyjs::toggleState("exponentials", input$transformation == 'log10' & input$facet == 'FALSE' & input$align == 'TRUE'))
    
    output$casesPlotPNG <- renderPlot(renderCasesPNG(inputData()))
    output$casesPlotSVG <- renderGirafe(girafe(ggobj = renderCasesSVG(inputData()),
                                               width_svg = 20,
                                               height_svg = 20 * 5 / 7,
                                               options = list(opts_selection(type = "single", only_shiny = FALSE))))
    output$mapPlot <- renderPlot(renderMap(inputData()))
}

# Run the application 
shinyApp(ui = ui, server = server)
