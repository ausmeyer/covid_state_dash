#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library('shiny')
library('shinyjs')
library('shinyWidgets') 
library('plotly')
library('tidyverse')
library('lubridate')
library('cowplot')
library('lemon')
library('colorspace')
library('scales')
library('shinycssloaders')
library('sf')
library('albersusa')
library('hues')
library('zoo')
library('ggiraph')

options(spinner.color="#3e5fff")

all.choices <- list('Total' = 'Total', 
                    "Alaska" = "AK", "Alabama" = "AL", "Arkansas" = "AR","American Samoa" = "AS", "Arizona" = "AZ", 
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

all.series <- list('Aggregate Cases' = 'positive', 
                   'Daily Cases' = 'positiveIncrease',
                   'Aggregate Negatives' = 'negative', 
                   'Daily Negatives' = 'negativeIncrease',
                   'Aggregate Hospitalizations' = 'hospitalized', 
                   'Daily Hospitalizations' = 'hospitalizedIncrease',
                   'Currently Hospitalized' = 'hospitalizedCurrently',
                   'Aggregate ICU' = 'inIcuCumulative',
                   'Currently ICU' = 'inIcuCurrently', 
                   'Aggregate Ventilated' = 'onVentilatorCumulative',
                   'Currently Ventilated' = 'onVentilatorCurrently',
                   'Aggregate Deaths' = 'death', 
                   'Daily Deaths' = 'deathIncrease',
                   'Recovered' = 'recovered',
                   'Aggregate Tests' = 'totalTestResults',
                   'Daily Tests' = 'totalTestResultsIncrease',
                   'Aggregate Pending' = 'pending')

all.transformations <- list('Linear' = 'none', 
                            'Log10' = 'log10')

state.df <- read_csv('http://covidtracking.com/api/states/daily.csv')
state.df$dateChecked <- date(state.df$dateChecked)
state.df$date <- ymd(state.df$date)
totals.df <- state.df %>%
    group_by(date) %>%
    summarise(state = 'Total',
              positive = sum(positive, na.rm = T),
              positiveIncrease = sum(positiveIncrease, na.rm = T),
              negative = sum(negative, na.rm = T),
              negativeIncrease = sum(negativeIncrease, na.rm = T),
              hospitalized = sum(hospitalized, na.rm = T),
              hospitalizedIncrease = sum(hospitalizedIncrease, na.rm = T),
              hospitalizedCurrently = sum(hospitalizedCurrently, na.rm = T),
              inIcuCumulative = sum(inIcuCumulative, na.rm = T),
              inIcuCurrently = sum(inIcuCurrently, na.rm = T),
              onVentilatorCumulative = sum(onVentilatorCumulative, na.rm = T),
              onVentilatorCurrently = sum(onVentilatorCurrently, na.rm = T),
              death = sum(death, na.rm = T),
              deathIncrease = sum(deathIncrease, na.rm = T),
              recovered = sum(recovered, na.rm = T),
              totalTestResults = sum(totalTestResults, na.rm = T),
              totalTestResultsIncrease = sum(totalTestResultsIncrease, na.rm = T),
              pending = sum(pending, na.rm = T))

state.df <- bind_rows(totals.df, state.df)
    
input.settings <- c()

us_sf <- usa_sf("laea")

#colors <- viridis_pal()(length(unique(state.df$state)))
default.colors <- c("#333333",
                    "#43b9d5",
                    "#d34336",
                    "#41c464",
                    "#a95bdb",
                    "#62c649",
                    "#6463e5",
                    "#a4be2e",
                    "#bc37ad",
                    "#5da225",
                    "#eb69db",
                    "#328634",
                    "#e5339b",
                    "#4cc07b",
                    "#e3325e",
                    "#65c699",
                    "#9356bd",
                    "#cbb43f",
                    "#4a6bdb",
                    "#dd9a35",
                    "#5e60bf",
                    "#92ba5b",
                    "#8386ed",
                    "#658125",
                    "#d284e1",
                    "#4c9157",
                    "#cb4f97",
                    "#52c8ba",
                    "#d54c79",
                    "#3d9d7d",
                    "#a4509b",
                    "#988625",
                    "#458ee8",
                    "#db6c2e",
                    "#356ab0",
                    "#a66628",
                    "#9792e2",
                    "#a1aa64",
                    "#7462b1",
                    "#8eba82",
                    "#805b92",
                    "#577741",
                    "#de8cc1",
                    "#307554",
                    "#b35254",
                    "#329495",
                    "#ea8d77",
                    "#69aae1",
                    "#7d7336",
                    "#bca8e4",
                    "#d2a46d",
                    "#407aaa",
                    "#a06647",
                    "#6d75ae",
                    "#e18b9d",
                    "#a881bb",
                    "#a4597a")

colors.list <- list()
sapply(1:length(unique(state.df$state)), function(x) colors.list[unique(state.df$state)[x]] <<- default.colors[x])

doubling_time <- function(N0, d0, ts) {
    N0 * 2 ^ (ts / d0)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(includeHTML(("google-analytics.html"))),
    useShinyjs(),
    
    # Application title
    titlePanel("COVID-19 US State Tracker Dashboard"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 4,
                     div(style = 'margin-top: -15px; margin-bottom: -5px',
                         fluidRow(
                             column(12,
                                    pickerInput("stateChoice", 
                                                h4("States"), 
                                                options = list(`actions-box` = TRUE),
                                                multiple = TRUE,
                                                choices = all.choices,
                                                selected = all.choices[-1]
                                    )
                             ),
                             column(12,
                                    pickerInput("highlightSet", 
                                                h4("Highlighted States"), 
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
                                    numericInput("start", 
                                                 h5("Start on Day"), 
                                                 value = 0)
                             ),
                             column(6,
                                    numericInput("smooth", 
                                                 h5("Smooth over Window"), 
                                                 value = 1)
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
                                    radioButtons("facet", 
                                                 h4("Facet"), 
                                                 choices = list('Yes' = T, 'No' = F),
                                                 selected = list('No' = F)
                                    )
                             )
                         ),
                         fluidRow(
                             column(12,
                                    numericInput("num_align", 
                                                 h4("Align on Number"), 
                                                 value = 0)
                             )
                         ),
                         fluidRow(
                             column(6,
                                    radioButtons("fitChoice", 
                                                 h4("Fit"), 
                                                 choices = list('Yes' = T, 'No' = F),
                                                 selected = list('No' = F)
                                    )
                             ),
                             column(6,
                                    radioButtons("exponentials", 
                                                 h4("Guide"), 
                                                 choices = list('Yes' = T, 'No' = F),
                                                 selected = list('No' = F)
                                    )
                             )
                         ),
                         fluidRow(
                             align = 'center',
                             column(12,
                                    actionButton('shuffle_colors',
                                                 'Shuffle Colors'),
                             )
                         )
                     )
        ),
        # Show a plot of the generated distribution
        mainPanel(width = 8,
                  tabsetPanel(type = "tabs",
                              tabPanel("Basic Plot", plotOutput("casesPlot", height = 700) %>% withSpinner()),
                              tabPanel('Interactive Plot', plotlyOutput('casesPlotly') %>% withSpinner()),
                              tabPanel('Map', girafeOutput("mapPlot") %>% withSpinner())
                  )
        )
    ),
    hr(),
    
    strong("Explanation:"),
    
    "Please note, most of the data fields from COVID Tracking Project are available here, but some of the fields (like the aggregate number of ventilated patients) are less reliable than others.
    Charts will build automatically 1.5 seconds after changing any parameter. 
    The States menu picks which states to include in the figure.
    The Highlights menu will preferentially color those states and make the rest gray. 
    The y-axis menu picks the scale of the data; it also affects the Map. 
    The Start on Day option will start the time series that day of days from the beginning of the available data; in terms of interaction this starting would happen AFTER alignment.
    The Smooth over Window option will plot the moving average of the time series with the selected sliding window; in terms of interaction the fits and guides would use the smoothed data. 
    A smoothing window of 1 day is unsmoothed.
    The Align option will align each state with Day 0 as the first day that each had at least 'Align on Number' number of the data you selected.
    The Facet option will split each state into its own subplot; be careful with it because it could take some time to render. 
    The Fit option will fit an exponential model to the displayed data
    The Guide option will display the doubling time in days from the median of the displayed data.
    The Data menu picks the data series to be plotted. 
    The map shows the most recent day's data. 
    Note, aligning correctly will significantly improve interpretability of the doubling guides. 
    The basic and interactive plots should show the same information. 
    The interactive plot could take longer to render so options could be selected in basic mode and then switch to interactive.",
    
    br(),br(),
    
    "Complaints/Suggestions Department: ",
    a("@austingmeyer", href="https://twitter.com/austingmeyer"),
    
    br(),
    
    "Data from: ",
    a("http://covidtracking.com/", href="http://covidtracking.com/"),
    
    br(),
    
    "My very ugly code available at: ",
    a("https://github.com/ausmeyer/covid_state_dash", href="https://github.com/ausmeyer/covid_state_dash"),
    
    br(),br()
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    renderTimeSeries <- function(these.data, these.colors, plotly.settings = F) {
        
        s <- these.data$series
        
        highlights <- these.data$highlights
        
        local.df <- state.df[state.df$state %in% these.data$state, ]
        
        # smooth data if requested
        local.df <- local.df %>% 
            group_by(state) %>%
            select(c(date, all_of(s))) %>% 
            arrange(date) %>%
            mutate(!!s := round(rollmeanr(.data[[s]], as.numeric(input.settings$smooth), fill = NA)))
        
        if(as.logical(these.data$align)) {
            
            start_dates <- local.df %>% 
                group_by(state) %>% 
                summarise(start_date = min(date[.data[[s]] >= as.numeric(these.data$num_align)], na.rm = TRUE))
            
            start_dates$start_date <- start_dates$start_date + input.settings$start
            
            if(nrow(start_dates) > 1)
                local.df <- local.df[order(local.df$state), ][unlist(sapply(1:nrow(start_dates), function(x) local.df$date[local.df$state == start_dates$state[x]] >= start_dates$start_date[x])), ]
            
            doubling.df <- local.df
            
            if(length(highlights) > 0)
                doubling.df <- local.df[local.df$state %in% highlights, ]
            
            if(nrow(start_dates) > 1)
                minimums <- doubling.df[order(doubling.df$state), ][unlist(lapply(1:nrow(start_dates), function(x) doubling.df$date[doubling.df$state == start_dates$state[x]] == start_dates$start_date[x])), ]
            
            if(as.logical(these.data$exp)) {
                low <- min(minimums[[s]])
                high <- max(minimums[[s]])
                
                if(low == 0)
                    low <- 1
                if(high == 0)
                    high <- 1
                
                start <- 10^mean(c(log10(low), log10(high)))
                
                date_seq <- 0:(max(doubling.df$date) - min(doubling.df$date))
                ys <- lapply(c(2, 3, 5, 7), function(x) doubling_time(start, x, date_seq))
                
                exp.df <- tibble(date = rep(min(doubling.df$date) + days(date_seq), 4),
                                 y = unlist(ys),
                                 ds = c(rep('2 days', length(date_seq)),
                                        rep('3 days', length(date_seq)),
                                        rep('5 days', length(date_seq)),
                                        rep('7 days', length(date_seq))))
                
                exp.df$date <- exp.df$date - min(doubling.df$date)
            }
            
            local.df <- local.df %>% group_by(state) %>% mutate(date = date - min(date))
        }
        else{
            local.df <- local.df[local.df$date - min(local.df$date) > input.settings$start, ]
        }
        
        local.colors <- unlist(these.colors[unique(local.df$state)])
        if(length(local.colors) == 0)
            local.colors <- colors[1]
        
        if(length(highlights) > 0) {
            sapply(names(local.colors), function(x) if(!(x %in% highlights)) {local.colors[x] <<- '#DEDEDE'})
        }
        
        p <- ggplot()
        
        # define base sizes
        base.size <- 14
        point.size <- 3.5
        line.size <- 1.5
        font.size <- 16
        
        # change sizes for plotly
        if(plotly.settings) {
            base.size <- 12
            point.size <- 2.0
            line.size <- 1.0
            font.size <- 13
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
        
        if(plotly.settings)
            p <- p + theme(legend.position = 'none')
        
        tooltip.label <- ''
        if(s == 'positive')
            tooltip.label <- 'Aggregate Cases:'
        if(s == 'negative')
            tooltip.label <- 'Aggregate Negatives:'
        if(s == 'pending')
            tooltip.label <- 'Aggregate Pending:'
        if(s == 'hospitalized')
            tooltip.label <- 'Aggregate Hospitalizations:'
        if(s == 'death')
            tooltip.label <- 'Aggregate Deaths:'
        if(s == 'totalTestResults')
            tooltip.label <- 'Aggregate Test:'
        if(s == 'positiveIncrease')
            tooltip.label <- 'Daily Cases:'
        if(s == 'negativeIncrease')
            tooltip.label <- 'Daily Negatives:'
        if(s == 'hospitalizedIncrease')
            tooltip.label <- 'Daily Hospitalizations:'
        if(s == 'deathIncrease')
            tooltip.label <- 'Daily Deaths:'
        if(s == 'totalTestResultsIncrease')
            tooltip.label <- 'Daily Tests:'
        
        tooltip.func <- function(dat) {
            this.list <- unlist(lapply(1:nrow(dat), function(i) paste('State:', dat$state[i], '\n', 
                                                                      'Day:', dat$date[i], '\n',
                                                                      tooltip.label, as.character(dat[[s]][i])))
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
                      alpha = 0.3) + 
            geom_point(data = plottable.df,
                       aes(x = date, 
                           y = .data[[s]], 
                           color = state,
                           fill = state,
                           text = tooltip.func(plottable.df)),
                       size = point.size, 
                       alpha = 0.6) +
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
        
        # Build fit if requested
        if(as.logical(these.data$do.fit)) {
            if(length(highlights > 0))
                fit.df <- local.df[local.df$state %in% highlights, ]
            else
                fit.df <- local.df
            
            fit.func <- function(tmp.s) {
                if(min(tmp.s[[s]]) == 0) {
                    t.f.df <- tibble(x = tmp.s$date, y = tmp.s[[s]] + 1)
                    
                    t.f <- lm(log10(y) ~ x, data = t.f.df)
                    
                    t.p.df <- tibble(x = t.f.df$x, 
                                     new.y = 10^predict(t.f) - 1, 
                                     state = tmp.s$state, 
                                     d.t = rep(log10(2) / coef(t.f)[2], nrow(t.f.df)))
                }
                else {
                    t.f.df <- tibble(x = tmp.s$date, y = tmp.s[[s]])
                    t.f <- lm(log10(y) ~ x, data = t.f.df)
                    
                    t.p.df <- tibble(x = t.f.df$x, 
                                     new.y = 10^predict(t.f), 
                                     state = tmp.s$state, 
                                     d.t = rep(log10(2) / coef(t.f)[2], nrow(t.f.df)))
                }
                return(t.p.df)
            }
            
            build.ano <- function(t.p) {
                tmp <- t.p %>% group_by(state, d.t) %>% summarise()
                captions <- unlist(lapply(1:nrow(tmp), function(x) paste(as.character(tmp$state[x]),
                                                                         as.character('doubling time:'), 
                                                                         as.character(round(tmp$d.t[x], digits = 2)),
                                                                         'days')))
                return(captions)
            }
            
            predicted.df <- tibble(x = numeric(), new.y = numeric(), state = character(), d.t = numeric())
            lapply(fit.df$state, function(x) predicted.df <<- rbind(predicted.df, 
                                                                    fit.func(fit.df[fit.df$state == x, ])))
            dist <- log10(max(local.df[[s]])) - log10(0.93 * (max(local.df[[s]])))
            
            lseq <- unlist(lapply(1:length(unique(predicted.df$state)), 
                                  function(x) (log10(max(local.df[[s]])) - 
                                                   log10(max(local.df[[s]])) * x * dist)))
            
            lseq <- rescale(lseq, 
                            to = c(ifelse(log10(min(local.df[[s]])) < 0, 0, log10(min(local.df[[s]]))), log10(max(local.df[[s]]))), 
                            from = c(1, log10(max(local.df[[s]]))))
            
            p <- p + geom_line(data = predicted.df,
                               aes(x = x, 
                                   y = new.y,
                                   color = state), 
                               alpha = 0.8,
                               size = line.size * 0.9)
            if(plotly.settings)
                p <- p + annotate('text', 
                                  x = min(predicted.df$x) + 5, 
                                  y = 10^(lseq + 2*dist),
                                  label = build.ano(predicted.df),
                                  vjust = 1,
                                  hjust = 0,
                                  size = 3)
            else
                p <- p + annotate('text', 
                                  x = min(predicted.df$x), 
                                  y = 10^(lseq + 2*dist),
                                  label = build.ano(predicted.df),
                                  vjust = 1,
                                  hjust = 0,
                                  size = 4)
        }
        
        if(as.logical(these.data$exp)) {
            this.max.x <- max(exp.df$date)
            this.max.y.multi <- 1.1
            this.size <- 6
            this.increment <- 0.5
            
            if(plotly.settings) {
                this.max.x <- max(exp.df$date) * 0.9
                this.max.y.multi <- 1.2
                this.size <- 4
                this.increment <- 0.25
            }
            
            p <- p + geom_line(data = exp.df,
                               aes(x = date, 
                                   y = y, 
                                   group = ds), 
                               color = 'gray50',
                               alpha = 0.8,
                               size = line.size * 0.9,
                               linetype = "dashed") +
                annotate("text",
                         x = this.max.x,
                         y = this.max.y.multi * max(exp.df$y[exp.df$ds == '1 day']),
                         label = "doubling every day",
                         size = this.size,
                         hjust = 1,
                         vjust = 0,
                         color = 'gray50',
                         alpha = 1) +
                annotate("text",
                         x = this.max.x,
                         y = this.max.y.multi * max(exp.df$y[exp.df$ds == '2 days']),
                         label = "doubling every 2 days",
                         size = this.size - this.increment * 1,
                         hjust = 1,
                         vjust = -0.25,
                         color = 'gray50',
                         alpha = 1) +
                annotate("text",
                         x = this.max.x,
                         y = this.max.y.multi * max(exp.df$y[exp.df$ds == '3 days']),
                         label = "doubling every 3 days",
                         size = this.size - this.increment * 2,
                         hjust = 1,
                         vjust = -0.25,
                         color = 'gray50',
                         alpha = 1) +
                annotate("text",
                         x = this.max.x,
                         y = this.max.y.multi * max(exp.df$y[exp.df$ds == '5 days']),
                         label = "doubling every 5 days",
                         size = this.size - this.increment * 3,
                         hjust = 1,
                         vjust = -0.25,
                         color = 'gray50',
                         alpha = 1) +
                annotate("text",
                         x = this.max.x,
                         y = this.max.y.multi * max(exp.df$y[exp.df$ds == '7 days']),
                         label = "doubling every 7 days",
                         size = this.size - this.increment * 4,
                         hjust = 1,
                         vjust = -0.25,
                         color = 'gray50',
                         alpha = 1)
        }
        
        if(length(highlights) > 0) {
            highlights.df <- local.df[local.df$state %in% highlights, ]
            p <- p + geom_line(data = highlights.df,
                               aes(x = date,
                                   y = .data[[s]],
                                   color = state),
                               size = line.size,
                               alpha = 0.8) + 
                geom_point(data = highlights.df,
                           aes(x = date, 
                               y = .data[[s]], 
                               color = state,
                               fill = state,
                               text = tooltip.func(highlights.df)),
                           size = point.size,
                           alpha = 0.9)
        }
        
        if(as.logical(these.data$align))
            p <- p + xlab(paste('Days since alignment number'))
        
        if(s == 'positive')
            p <- p + ylab('Daily aggregate number of COVID-19 positive tests')
        if(s == 'negative')
            p <- p + ylab('Daily aggregate number of COVID-19 negative tests')
        if(s == 'pending')
            p <- p + ylab('Daily aggregate number of COVID-19 pending tests')
        if(s == 'hospitalized')
            p <- p + ylab('Daily aggregate number of COVID-19 hospitalized patients')
        if(s == 'death')
            p <- p + ylab('Daily aggregate number of COVID-19 deaths')
        if(s == 'totalTestResults')
            p <- p + ylab('Daily aggregate number of COVID-19 tests run')
        if(s == 'positiveIncrease')
            p <- p + ylab('Daily number of COVID-19 positive tests')
        if(s == 'negativeIncrease')
            p <- p + ylab('Daily number of COVID-19 negative tests')
        if(s == 'hospitalizedIncrease')
            p <- p + ylab('Daily number of COVID-19 hospitalized patients')
        if(s == 'deathIncrease')
            p <- p + ylab('Daily number of COVID-19 deaths')
        if(s == 'totalTestResultsIncrease')
            p <- p + ylab('Daily number of COVID-19 tests run')
        
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
        us_sf$mid <- us_sf$geometry
        
        if(s == 'positive')
            this.legend.title <- 'Daily aggregate number of COVID-19 positive tests'
        if(s == 'negative')
            this.legend.title <- 'Daily aggregate number of COVID-19 negative tests'
        if(s == 'pending')
            this.legend.title <- 'Daily aggregate number of COVID-19 pending tests'
        if(s == 'hospitalized')
            this.legend.title <- 'Daily aggregate number of COVID-19 hospitalized patients'
        if(s == 'death')
            this.legend.title <- 'Daily aggregate number of COVID-19 deaths'
        if(s == 'totalTestResults')
            this.legend.title <- 'Daily aggregate number of COVID-19 tests run'
        if(s == 'positiveIncrease')
            this.legend.title <- "Daily number of COVID-19 positive tests"
        if(s == 'negativeIncrease')
            this.legend.title <- "Daily number of COVID-19 negative tests"
        if(s == 'hospitalizedIncrease')
            this.legend.title <- "Daily number of COVID-19 hospitalized patients"
        if(s == 'deathIncrease')
            this.legend.title <- "Daily number of COVID-19 deaths"
        if(s == 'totalTestResultsIncrease')
            this.legend.title <- "Daily number of COVID-19 tests run"
        
        if(s == 'positive')
            tooltip.label <- 'Aggregate Cases:'
        if(s == 'negative')
            tooltip.label <- 'Aggregate Negatives:'
        if(s == 'pending')
            tooltip.label <- 'Aggregate Pending:'
        if(s == 'hospitalized')
            tooltip.label <- 'Aggregate Hospitalizations:'
        if(s == 'death')
            tooltip.label <- 'Aggregate Deaths:'
        if(s == 'totalTestResults')
            tooltip.label <- 'Aggregate Test Results:'
        if(s == 'positiveIncrease')
            tooltip.label <- 'Cases Today:'
        if(s == 'negativeIncrease')
            tooltip.label <- 'Negatives Today:'
        if(s == 'hospitalizedIncrease')
            tooltip.label <- 'Hospitalizations Today:'
        if(s == 'deathIncrease')
            tooltip.label <- 'Deaths Today:'
        if(s == 'totalTestResultsIncrease')
            tooltip.label <- 'Test Results Today:'
        
        tooltip.func <- function(dat) {
            this.list <- unlist(lapply(1:nrow(dat), function(i) paste(dat$name[i], '\n', 
                                                                      tooltip.label, as.character(dat[[s]][i]))))
            return(this.list)
        }
        
        p <- ggplot(us_sf) + 
            geom_sf(colour = "white") +
            geom_sf_interactive(aes(geometry = mid,
                                    fill = .data[[s]],
                                    data_id = state,
                                    tooltip = tooltip.func(us_sf))
            ) +
            theme_map(24) +
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
                    barwidth = grid::unit(6.0, "in"),
                    barheight = grid::unit(0.6, "in")
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
                    barwidth = grid::unit(6.0, "in"),
                    barheight = grid::unit(0.6, "in")
                )
            )
        
        return(p)
    }
    
    shuffleColors <- eventReactive(input$shuffle_colors, {
        new.cols <<- iwanthue(length(unique(state.df$state)), random = T)
        sapply(1:length(unique(state.df$state)), function(x) colors.list[unique(state.df$state)[x]] <<- new.cols[x])
    })
    
    inputData <- isolate({reactive({
        input.settings <<- list(state = input$stateChoice,
                                series = input$seriesChoice,
                                do.fit = input$fitChoice,
                                transformation = input$transformation,
                                highlights = input$highlightSet,
                                align = input$align,
                                num_align = input$num_align,
                                facet = input$facet,
                                exp = input$exponentials,
                                smooth = input$smooth,
                                start = input$start)
    }) %>% debounce(1500)})
    
    build.plots <- function() {
        this.validate <- function() {
            validate(
                need(input.settings$smooth > 0, 
                     "Cannot smooth over less than 1 day."),
                need(length(input.settings$state) > 0, 
                     "Please select a state set."),
                need(all(input.settings$highlights %in% input.settings$state)  | 
                         length(input.settings$highlights) == 0, 
                     "Please ensure highlights are in states selected."),
                need(length(input.settings$state) != length(input.settings$highlights),
                     "No need to highlight all of the selected states Unselect highlights."),
                need(!(as.logical(input.settings$exp) & 
                           as.logical(input.settings$do.fit)), 
                     "Cannot fit the data and place exponential guides. Pick one or the other."),
                if(as.logical(input.settings$exp))
                    need(input.settings$transformation == 'log10' &
                             as.logical(input.settings$align),
                         "Data must be aligned and Log10 selected to plot fits."),
                if(as.logical(input.settings$do.fit))
                    need(input.settings$transformation == 'log10' &
                             as.logical(input.settings$align),
                         "Data must be aligned and Log10 selected to plot guides."),
                if(as.logical(input.settings$do.fit) | as.logical(input.settings$exp))
                    need(!as.logical(input.settings$facet), 
                         "Cannot place guides or fits on faceted plot.")
            )
        }
        
        output$casesPlot <- renderPlot({
            this.validate()
            renderTimeSeries(input.settings, colors.list)
        })
        
        if(!as.logical(input.settings$facet)) {
            output$casesPlotly <- renderPlotly({
                this.validate()
                gg.p <- ggplotly(renderTimeSeries(input.settings, 
                                                  colors.list, 
                                                  plotly.settings = T),
                                 height = 1000 * 5 / 7,
                                 tooltip = c('text')) %>%
                    layout(font = list(family = 'Arial'),
                           xaxis = list(title = list(standoff = 15, font = list(size = 18)), 
                                        tickfont = list(size = 18),
                                        automargin = T),
                           yaxis = list(title = list(standoff = 15, font = list(size = 18)), 
                                        tickfont = list(size = 18),
                                        automargin = T))
                
                return(gg.p)
            })}
        else {
            output$casesPlotly <- renderPlotly({
                this.validate()
                gg.p <- ggplotly(renderTimeSeries(input.settings, 
                                                  colors.list, 
                                                  plotly.settings = T),
                                 height = 1000 * 5 / 7,
                                 tooltip = c('text'))
                if(as.logical(input$align)) {
                    gg.p[["x"]][["layout"]][["annotations"]][[1]][["y"]] <- -0.06
                    gg.p[["x"]][["layout"]][["annotations"]][[2]][["x"]] <- -0.08
                }
                else {
                    gg.p[["x"]][["layout"]][["annotations"]][[1]][["x"]] <- -0.08
                }
                
                gg.p <- layout(gg.p,
                               font = list(family = 'Arial'),
                               margin = list(l = 85, b = 70))
                
                return(gg.p)
            })
        }
        
        output$mapPlot <- renderGirafe({
            this.validate()
            girafe(ggobj = renderMap(input.settings),
                   width_svg = 20,
                   height_svg = 20 * 5 / 7,
                   options = list(opts_selection(type = "single", only_shiny = FALSE)))
        })
        
    }
    
    observe({
        inputData()
        build.plots()
    })
    
    observe({
        shuffleColors()
        build.plots()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
