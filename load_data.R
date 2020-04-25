library('tidyverse')
library('lubridate')

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