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