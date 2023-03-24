
library(shiny)
library(reactlog)
library(here)

# tell shiny to log all reactivity
reactlog_enable()

# run a shiny app
# once app has closed, display reactlog from shiny
shiny::reactlogShow()