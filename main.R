# main.R
# Entry point for the Pollution Reduction Optimization Shiny App

library(shiny)
library(DT)
library(shinyjs)

# Force shiny to launch in browser
options(shiny.launch.browser = TRUE)

# Load backend
source("src/data.R", local = TRUE)
source("src/optimization_core.R", local = TRUE)
source("src/solver_interface.R", local = TRUE)

# UI + server
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

# Run the app
shinyApp(ui = ui, server = server)