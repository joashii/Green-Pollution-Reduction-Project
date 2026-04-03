# main.R
packages <- c("shiny", "DT", "shinyjs")

for (p in packages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
    library(p, character.only = TRUE)
  }
}

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