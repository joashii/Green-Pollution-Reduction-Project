# ui.R
# User Interface for Pollution Reduction Optimization (4-Tab Structure)

ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  # Include custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
    /* Completely remove Shiny's default title + nav tabs layout */
    .container-fluid > h2 {
      display: none !important;
    }
    .nav-tabs {
      display: none !important;
    }
    body {
      padding: 0 !important;
    }
    .container-fluid, .tab-content {
      padding: 0 !important;
    }
  "))
  ),
  
  
  # Main Tab Panel
  tabsetPanel(
    id = "main_tabs",
    type = "hidden",
    
    # ===== TAB 1: WELCOME =====
    
    tabPanel(
      "Welcome",
      value = "welcome_tab",
      
      div(
        class = "welcome-container bg-1",
        id = "welcome_bg_container",
        
        div(
          class = "welcome-overlay",
          
          # Top Navigation Bar
          div(
            class = "welcome-nav",
            div(
              class = "welcome-nav-buttons",
              actionButton("nav_main_btn", "Main", class = "nav-button"),
              actionButton("nav_credits_btn", "Credits", class = "nav-button"),
              actionButton("nav_about_btn", "About this App", class = "nav-button active")
            )
          ),
          
          # Logo
          div(class = "welcome-logo"),
          
          # Centered Images Wrapper
          div(
            class = "center-wrapper",
            div(class = "header"),
            div(class = "plant"),
            div(class = "subtitle", "Pollution Reduction System"),
            div(class = "subtitle-text", "Pollution Reduction System")
          ),
          
          # Sunlight Effects
          div(
            class = "sunlight-effects",
            div(class = "light-beam light-beam-1"),
            div(class = "light-beam light-beam-2"),
            div(class = "light-beam light-beam-3"),
            div(class = "ambient-glow")
          ),
          
          # Firefly Bubbles (30 total)
          div(
            class = "bubble-container",
            div(class = "bubble bubble-small"),
            div(class = "bubble bubble-medium"),
            div(class = "bubble bubble-large"),
            div(class = "bubble bubble-small"),
            div(class = "bubble bubble-medium"),
            div(class = "bubble bubble-extra-large"),
            div(class = "bubble bubble-small"),
            div(class = "bubble bubble-medium"),
            div(class = "bubble bubble-large"),
            div(class = "bubble bubble-small"),
            div(class = "bubble bubble-medium"),
            div(class = "bubble bubble-large"),
            div(class = "bubble bubble-small"),
            div(class = "bubble bubble-extra-large"),
            div(class = "bubble bubble-medium"),
            div(class = "bubble bubble-medium"),
            div(class = "bubble bubble-small"),
            div(class = "bubble bubble-large"),
            div(class = "bubble bubble-extra-large"),
            div(class = "bubble bubble-small"),
            div(class = "bubble bubble-medium"),
            div(class = "bubble bubble-large"),
            div(class = "bubble bubble-small"),
            div(class = "bubble bubble-medium"),
            div(class = "bubble bubble-small"),
            div(class = "bubble bubble-large"),
            div(class = "bubble bubble-medium"),
            div(class = "bubble bubble-small"),
            div(class = "bubble bubble-extra-large"),
            div(class = "bubble bubble-medium")
          ),
          
          # Get Started Button
          div(
            class = "welcome-cta",
            actionButton("get_started_btn", "Get Started", class = "cta-button")
          ),
          
          # Modals
          uiOutput("credits_modal"),
          uiOutput("about_modal")
        )
      )
    ),
    
    # ===== TAB 2: PROJECT SELECTION =====
    
    tabPanel(
      "Project Selection",
      value = "selection_tab",
      
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "project-selection.css")
      ),
      
      div(
        class = "project-selection-container",
        
        # Upper-upper-left
        div(
          class = "upper-upper-left",
                    # Header with info icon and return 
          div(
            class = "panel-header",
            
            div(
              class = "return-icon",
              title = "Return to Welcome",
              onclick = "Shiny.setInputValue('go_back_welcome', Math.random())"
            ),
            
            div(
              class = "info-icon",
              title = "View Instructions",
              onclick = "Shiny.setInputValue('show_instructions_btn', Math.random())"
            )
          ),
        ),
        
        # Upper Left Panel
        div(
          class = "upper-left",
          
          # Instructions
          div(
            class = "instructions-content",
            tags$ol(
              tags$li("Select mitigation projects from the grid"),
              tags$li("Reset if you want all to deselect"),
              tags$li("Review target pollutants and costs"),
              tags$li("Click 'Run Optimization' to find solution")
            )
          ),
          
          # Control Buttons
          div(
            class = "buttons-section",
            
            div(
              class = "button-row",
              tags$button(
                id = "select_all",
                class = "control-btn",
                onclick = "Shiny.setInputValue('select_all', Math.random())",
                "Select All"
              ),
              tags$button(
                id = "reset_all",
                class = "control-btn",
                onclick = "Shiny.setInputValue('reset_all', Math.random())",
                "Reset"
              )
            ),
            
            tags$button(
              id = "solve_btn",
              class = "run-optimization-btn",
              onclick = "Shiny.setInputValue('solve_btn', Math.random())",
              "Run Optimization"
            )
          )
        ),
        
        # Bottom Left - Target Pollutants
        div(
          class = "bottom-left",
          uiOutput("target_pollutants_grid")
        ),
        
        # Center - Projects Grid
        div(
          class = "center",
          uiOutput("center_projects_grid")
        ),
        
        # Upper Right - Optimization Summary
        div(
          class = "upper-right",
          uiOutput("optimization_summary")
        ),
        
        # Bottom Right (reserved for future use)
        div(class = "bottom-right"),
        
        # Results Modal
        uiOutput("results_modal"),
        
        # Project Info Modal (per project)
        uiOutput("project_info_modal"),
        
        # Full Instructions Modal
        uiOutput("instructions_modal")
      )
    ),
    
    # ===== TAB 3: ITERATIONS =====
    
    tabPanel(
      "Simplex Iterations",
      value = "iterations_tab",
      
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "iterations.css")
      ),
      
      div(
        class = "iterations-container",
        
        # Upper Left - Return Home Button
        div(
          class = "upper-left-it",
          actionButton(
            "return_from_iterations",
            "Return Home",
            class = "return-home-btn",
            onclick = "Shiny.setInputValue('back_to_selection', Math.random())"
          )
        ),
        
        # Right - Iteration Viewer
        div(
          class = "right-it",
          
          # No iterations available
          conditionalPanel(
            condition = "!output.iterations_available",
            div(
              class = "no-iterations-container",
              div(class = "no-iterations-icon", "📊"),
              h3(class = "no-iterations-title", "No Iterations to Display"),
              p(class = "no-iterations-text", 
                "Run the optimization to see the detailed solving process here.")
            )
          ),
          
          # Iterations available
          conditionalPanel(
            condition = "output.iterations_available",
            div(
              class = "iterations-viewer",
              uiOutput("iteration_selector"),   # Dropdown + nav buttons
              uiOutput("iterations_display")    # DataTable display
            )
          )
        )
      )
    ),
  )
)