# server.R - Cleaned Version
# Server logic for Pollution Reduction Optimization

server <- function(input, output, session) {
  
  # ===== REACTIVE VALUES =====
  
  # Store optimization results
  solution_results <- reactiveVal(NULL)
  iterations_text <- reactiveVal("")
  
  # Modal state for Welcome tab
  modal_state <- reactiveValues(
    credits_open = FALSE,
    about_open = FALSE,
    instructions_open = FALSE   
  )
  
  # Project selection state
  project_state <- reactiveValues(
    selected_projects = character(0),
    select_all_active = FALSE,
    reset_active = FALSE,
    optimize_active = FALSE,
    results_modal_open = FALSE,
    
    project_info_open = FALSE,
    current_project_index = NA_integer_
  )
  
  # ===== TAB CLASS TOGGLE =====
  observe({
    if (input$main_tabs == "welcome_tab") {
      runjs("$('body')
      .addClass('welcome-active')
      .removeClass('selection-active')
      .removeClass('iterations-active');")
      
    } else if (input$main_tabs == "selection_tab") {
      runjs("$('body')
      .addClass('selection-active')
      .removeClass('welcome-active')
      .removeClass('iterations-active');")
      
    } else if (input$main_tabs == "iterations_tab") {
      runjs("$('body')
      .addClass('iterations-active')
      .removeClass('welcome-active')
      .removeClass('selection-active');")
    }
  })
  
  
  
  # ===== DYNAMIC BUTTON CLASSES =====
  
  observe({
    if (project_state$select_all_active) {
      runjs("$('#select_all').addClass('active-glow');")
    } else {
      runjs("$('#select_all').removeClass('active-glow');")
    }
  })
  
  observe({
    if (project_state$reset_active) {
      runjs("$('#reset_all').addClass('active-glow');")
    } else {
      runjs("$('#reset_all').removeClass('active-glow');")
    }
  })
  
  observe({
    if (project_state$optimize_active) {
      runjs("$('#solve_btn').addClass('active-glow');")
    } else {
      runjs("$('#solve_btn').removeClass('active-glow');")
    }
  })
  
  # ===== WELCOME TAB NAVIGATION =====
  
  observeEvent(input$get_started_btn, {
    updateTabsetPanel(session, "main_tabs", selected = "selection_tab")
  })
  
  observeEvent(input$nav_main_btn, {
    updateTabsetPanel(session, "main_tabs", selected = "selection_tab")
  })
  
  observeEvent(input$nav_credits_btn, {
    modal_state$credits_open <- TRUE
    modal_state$about_open <- FALSE
  })
  
  observeEvent(input$nav_about_btn, {
    modal_state$about_open <- TRUE
    modal_state$credits_open <- FALSE
  })
  
  observeEvent(input$close_credits_btn, {
    modal_state$credits_open <- FALSE
  })
  
  observeEvent(input$close_credits_backdrop, {
    modal_state$credits_open <- FALSE
  })
  
  observeEvent(input$close_about_btn, {
    modal_state$about_open <- FALSE
  })
  
  observeEvent(input$close_about_backdrop, {
    modal_state$about_open <- FALSE
  })
  
  # ===== PROJECT SELECTION – INSTRUCTIONS MODAL TOGGLE =====
  
  # Open instructions when info icon is clicked
  observeEvent(input$show_instructions_btn, {
    modal_state$instructions_open <- TRUE
  })
  
  # Close via X button
  observeEvent(input$close_instructions_btn, {
    modal_state$instructions_open <- FALSE
  })
  
  # Close via backdrop click
  observeEvent(input$close_instructions_backdrop, {
    modal_state$instructions_open <- FALSE
  })
  
  
  # ===== PROJECT SELECTION TAB =====
  
  # Render optimization summary (upper-right section)
  output$optimization_summary <- renderUI({
    results <- solution_results()
    
    if (is.null(results)) {
      return(
        div(
          class = "summary-placeholder",
          div(class = "placeholder-icon", "📊"),
          div(class = "placeholder-text", "No results yet"),
          div(class = "placeholder-subtext", "Run optimization to see results")
        )
      )
    }
    
    if (results$status == "infeasible") {
      return(
        div(
          class = "summary-content",
          div(class = "status-badge infeasible", "INFEASIBLE"),
          div(class = "summary-message infeasible-msg", 
              "Cannot meet all pollution targets with selected projects"),
          div(class = "summary-suggestion", 
              "Try selecting more projects")
        )
      )
    }
    
    div(
      class = "summary-content",
      div(class = "status-badge feasible", "FEASIBLE"),
      div(
        class = "summary-stat-large",
        div(class = "stat-label", "OPTIMAL COST"),
        div(class = "stat-value-large", 
            paste0("$", format(results$optimal_cost, big.mark = ",", nsmall = 2)))
      ),
      div(
        class = "summary-stat-row",
        div(class = "stat-label-small", "PROJECTS USED"),
        div(class = "stat-value-small", results$num_projects_selected),
        tags$button(
          id = "view_iterations_summary",
          class = "iterations-icon-btn",
          onclick = "Shiny.setInputValue('view_iterations_summary', Math.random())",
          title = "View Simplex Iterations"
        )
      )
    ) 
  })
  
  observeEvent(input$view_iterations_summary, {
    updateTabsetPanel(session, "main_tabs", selected = "iterations_tab")
  })
  
  # Generate project cards for CENTER section
  output$center_projects_grid <- renderUI({
    if (!exists("projects")) {
      return(p("Error: Projects data not loaded!", style = "color: white;"))
    }
    
    cards <- lapply(1:nrow(projects), function(i) {
      project_name <- projects$Project[i]
      project_cost <- projects$Cost[i]
      is_selected <- project_name %in% project_state$selected_projects
      
      div(
        class = paste("center-project-card", if(is_selected) "selected" else ""),
        id = paste0("center_card_", i),
        onclick = paste0("Shiny.setInputValue('toggle_project_", i, "', Math.random())"),
        div(class = "center-card-name", project_name),
        div(
          class = "center-card-price",
          paste0("$", format(project_cost, big.mark = ",", scientific = FALSE))
        ),
        div(
          class = "center-card-info",
          onclick = paste0("event.stopPropagation(); Shiny.setInputValue('show_project_", i, "', Math.random())"),
          "i"
        )
      )
    })
    
    div(class = "center-projects-grid", tagList(cards))
  })
  
  # ===== LEFT PANEL FUNCTIONALITY =====
  
  # Render target pollutants table
  output$target_pollutants_grid <- renderUI({
    if (!exists("targets")) {
      return(p("No targets loaded", style = "color: white;"))
    }
    
    total_pollutants <- nrow(targets)
    half <- ceiling(total_pollutants / 2)
    
    left_pollutants <- targets[1:half, ]
    right_pollutants <- targets[(half + 1):total_pollutants, ]
    
    table_rows <- lapply(1:half, function(i) {
      div(
        class = "pollutant-row",
        div(class = "pollutant-cell pollutant-name", 
            left_pollutants$Pollutant[i]),
        div(class = "pollutant-cell pollutant-value", 
            paste0(format(left_pollutants$Target[i], big.mark = ",", scientific = FALSE), " tons")),
        div(class = "pollutant-cell pollutant-name", 
            if(i <= nrow(right_pollutants)) right_pollutants$Pollutant[i] else ""),
        div(class = "pollutant-cell pollutant-value", 
            if(i <= nrow(right_pollutants)) paste0(format(right_pollutants$Target[i], big.mark = ",", scientific = FALSE), " tons") else "")
      )
    })
    
    div(class = "pollutants-table", table_rows)
  })
  
  # Toggle project selection
  observe({
    lapply(1:30, function(i) {
      observeEvent(input[[paste0("toggle_project_", i)]], {
        project_name <- projects$Project[i]
        
        if (project_name %in% project_state$selected_projects) {
          project_state$selected_projects <- setdiff(project_state$selected_projects, project_name)
        } else {
          project_state$selected_projects <- c(project_state$selected_projects, project_name)
        }
      })
    })
  })
  
  # ===== PROJECT INFO MODAL - OPEN ON "i" CLICK =====
  observe({
    if (!exists("projects")) return(NULL)
    
    n_projects <- nrow(projects)
    
    lapply(1:n_projects, function(i) {
      local_i <- i  # avoid closure issue
      
      observeEvent(input[[paste0("show_project_", local_i)]], {
        project_state$current_project_index <- local_i
        project_state$project_info_open <- TRUE
      }, ignoreInit = TRUE)
    })
  })
  
  
  # Select All button
  observeEvent(input$select_all, {
    project_state$selected_projects <- projects$Project
    project_state$select_all_active <- TRUE
    project_state$reset_active <- FALSE
    project_state$optimize_active <- FALSE
  })
  
  # Reset button
  observeEvent(input$reset_all, {
    project_state$selected_projects <- character(0)
    solution_results(NULL)
    iterations_text("")
    project_state$select_all_active <- FALSE
    project_state$reset_active <- TRUE
    project_state$optimize_active <- FALSE
  })
  
  # ===== SOLVE BUTTON =====
  observeEvent(input$solve_btn, {
    selected_projects <- project_state$selected_projects
    
    project_state$optimize_active <- TRUE
    project_state$select_all_active <- FALSE
    project_state$reset_active <- FALSE
    
    if (length(selected_projects) == 0) {
      showNotification(
        "Please select at least one project before running optimization.",
        type = "warning",
        duration = 3
      )
      project_state$optimize_active <- FALSE
      return()
    }
    
    constraints <- build_constraints(selected_projects = selected_projects)
    primal_matrix <- build_primal_matrix(constraints)
    
    solution_raw <- tryCatch({
      simplex_solve(primal_matrix, maximize = FALSE)
    }, error = function(e) {
      showNotification(
        paste("Error solving:", e$message),
        type = "error",
        duration = 10
      )
      return(NULL)
    })
    
    if (!is.null(solution_raw) && !is.null(solution_raw$iterations)) {
      iterations_text(solution_raw$iterations)
    } else {
      iterations_text(list())
    }
    
    results <- pollution_solver(selected_projects = selected_projects)
    solution_results(results)
    
    if (!is.null(results)) {
      project_state$results_modal_open <- TRUE
    }
  })
  
  # ===== RENDER RESULTS MODAL =====
  
  output$results_modal <- renderUI({
    if (is.null(project_state$results_modal_open) || !project_state$results_modal_open) {
      return(NULL)
    }
    
    results <- solution_results()
    
    if (is.null(results)) {
      return(NULL)
    }
    
    # Get the list of selected projects (user input)
    selected_project_names <- project_state$selected_projects
    
    # INFEASIBLE SOLUTION
    if (results$status == "infeasible") {
      # Create grid of selected projects with icons
      project_items <- lapply(selected_project_names, function(proj) {
        div(
          class = "results-project-item infeasible-item",
          div(class = "project-item-icon", ""),
          div(class = "project-item-name", proj)
        )
      })
      
      return(
        tagList(
          div(
            class = "results-modal-backdrop",
            onclick = "Shiny.setInputValue('close_results_backdrop', Math.random())"
          ),
          div(
            class = "results-modal enhanced-modal",
            
            # Animated background particles
            div(
              class = "modal-particles",
              lapply(1:8, function(i) div(class = paste0("particle particle-", i)))
            ),
            
            div(
              class = "results-modal-header",
              div(class = "header-decoration"),
              h2(class = "results-modal-title", 
                 tags$span(class = "title-icon", ""), 
                 " Optimization Results"),
              p(class = "results-modal-subtitle", "Greenvale Pollution Reduction Analysis"),
              tags$button(
                class = "results-modal-close",
                onclick = "Shiny.setInputValue('close_results_modal', Math.random())",
                "×"
              )
            ),
            div(
              class = "results-modal-body",
              
              # Your Input Section with animation
              div(
                class = "results-input-section slide-in-up",
                div(class = "section-header",
                    h3(class = "results-section-title", 
                       tags$span(class = "section-icon", ""), 
                       " Your Input"),
                    div(class = "section-badge", 
                        paste0(length(selected_project_names), " Projects Selected"))
                ),
                div(class = "results-project-grid", project_items)
              ),
              
              # Status Section with pulse animation
              div(
                class = "results-status-section slide-in-up delay-1",
                div(class = "status-icon-container infeasible-icon",
                    tags$span(class = "status-icon-large", "")
                ),
                div(class = "results-status-badge infeasible pulse-animation", "INFEASIBLE"),
                div(class = "status-divider")
              ),
              
              # Infeasible Message with illustration
              div(
                class = "results-infeasible-message slide-in-up delay-2",
                div(class = "message-icon", ""),
                h3(class = "results-infeasible-title", "The Problem is Infeasible"),
                p(class = "results-infeasible-text", 
                  "The selected projects cannot reduce all pollutants to the required targets."),
                div(class = "infeasible-suggestions",
                    h4(class = "suggestions-title", "💡 Suggestions:"),
                    tags$ul(
                      tags$li("Try selecting more mitigation projects"),
                      tags$li("Include projects with diverse pollutant coverage"),
                      tags$li("Consider high-impact options like Industrial Scrubbers or Rail Electrification")
                    )
                )
              )
            ),
            div(
              class = "results-modal-actions",
              tags$button(
                class = "results-action-btn secondary-btn",
                onclick = "Shiny.setInputValue('close_results_modal', Math.random())",
                tags$span(class = "btn-icon", ""), " Back to Selection"
              )
            )
          )
        )
      )
    }
    
    # FEASIBLE SOLUTION
    selected_df <- results$selected_projects_df
    
    # Create grid of ALL selected projects with checkmarks
    all_project_items <- lapply(selected_project_names, function(proj) {
      is_used <- proj %in% selected_df$Project
      div(
        class = paste("results-project-item", if(is_used) "used-project" else "unused-project"),
        div(class = "project-item-icon", if(is_used) "" else ""),
        div(class = "project-item-name", proj)
      )
    })
    
    # Create enhanced project cards for projects WITH units > 0
    project_cards <- lapply(1:nrow(selected_df), function(i) {
      project <- selected_df[i, ]
      
      div(
        class = paste("results-project-card enhanced-card", if(i == 1) "slide-in-left" else if(i == 2) "slide-in-left delay-1" else "slide-in-left delay-2"),
        
        # Card glow effect
        div(class = "card-glow"),
        
        # Rank badge
        div(class = "project-rank-badge", paste0("#", i)),
        
        div(
          class = "results-project-header",
          div(
            class = "project-name-container",
            div(class = "project-icon", ""),
            div(class = "results-project-name", project$Project)
          ),
          div(class = "results-project-units", 
              div(class = "units-number", format(project$Units, nsmall = 2)),
              div(class = "units-label", "units"))
        ),
        
        div(class = "project-details-divider"),
        
        div(
          class = "results-project-details",
          div(class = "detail-row",
              div(class = "detail-label", "Unit Cost:"),
              div(class = "results-project-cost", 
                  paste0("$", format(project$Cost_per_Unit, big.mark = ",", scientific = FALSE)))
          ),
          div(class = "detail-row total-row",
              div(class = "detail-label", "Total Cost:"),
              div(class = "results-project-total", 
                  paste0("$", format(project$Total_Cost, big.mark = ",", nsmall = 2)))
          )
        ),
        
        # Progress bar showing contribution
        div(class = "project-contribution-bar",
            div(class = "contribution-fill", 
                style = paste0("width: ", 
                               min(100, (project$Total_Cost / results$optimal_cost) * 100), "%"))
        )
      )
    })
    
    tagList(
      div(
        class = "results-modal-backdrop",
        onclick = "Shiny.setInputValue('close_results_backdrop', Math.random())"
      ),
      div(
        class = "results-modal enhanced-modal",
        
        # Animated background particles
        div(
          class = "modal-particles",
          lapply(1:8, function(i) div(class = paste0("particle particle-", i)))
        ),
        
        div(
          class = "results-modal-header",
          div(class = "header-decoration"),
          h2(class = "results-modal-title", 
             tags$span(class = "title-icon", ""), 
             " Optimization Results"),
          p(class = "results-modal-subtitle", "Greenvale Pollution Reduction Analysis"),
          tags$button(
            class = "results-modal-close",
            onclick = "Shiny.setInputValue('close_results_modal', Math.random())",
            "×"
          )
        ),
        div(
          class = "results-modal-body",
          
          # Your Input Section
          div(
            class = "results-input-section slide-in-up",
            div(class = "section-header",
                h3(class = "results-section-title", 
                   tags$span(class = "section-icon", ""), 
                   " Your Input"),
                div(class = "section-badge", 
                    paste0(length(selected_project_names), " Projects Selected"))
            ),
            div(class = "results-project-grid", all_project_items)
          ),
          
          # Status Section with celebration animation
          div(
            class = "results-status-section slide-in-up delay-1",
            
            # Success icon with animation
            div(class = "status-icon-container success-icon",
                tags$span(class = "status-icon-large", ""),
                div(class = "icon-sparkles",
                    lapply(1:6, function(i) div(class = paste0("sparkle sparkle-", i)))
                )
            ),
            
            div(class = "results-status-badge feasible pulse-animation", "FEASIBLE"),
            
            div(class = "status-divider"),
            
            # Optimized Cost Display
            div(class = "cost-display-container",
                p(class = "results-cost-label", "THE OPTIMIZED COST"),
                div(class = "cost-amount-wrapper",
                    div(class = "currency-symbol", "$"),
                    div(class = "results-optimal-cost", 
                        format(results$optimal_cost, big.mark = ",", nsmall = 2))
                ),
                p(class = "results-cost-subtext",
                  paste0("Minimum cost to achieve all pollution reduction targets"))
            ),
            
            # Quick Stats
            div(class = "quick-stats-container",
                div(class = "quick-stat",
                    div(class = "stat-icon", ""),
                    div(class = "stat-value", results$num_projects_selected),
                    div(class = "stat-label", "Projects Used")
                ),
                div(class = "quick-stat",
                    div(class = "stat-icon", ""),
                    div(class = "stat-value", "10/10"),
                    div(class = "stat-label", "Targets Met")
                ),
                div(class = "quick-stat",
                    div(class = "stat-icon", ""),
                    div(class = "stat-value", "100%"),
                    div(class = "stat-label", "Optimized")
                )
            )
          ),
          
          # Projects Used Section
          div(
            class = "results-projects-section slide-in-up delay-2",
            h3(class = "results-section-title breakdown-title", 
               tags$span(class = "section-icon", ""), 
               " Solution & Cost Breakdown"),
            div(class = "projects-grid-enhanced", project_cards)
          )
        ),
        div(
          class = "results-modal-actions",
          tags$button(
            class = "results-action-btn primary-btn",
            onclick = "Shiny.setInputValue('view_iterations_from_modal', Math.random())",
            tags$span(class = "btn-icon", ""), " View Simplex Iterations"
          ),
          tags$button(
            class = "results-action-btn secondary-btn",
            onclick = "Shiny.setInputValue('close_results_modal', Math.random())",
            tags$span(class = "btn-icon", ""), " Done"
          )
        )
      )
    )
  })
  
  
  # ===== PROJECT INFO MODAL =====
  output$project_info_modal <- renderUI({
    # Only show if open
    if (!isTRUE(project_state$project_info_open)) {
      return(NULL)
    }
    
    # Get current index safely
    idx <- project_state$current_project_index
    if (is.null(idx) || is.na(idx)) return(NULL)
    if (!exists("projects")) return(NULL)
    if (idx < 1 || idx > nrow(projects)) return(NULL)
    
    proj <- projects[idx, , drop = FALSE]
    
    # Basic display fields
    project_name <- as.character(proj$Project)
    project_cost <- if ("Cost" %in% names(proj)) proj$Cost else NA
    
    # Decide which columns are pollutants:
    # all numeric columns except Project / Cost (you can add more exclusions)
    non_pollutant_cols <- c("Project", "Cost", "ImageFile")
    candidate_cols <- setdiff(names(proj), non_pollutant_cols)
    
    numeric_mask <- vapply(
      proj[, candidate_cols, drop = FALSE],
      is.numeric,
      logical(1)
    )
    pollutant_cols <- candidate_cols[numeric_mask]
    
    # Limit to max 10 pollutant columns (2 per row * 5 rows)
    max_show <- min(length(pollutant_cols), 10L)
    pollutant_cols <- pollutant_cols[seq_len(max_show)]
    
    # Precompute pollutant names & values
    pol_names <- pollutant_cols
    pol_values <- vapply(
      pollutant_cols,
      function(col) {
        val <- proj[[col]]
        format(round(val, 2), nsmall = 2)
      },
      character(1)
    )
    
    # Build 5 rows: each row = name1, val1, name2, val2
    rows <- lapply(1:5, function(r) {
      idx1 <- (r - 1) * 2 + 1
      idx2 <- (r - 1) * 2 + 2
      
      name1 <- if (idx1 <= length(pol_names)) pol_names[idx1] else ""
      val1  <- if (idx1 <= length(pol_values)) pol_values[idx1] else ""
      
      name2 <- if (idx2 <= length(pol_names)) pol_names[idx2] else ""
      val2  <- if (idx2 <= length(pol_values)) pol_values[idx2] else ""
      
      tags$tr(
        tags$td(class = "pi-pollutant-name",  name1),
        tags$td(class = "pi-pollutant-value", val1),
        tags$td(class = "pi-pollutant-name",  name2),
        tags$td(class = "pi-pollutant-value", val2)
      )
    })
    
    tagList(
      # Backdrop (click to close)
      div(
        class = "project-info-backdrop",
        onclick = "Shiny.setInputValue('close_project_info_backdrop', Math.random())"
      ),
      
      # Modal card (portrait)
      div(
        class = "project-info-modal",
        
        # Header
        div(
          class = "project-info-header",
          div(
            class = "project-info-title",
            project_name
          ),
          tags$button(
            class = "project-info-close",
            onclick = "Shiny.setInputValue('close_project_info', Math.random())",
            HTML("&times;")
          )
        ),
        
        # Body
        div(
          class = "project-info-body",
          
          # Cost pill (if available)
          if (!is.na(project_cost)) div(
            class = "project-info-cost-pill",
            "Estimated Cost: ",
            span(
              class = "project-info-cost-value",
              paste0("$", format(project_cost, big.mark = ",", nsmall = 2))
            )
          ),
          
          if (length(pollutant_cols) > 0) {
            tagList(
              h4(class = "project-info-subtitle", "Pollutant Reductions (per unit)"),
              tags$table(
                class = "pi-pollutant-table",
                tags$tbody(rows)
              )
            )
          } else {
            p(
              class = "pi-no-data",
              "No pollutant breakdown available for this project."
            )
          }
        )
      )
    )
  })
  
  
  
  # ===== MODAL CLOSE OBSERVERS =====
  
  observeEvent(input$close_results_modal, {
    project_state$results_modal_open <- FALSE
  })
  
  observeEvent(input$close_results_backdrop, {
    project_state$results_modal_open <- FALSE
  })
  
  observeEvent(input$view_iterations_from_modal, {
    project_state$results_modal_open <- FALSE
    updateTabsetPanel(session, "main_tabs", selected = "iterations_tab")
  })
  
  # ===== PROJECT INFO MODAL - CLOSE HANDLERS =====
  observeEvent(input$close_project_info, {
    project_state$project_info_open <- FALSE
  })
  
  observeEvent(input$close_project_info_backdrop, {
    project_state$project_info_open <- FALSE
  })
  
  # ===== RENDER CREDITS MODAL =====
  
  output$credits_modal <- renderUI({
    if (modal_state$credits_open) {
      tagList(
        div(
          class = "modal-backdrop",
          id = "credits_backdrop",
          onclick = "Shiny.setInputValue('close_credits_backdrop', Math.random())"
        ),
        div(
          class = "credits-section",
          
          # Close button (X)
          tags$button(
            class = "modal-close",
            id = "close_credits_x",
            onclick = "Shiny.setInputValue('close_credits_btn', Math.random())",
            "×"
          ),
          
          # Content
          h2("Credits", style = "color: #2c3e50; margin-top: 0;"),
          hr(),
          
          h3("Course & Institution"),
          p("CMSC 150 – Numerical and Symbolic Computation"),
          p("University of the Philippines Los Baños"),
          h3("Instructor"),
          p("Prof. Ariel B. Doria"),
          h3("Developer"),
          p("Ivan Joas B. Managat"),
          
          br(),
          p(
            strong("City of Greenvale – Pollution Reduction Optimization System"),
            style = "text-align: center; color: #666;"
          )
        )
      )
    } else {
      return(NULL)
    }
  })
  
  # ===== RENDER INSTRUCTIONS MODAL (PROJECT SELECTION) =====
  
  output$instructions_modal <- renderUI({
    if (modal_state$instructions_open) {
      tagList(
        # Dark overlay
        div(
          class = "modal-backdrop",
          id = "instructions_backdrop",
          onclick = "Shiny.setInputValue('close_instructions_backdrop', Math.random())"
        ),
        
        # White card (reuse about-section styling)
        div(
          class = "about-section",
          
          # Close button
          tags$button(
            class = "modal-close",
            id = "close_instructions_x",
            onclick = "Shiny.setInputValue('close_instructions_btn', Math.random())",
            "×"
          ),
          
          h2("How to Use the Project Selection Page", 
             style = "color: #2c3e50; margin-top: 0;"),
          hr(),
          
          h4("1. Select Projects"),
          tags$ul(
            tags$li("Use the grid at the center to select mitigation projects."),
            tags$li("Click a project card to toggle it on/off."),
            tags$li("Selected cards will glow with a green-yellow border.")
          ),
          
          h4("2. Use the Control Buttons"),
          tags$ul(
            tags$li(strong("Select All:"), " selects all projects in one click."),
            tags$li(strong("Reset:"), " clears all selections and removes previous results."),
            tags$li(strong("Run Optimization:"), 
                    " solves for the lowest-cost combination that meets all pollution targets.")
          ),
          
          h4("3. Review Target Pollutants"),
          p("On the lower-left side, the pollutants table shows the required reduction targets (in tons) for each pollutant."),
          p("The optimizer will try to ensure that the total reduction from your selected projects meets or exceeds these values."),
          
          h4("4. Read the Optimization Summary"),
          tags$ul(
            tags$li("The upper-right panel shows whether the solution is ", strong("Feasible"), " or ", strong("Infeasible"), "."),
            tags$li(strong("Optimal Cost:"), " is the minimum total cost to meet all targets."),
            tags$li(strong("Projects Used:"), " shows how many selected projects are actually used in the optimal solution.")
          ),
          
          h4("5. View Detailed Simplex Iterations"),
          tags$ul(
            tags$li("Click the circular iterations icon in the summary, or the button in the results modal, to jump to the ",
                    strong("Simplex Iterations"), " tab."),
            tags$li("There, you can see each tableau, basic solution, and pivot step in the solving process.")
          ),
          
          br(),
          div(
            style = "background-color: #e8f5e9; padding: 15px; border-radius: 8px; border-left: 4px solid #4CAF50;",
            h4("💡 Tip", style = "margin-top: 0;"),
            p("If you get an infeasible solution, select more projects or try different combinations, then run the optimization again.")
          )
        )
      )
    } else {
      return(NULL)
    }
  })
  
  
  # ===== RENDER ABOUT MODAL =====
  
  output$about_modal <- renderUI({
    if (modal_state$about_open) {
      tagList(
        div(
          class = "modal-backdrop",
          id = "about_backdrop",
          onclick = "Shiny.setInputValue('close_about_backdrop', Math.random())"
        ),
        div(
          class = "about-section",
          tags$button(
            class = "modal-close",
            id = "close_about_x",
            onclick = "Shiny.setInputValue('close_about_btn', Math.random())",
            "×"
          ),
          h2("About This Application", style = "color: #2c3e50; margin-top: 0;"),
          hr(),
          h4("Problem Description"),
          p("The City of Greenvale must reduce 10 types of pollutants to meet national environmental standards.",
            "The city has access to 30 different mitigation projects, each with specific costs and pollution reduction capabilities."),
          h4("Objective"),
          p("Find the minimum-cost combination of projects that meets or exceeds all pollution reduction targets."),
          h4("Methodology"),
          p("This application uses the Simplex Method (Linear Programming) to solve the optimization problem.",
            "The problem is formulated as a cost minimization with constraint satisfaction."),
          h4("How to Use"),
          tags$ol(
            tags$li(strong("Navigate:"), " Click 'Get Started' or 'Main' to go to the Project Selection page."),
            tags$li(strong("Select Projects:"), " Click on project cards to select them."),
            tags$li(strong("Run Optimization:"), " Click 'Run Optimization' to solve the problem."),
            tags$li(strong("View Results:"), " See the optimal solution with costs and project units."),
          ),
          h4("Constraints"),
          tags$ul(
            tags$li("Each project can be implemented 0 to 20 times (units)"),
            tags$li("All 10 pollutant reduction targets must be met"),
            tags$li("Projects can be implemented in fractional units")
          ),
          h4("Pollutants Tracked"),
          p("CO₂, NO₂, SO₂, PM2.5, CH₄, VOC, CO, NH₃, Black Carbon (BC), N₂O"),
          br(),
          div(
            style = "background-color: #e8f5e9; padding: 15px; border-radius: 8px; border-left: 4px solid #4CAF50;",
            h4("💡 Tip", style = "margin-top: 0;"),
            p("If your solution is infeasible, try selecting more projects or different combinations to meet all pollution targets.")
          ),
          br(),
          p(strong("CMSC 150 - Numerical and Symbolic Computation"), 
            style = "text-align: center; color: #666;")
        )
      )
    } else {
      return(NULL)
    }
  })
  
  # ===== TAB NAVIGATION =====
  
  observeEvent(input$back_to_selection, {
    updateTabsetPanel(session, "main_tabs", selected = "selection_tab")
  })
  
  observeEvent(input$go_back_welcome, {
    updateTabsetPanel(session, "main_tabs", selected = "welcome_tab")
  })
  
  # ===== OUTPUTS =====
  
  # ===== REPLACE THE ENTIRE ITERATIONS SECTION IN server.R =====
  # Find the section starting with "output$iterations_available" and replace everything
  # up to (but not including) "observeEvent(input$back_to_selection..."
  
  # ===== ITERATIONS TAB - DATATABLE VERSION =====
  
  output$iterations_available <- reactive({
    length(iterations_text()) > 0
  })
  outputOptions(output, "iterations_available", suspendWhenHidden = FALSE)
  
  # Iteration selector dropdown
  output$iteration_selector <- renderUI({
    iter_data <- iterations_text()
    
    if (length(iter_data) == 0) {
      return(NULL)
    }
    
    # Build choices with descriptive labels
    choices <- setNames(
      seq_along(iter_data),
      sapply(iter_data, function(x) {
        if (x$iteration_num == 0) {
          "Initial Tableau"
        } else if (x$status == "Optimal") {
          paste0("Iteration ", x$iteration_num, " - OPTIMAL")
        } else {
          paste0("Iteration ", x$iteration_num)
        }
      })
    )
    
    div(
      class = "iteration-controls",
      selectInput(
        "selected_iteration",
        "Select Iteration:",
        choices = choices,
        selected = 1,
        width = "100%"
      ),
      div(
        class = "iteration-nav-buttons",
        actionButton("prev_iter", "← Previous", class = "iter-nav-btn"),
        actionButton("next_iter", "Next →", class = "iter-nav-btn"),
        actionButton("jump_to_optimal", "Jump to Optimal", class = "iter-nav-btn optimal-btn")
      )
    )
  })
  
  # Previous button
  observeEvent(input$prev_iter, {
    current <- as.numeric(input$selected_iteration)
    if (!is.na(current) && current > 1) {
      updateSelectInput(session, "selected_iteration", selected = current - 1)
    }
  })
  
  # Next button
  observeEvent(input$next_iter, {
    iter_data <- iterations_text()
    current <- as.numeric(input$selected_iteration)
    if (!is.na(current) && current < length(iter_data)) {
      updateSelectInput(session, "selected_iteration", selected = current + 1)
    }
  })
  
  # Jump to optimal button
  observeEvent(input$jump_to_optimal, {
    iter_data <- iterations_text()
    if (length(iter_data) == 0) return()
    
    # Find optimal iteration
    optimal_idx <- which(sapply(iter_data, function(x) x$status == "Optimal"))
    
    if (length(optimal_idx) > 0) {
      updateSelectInput(session, "selected_iteration", selected = optimal_idx[1])
    }
  })
  
  # Render selected iteration using DataTables
  output$iterations_display <- renderUI({
    iter_data <- iterations_text()
    
    if (length(iter_data) == 0 || is.null(input$selected_iteration)) {
      return(NULL)
    }
    
    idx <- as.numeric(input$selected_iteration)
    if (is.na(idx) || idx < 1 || idx > length(iter_data)) {
      return(NULL)
    }
    
    iter <- iter_data[[idx]]
    
    # Build header info
    if (iter$iteration_num == 0) {
      header_text <- "Initial Tableau"
      header_class <- "iter-header-initial"
      show_basic_solution <- FALSE
    } else if (iter$status == "Optimal") {
      header_text <- paste0("Iteration ", iter$iteration_num, " - OPTIMAL SOLUTION")
      header_class <- "iter-header-optimal"
      show_basic_solution <- TRUE
    } else {
      pivot_info <- ""
      if (!is.na(iter$pivot_row) && !is.na(iter$pivot_col)) {
        pivot_col_name <- colnames(iter$tableau)[iter$pivot_col]
        pivot_info <- paste0(" (Pivot: Row ", iter$pivot_row, ", Col ", pivot_col_name, ")")
      }
      header_text <- paste0("Iteration ", iter$iteration_num, pivot_info)
      header_class <- "iter-header-normal"
      show_basic_solution <- TRUE
    }
    
    # Basic solution table UI
    basic_solution_output <- NULL
    if (show_basic_solution && length(iter$basic_vars) > 0) {
      basic_solution_output <- div(
        class = "basic-solution-dt-container",
        h4("Basic Solution", class = "dt-section-title"),
        DT::dataTableOutput(paste0("basic_solution_", idx))
      )
    }
    
    # Main tableau UI
    div(
      class = "iteration-container-dt",
      div(class = paste("iteration-header", header_class), header_text),
      basic_solution_output,
      div(
        class = "iteration-tableau-dt-container",
        h4("Tableau", class = "dt-section-title"),
        DT::dataTableOutput(paste0("tableau_", idx))
      )
    )
  })
  
  # Dynamic DataTable outputs for basic solution
  observe({
    iter_data <- iterations_text()
    
    if (length(iter_data) == 0 || is.null(input$selected_iteration)) {
      return()
    }
    
    idx <- as.numeric(input$selected_iteration)
    if (is.na(idx) || idx < 1 || idx > length(iter_data)) {
      return()
    }
    
    iter <- iter_data[[idx]]
    
    # Render basic solution table
    if (iter$iteration_num > 0 && length(iter$basic_vars) > 0) {
      output[[paste0("basic_solution_", idx)]] <- DT::renderDataTable({
        all_vars <- colnames(iter$tableau)
        all_vars <- all_vars[all_vars != "Solution"]
        all_vars <- c(all_vars[all_vars != "Z"], "Z")
        
        values <- setNames(rep(0, length(all_vars)), all_vars)
        
        for (j in seq_along(iter$basic_vars)) {
          var_name <- iter$basic_vars[j]
          if (var_name %in% names(values)) {
            values[var_name] <- round(iter$basic_vals[j], 4)
          }
        }
        
        if ("Z" %in% names(values)) {
          values["Z"] <- round(iter$z_value, 4)
        }
        
        # Create horizontal layout dataframe
        basic_df <- data.frame(
          t(values),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        
        datatable(
          basic_df,
          options = list(
            dom = 't',
            pageLength = 1,
            ordering = FALSE,
            scrollX = TRUE,
            columnDefs = list(
              list(className = 'dt-center', targets = '_all')
            )
          ),
          rownames = FALSE,
          class = 'cell-border compact basic-solution-table',
          selection = 'none'
        ) %>%
          formatRound(columns = names(basic_df), digits = 4)
      })
    }
    
    # Render main tableau table
    output[[paste0("tableau_", idx)]] <- DT::renderDataTable({
      tableau_df <- as.data.frame(iter$tableau)
      tableau_df <- round(tableau_df, 4)
      
      # Add row names as first column
      tableau_with_rows <- data.frame(
        Row = rownames(tableau_df),
        tableau_df,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      datatable(
        tableau_with_rows,
        options = list(
          scrollX = TRUE,
          scrollY = "450px",
          pageLength = 100,
          dom = 't',
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all'),
            list(className = 'dt-row-header', targets = 0)
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover compact',
        selection = 'none'
      ) %>%
        formatRound(columns = names(tableau_df), digits = 4)
    })
  })
  
  # ===== DATA TAB TABLES =====
  
  output$projects_data_table <- DT::renderDT({
    datatable(
      projects,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatCurrency(columns = 'Cost', currency = "$", digits = 2)
  })
  
  output$targets_data_table <- DT::renderDT({
    datatable(
      targets,
      options = list(
        pageLength = 10,
        dom = 't'
      ),
      rownames = FALSE
    )
  })
}