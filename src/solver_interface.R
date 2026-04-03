# solver_interface.R
# User-facing interface functions for pollution reduction optimization

# ===== MAIN SOLVER FUNCTION =====

solve_pollution_problem <- function(selected_projects = NULL, show_iterations = FALSE) {
  
  # Step 1: Build constraints
  cat("Building constraints...\n")
  constraints <- build_constraints(selected_projects)
  
  # Step 2: Build primal matrix
  cat("Building primal matrix...\n")
  primal_matrix <- build_primal_matrix(constraints)
  
  # Step 3: Solve using simplex (dual approach)
  cat("Solving optimization problem...\n")
  
  # Temporarily suppress iteration output if requested
  if (!show_iterations) {
    sink(tempfile())
  }
  
  result <- tryCatch({
    solution <- simplex_solve(primal_matrix, maximize = FALSE)
    list(success = TRUE, solution = solution, error = NULL)
  }, error = function(e) {
    list(success = FALSE, solution = NULL, error = e$message)
  })
  
  if (!show_iterations) {
    sink()
  }
  
  # Step 4: Check if solution was found
  if (!result$success) {
    cat("\nPROBLEM IS INFEASIBLE\n")
    cat("The selected projects cannot meet all pollutant reduction targets.\n")
    cat("Error details:", result$error, "\n")
    return(list(
      feasible = FALSE,
      error = result$error,
      selected_projects = if(is.null(selected_projects)) "All 30 projects" else selected_projects
    ))
  }
  
  solution <- result$solution
  
  # Step 5: Extract project units
  project_units <- solution$primal_solution
  names(project_units) <- constraints$project_info$Project
  
  # Step 6: Calculate pollutant reductions achieved
  pollutant_reduction_achieved <- as.numeric(t(constraints$pollutant_matrix) %*% project_units)
  names(pollutant_reduction_achieved) <- colnames(constraints$project_info)[3:12]
  
  # Step 7: Check if all constraints are met
  constraints_met <- all(pollutant_reduction_achieved >= constraints$pollutant_targets - 0.01)
  
  # Step 8: Prepare results
  results <- list(
    feasible = constraints_met,
    optimal_cost = solution$Z,
    project_units = project_units,
    selected_projects = project_units[project_units > 0.001],
    pollutant_targets = constraints$pollutant_targets,
    pollutant_achieved = pollutant_reduction_achieved,
    constraints = constraints,
    solution = solution
  )
  
  cat("\nSolution found!\n")
  cat("Optimal Cost: $", format(results$optimal_cost, big.mark=","), "\n")
  cat("Number of projects used:", length(results$selected_projects), "\n")
  
  return(results)
}

# ===== FORMATTED SOLVER FOR SHINY =====

solve_and_format <- function(selected_projects = NULL, show_iterations = FALSE) {
  
  # Solve the problem
  results <- solve_pollution_problem(selected_projects, show_iterations)
  
  # Format for Shiny display
  if (!results$feasible) {
    return(list(
      status = "infeasible",
      message = paste("The problem is infeasible.",
                      "The selected projects cannot meet all pollutant reduction targets."),
      selected_projects = results$selected_projects,
      error = results$error
    ))
  }
  
  # Create project summary table with CORRECT column names for the modal
  selected <- results$selected_projects
  
  # Get project info for each selected project
  project_info <- results$constraints$project_info
  
  # Build the dataframe with correct structure
  selected_projects_df <- data.frame(
    Project = names(selected),
    Units = as.numeric(selected),
    Cost_per_Unit = sapply(names(selected), function(p) {
      project_info$Cost[project_info$Project == p]
    }),
    Total_Cost = sapply(names(selected), function(p) {
      unit_cost <- project_info$Cost[project_info$Project == p]
      as.numeric(selected[p]) * unit_cost
    }),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  # Create pollutant summary table
  pollutant_table <- data.frame(
    Pollutant = names(results$pollutant_achieved),
    Target = round(results$pollutant_targets, 2),
    Achieved = round(results$pollutant_achieved, 2),
    Status = ifelse(results$pollutant_achieved >= results$pollutant_targets - 0.01, "Met", "Failed"),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  return(list(
    status = "feasible",
    optimal_cost = round(results$optimal_cost, 2),
    num_projects_selected = length(selected),
    selected_projects_df = selected_projects_df, 
    pollutant_table = pollutant_table,
    raw_results = results
  ))
}

# Export as pollution_solver for backward compatibility
pollution_solver <- solve_and_format