# optimization_core.R
# Core optimization functions for pollution reduction problem

# ===== CONSTRAINTS BUILDER =====

build_constraints <- function(selected_projects = NULL) {
  
  # Step 1: Filter projects if user selected a subset
  if (!is.null(selected_projects)) {
    chosen_projects <- projects[projects$Project %in% selected_projects, ]
  } else {
    chosen_projects <- projects
  }
  
  # Step 2: Pollutant reduction matrix
  # Each row = project, each column = pollutant reduction
  pollutant_matrix <- as.matrix(chosen_projects[, 3:12])
  
  # Step 3: Cost vector (objective coefficients)
  project_costs <- chosen_projects$Cost
  
  # Step 4: Target vector (minimum pollutant reductions)
  pollutant_targets <- targets$TargetValue
  
  # Step 5: Bounds (0 ≤ units ≤ 20 for each project)
  min_units <- rep(0, nrow(chosen_projects))
  max_units <- rep(20, nrow(chosen_projects))
  
  # Step 6: Return everything as a list
  return(list(
    pollutant_matrix = pollutant_matrix,   # constraint coefficients
    pollutant_targets = pollutant_targets, # RHS of constraints
    project_costs = project_costs,         # objective coefficients
    min_units = min_units,                 # lower bounds
    max_units = max_units,                 # upper bounds
    project_info = chosen_projects         # metadata (names, costs, reductions)
  ))
}

# ===== PRIMAL MATRIX BUILDER =====

build_primal_matrix <- function(constraints) {
  
  # Extract data from constraints
  pollutant_matrix <- constraints$pollutant_matrix  # rows = projects, cols = pollutants
  pollutant_targets <- constraints$pollutant_targets
  project_costs <- constraints$project_costs
  n_projects <- length(project_costs)
  n_pollutants <- length(pollutant_targets)
  
  # Transpose pollutant_matrix so that:
  # rows = pollutants, cols = projects
  # This gives us the constraint coefficients
  constraint_coeffs <- t(pollutant_matrix)
  
  # Number of constraints:
  # - n_pollutants constraints (≥ type)
  # - n_projects upper bound constraints (≤ type, will be converted to ≥)
  n_constraints <- n_pollutants + n_projects
  
  # Build the full constraint matrix
  # We'll have (n_pollutants + n_projects) rows and n_projects columns
  primal_constraints <- matrix(0, nrow = n_constraints, ncol = n_projects)
  primal_rhs <- numeric(n_constraints)
  
  # First n_pollutants rows: pollutant reduction constraints (≥)
  primal_constraints[1:n_pollutants, ] <- constraint_coeffs
  primal_rhs[1:n_pollutants] <- pollutant_targets
  
  # Next n_projects rows: upper bound constraints (x_i ≤ 20)
  # Convert to -x_i ≥ -20
  for (i in 1:n_projects) {
    row_idx <- n_pollutants + i
    primal_constraints[row_idx, i] <- -1
    primal_rhs[row_idx] <- -20
  }
  
  # Add objective function as the last row
  # Objective: minimize cost = c1*x1 + c2*x2 + ... + cn*xn
  objective_row <- c(project_costs, 0)  # Add 0 for the Z column
  
  # Combine constraints and RHS
  primal_matrix <- cbind(primal_constraints, primal_rhs)
  
  # Add objective row at the bottom
  primal_matrix <- rbind(primal_matrix, objective_row)
  
  # Add column names
  project_names <- paste0("x", 1:n_projects)
  colnames(primal_matrix) <- c(project_names, "Solution")
  
  # Add row names for clarity
  # Get pollutant names from the project_info dataframe
  pollutant_names <- colnames(constraints$project_info)[3:12]
  
  constraint_names <- c(
    pollutant_names,
    paste0("x", 1:n_projects, "<=20")
  )
  
  rownames(primal_matrix) <- c(constraint_names, "Z")
  
  return(primal_matrix)
}

# ===== SIMPLEX SOLVER =====

simplex_solve <- function(initial_tableau, maximize = TRUE) {
  run_simplex <- function(tab) {
    tab <- as.matrix(tab)
    nr <- nrow(tab); nc <- ncol(tab)
    sol_col <- nc
    iterations <- list()
    
    # Helper function to extract basic solution FROM THE OBJECTIVE ROW (last row)
    extract_basic_solution <- function(T) {
      nrf <- nrow(T); ncf <- ncol(T)
      
      # Read values directly from the objective row (last row)
      obj_row <- T[nrf, 1:(ncf - 1)]  # All columns except Solution column
      
      basic_vars <- character(0)
      basic_vals <- numeric(0)
      
      # Include ALL variables with non-zero values in the objective row
      for (j in seq_len(ncf - 1)) {
        val <- obj_row[j]
        if (abs(val) > 1e-8) {  # Only include significant non-zero values
          var_name <- if(!is.null(colnames(T))) colnames(T)[j] else paste0("v", j)
          basic_vars <- c(basic_vars, var_name)
          basic_vals <- c(basic_vals, val)
        }
      }
      
      list(
        full_solution = obj_row,
        basic_vars = basic_vars,
        basic_vals = basic_vals
      )
    }
    
    # Store initial tableau
    initial_sol <- extract_basic_solution(tab)
    iterations[[1]] <- list(
      tableau = as.data.frame(tab),
      iteration_num = 0,
      pivot_row = NA,
      pivot_col = NA,
      status = "Initial Tableau",
      basic_vars = initial_sol$basic_vars,
      basic_vals = initial_sol$basic_vals,
      z_value = tab[nr, sol_col]
    )
    
    pick_col <- function(T) {
      obj <- T[nr, 1:(nc-1)]
      neg <- which(obj < 0)
      if (length(neg) == 0) return(NA)
      return(neg[which.min(obj[neg])])
    }
    
    pick_row <- function(T, pc) {
      col <- T[1:(nr-1), pc]
      rhs <- T[1:(nr-1), sol_col]
      ratios <- rep(Inf, nr-1)
      pos <- which(col > 0)
      ratios[pos] <- rhs[pos] / col[pos]
      if (all(is.infinite(ratios))) return(NA)
      return(which.min(ratios))
    }
    
    pivot <- function(T, pr, pc) {
      pe <- T[pr, pc]
      T[pr, ] <- T[pr, ] / pe
      for (r in seq_len(nrow(T))) {
        if (r != pr) {
          f <- T[r, pc]
          T[r, ] <- T[r, ] - f * T[pr, ]
        }
      }
      T
    }
    
    iter_count <- 1
    repeat {
      pc <- pick_col(tab)
      if (is.na(pc)) break
      pr <- pick_row(tab, pc)
      if (is.na(pr)) stop("Unbounded")
      
      tab <- pivot(tab, pr, pc)
      rownames(tab) <- NULL
      
      # Extract basic solution after pivot
      current_sol <- extract_basic_solution(tab)
      
      # Store iteration with basic solution
      iterations[[length(iterations) + 1]] <- list(
        tableau = as.data.frame(tab),
        iteration_num = iter_count,
        pivot_row = pr,
        pivot_col = pc,
        status = "Iterating",
        basic_vars = current_sol$basic_vars,
        basic_vals = current_sol$basic_vals,
        z_value = tab[nr, sol_col]
      )
      
      iter_count <- iter_count + 1
    }
    
    # Mark final iteration
    iterations[[length(iterations)]]$status <- "Optimal"
    
    nrf <- nrow(tab); ncf <- ncol(tab)
    sol_vec <- numeric(ncf-1)
    for (j in seq_len(ncf-1)) {
      colj <- tab[1:(nrf-1), j]
      nz_idx <- which(abs(colj) > .Machine$double.eps * 100)
      if (length(nz_idx) == 1 && abs(colj[nz_idx] - 1) < 1e-8) {
        sol_vec[j] <- tab[nz_idx, ncf]
      } else {
        sol_vec[j] <- 0
      }
    }
    if (!is.null(colnames(tab))) names(sol_vec) <- colnames(tab)[1:(ncf-1)]
    
    list(final = tab, Z = tab[nrf, ncf], basic_solution = sol_vec, iterations = iterations)
  }
  
  if (maximize) {
    res <- run_simplex(initial_tableau)
    return(list(
      final_tableau = res$final, 
      primal_solution = res$basic_solution, 
      Z = res$Z, 
      iterations = res$iterations
    ))
  }
  
  # Dual approach for minimization
  P <- as.matrix(initial_tableau)
  m <- nrow(P) - 1
  n <- ncol(P) - 1
  A <- P[1:m, 1:n, drop = FALSE]
  b <- P[1:m, n + 1]
  c <- P[m + 1, 1:n]
  A_t <- t(A)
  rhs_dual <- c
  obj_dual <- b
  
  rows_dual <- n
  cols_S <- m
  cols_X <- n
  
  tableau <- cbind(A_t, diag(rows_dual), matrix(0, rows_dual, 1), matrix(rhs_dual, ncol = 1))
  rownames(tableau) <- NULL
  last_row <- c(-obj_dual, rep(0, rows_dual), 1, 0)
  tableau <- rbind(tableau, last_row)
  rownames(tableau) <- NULL
  colnames(tableau) <- c(paste0("S", 1:cols_S), paste0("X", 1:cols_X), "Z", "Solution")
  
  res <- run_simplex(tableau)
  final <- res$final
  nr <- nrow(final); nc <- ncol(final)
  S_count <- cols_S
  X_start <- S_count + 1
  X_end <- S_count + cols_X
  primal <- as.numeric(final[nr, X_start:X_end])
  names(primal) <- paste0("x", seq_len(cols_X))
  
  list(
    final_tableau = final, 
    primal_solution = primal, 
    Z = res$Z, 
    iterations = res$iterations
  )
}