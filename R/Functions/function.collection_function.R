#*******************************************************************************
#*
#*
#*           A collection of functions for data-cleaning and analysis                                                                                  
#*
#* Author: Loukia M. Spineli 
#* Date: October 2024
#*******************************************************************************


## *run_nodesplit*: Judge whether the model converged for *all* monitored parameters ----
rhat_run_nodesplit <- function(results) {
  
  
  ## Get the Rhat for specific monitored parameters
  # Direct estimates
  dir_rhat <- results$direct[, 7]
  
  # Indirect estimates
  indir_rhat <- results$indirect[, 7]
  
  # Inconsistency factor
  diff_rhat <- results$diff[, 7]
  
  # Between-study standard deviation
  tau_rhat <- results$tau[, 7]
  
  
  ## Judge convergence based on 'rhat' for each node
  # Direct estimates 
  converged_dir <- ifelse(dir_rhat < 1.1, "yes", "no")
  
  # Indirect estimates 
  converged_indir <- ifelse(indir_rhat < 1.1, "yes", "no")
  
  # Inconsistency factor 
  converged_diff <- ifelse(diff_rhat < 1.1, "yes", "no")
  
  # Between-study standard deviation
  converged_tau <- ifelse(tau_rhat < 1.1, "yes", "no")
  
  
  ## Did the model converge or fail to converge? For each monitored parameter overall
  # Direct estimates
  decision_dir <- 
    ifelse(any(unique(converged_dir) == "no" | length(unique(converged_dir)) > 2) == TRUE, 
           "lack of convergence", "convergence")
  
  # indirect estimates
  decision_indir <- 
    ifelse(any(unique(converged_indir) == "no" | length(unique(converged_indir)) > 2) == TRUE, 
           "lack of convergence", "convergence")
  
  # Inconsistency factor 
  decision_diff <- 
    ifelse(any(unique(converged_diff) == "no" | length(unique(converged_diff)) > 2) == TRUE, 
           "lack of convergence", "convergence")
  
  # Between-study standard deviation 
  decision_tau <- 
    ifelse(any(unique(converged_tau) == "no" | length(unique(converged_tau)) > 2) == TRUE, 
           "lack of convergence", "convergence")
  
  
  ## Collect all results
  return(list(decision_dir = decision_dir,
              decision_indir = decision_indir,
              decision_diff = decision_diff,
              decision_tau = decision_tau,
              dir_rhat = dir_rhat,
              indir_rhat = indir_rhat,
              diff_rhat = diff_rhat,
              tau_rhat = tau_rhat))
}


## Remove nodes with lack of convergence from the node-splitting results ----
node_res_fin <- function (all_failed_nets, node_res, name) {
  
  node_fail <-
    lapply(1:length(all_failed_nets), function(x) {as.data.frame(node_res[[all_failed_nets[x]]][name])[-all_failed_nodes[[x]], ]})
  names(node_fail) <- all_failed_nets
  
  node_conv <- lapply(c(1:length(node_res))[-all_failed_nets], function(x) data.frame(node_res[[x]][name]))
  names(node_conv) <- c(1:length(node_res))[-all_failed_nets]
  
  node_fin0 <- c(node_fail, node_conv)
  node_fin <- node_fin0[order(as.numeric(names(node_fin0)))]
  
  return(node_fin)
}


## Find the split nodes in a network ----
summary_network_chars <- function (data, measure) {
  
  # Prepare the dataset
  item <- rnmamod::data_preparation(data, measure)
  
  # Rename columns to agree with gemtc
  if (measure == "SMD") {
    names(item$y0) <- paste0("y..", seq_len(max(item$na)), ".")
    names(item$se0) <- paste0("se..", seq_len(max(item$na)), ".")
    names(item$N) <- paste0("n..", seq_len(max(item$na)), ".")
    names(item$t) <- paste0("t..", seq_len(max(item$na)), ".")
    na.. <- item$na
    
    # Convert to one-arm-per-row as required in GeMTC
    transform <- gemtc::mtc.data.studyrow(cbind(item$t,
                                                item$y0,
                                                item$se0,
                                                item$N,
                                                na..),
                                          armVars = c("treatment" = "t",
                                                      "mean" = "y",
                                                      "std.error" = "se",
                                                      "sampleSize" = "n"),
                                          nArmsVar = "na")
  } else if (measure == "OR") {
    names(item$r) <- paste0("r..", seq_len(max(item$na)), ".")
    names(item$N) <- paste0("n..", seq_len(max(item$na)), ".")
    names(item$t) <- paste0("t..", seq_len(max(item$na)), ".")
    na.. <- item$na
    
    # Convert to one-arm-per-row as required in GeMTC
    transform <- gemtc::mtc.data.studyrow(cbind(item$t,
                                                item$r,
                                                item$N,
                                                na..),
                                          armVars = c("treatment" = "t",
                                                      "response" = "r",
                                                      "sampleSize" = "n"),
                                          nArmsVar = "na")
  }
  
  # Detect the nodes to split (GeMTC functions)
  transform$treatment <- as.numeric(transform$treatment)
  splitting0 <- gemtc::mtc.nodesplit.comparisons(gemtc::mtc.network(transform))
  
  # Convert into 'double' and Re-order treatments within nodes so that t1 < t2 < t3 ...
  splitting <- t(apply(t(apply(splitting0, 1, as.numeric)), 1, function(x) sort(x, decreasing = TRUE)))
  
  return(splitting)
}


#' Function to get the parameters of quadratic equation, ax^2+bx+c, with b=0, ----
#' and plot the original data with the predicted line

solve_quadratic_equ <- function (x, y, weight = NULL) {
  
  # Calculate the square of x
  x2 <- x * x
  
  # Run weighted least squares quadratic regression, Y = ax^2+bx+c, with b=0
  res <- if (is.null(weight)) {
    lm(y ~ x2)
  } else {
    lm(y ~ x2, weights = weight)
  }
  
  # Get predictions for a range of x values
  predict_res <- predict(res, list(x = sort(x), x2 = sort(x)^2))
  
  # Prot original data with predicted line
  plot(x, y, pch = 16)
  lines(sort(x), predict_res, col='blue')
  
  return(summary(res))
}
