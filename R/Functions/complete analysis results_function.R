## Several functions that merge the node-splitting results with characteristics of
## the splits nodes, such as, number of studies informing the nodes. This
## function is called to create the main and supplementary Tables and Figures. 

complete_analysis_results <- function() {
  
  
  ## Load library
  list.of.packages <- c("rnmamod", "dplyr")
  lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)
  
  
  ## Load functions ----
  source("./30_Analysis/Functions/function.collection_function.R")
  source("./30_Analysis/Functions/convert_wide_to_pairwise_function.R")
  
  
  ## Load analysis database ----
  # List of analysis (final) datasets
  load("./31_Database/Analysed database/dataset final.RData")
  
  # Data-frame with characteristics of the (final) datasets
  load("./31_Database/Analysed database/nmalist final.RData")
  
  # List of node-splitting results
  load("./30_Analysis/Node-splitting results.RData")
  
  
  ## Sort in decreasing order the treatments within study  (where applicable)
  prepare_dataset <- 
    lapply(1:length(dataset_final), 
           function(x) {message(paste(x, "out of", length(dataset_final), "networks (re-ordering treatments in each study)")); suppressMessages(
             data_preparation(data = dataset_final[[x]], 
                              measure = "OR"))})
  
  
  ## Get the networks after the above within-study treatment re-ordering
  dataset_new <- lapply(prepare_dataset, function(x) data.frame(x$t, x$N, x$r))
  
  # Check that the treatments are indeed sorted in ascending order within each study
  table(unlist(lapply(dataset_new, function(x) apply(x[, c("t1", "t2")], 1, function(y) !is.unsorted(na.omit(y)))))) # All TRUE! :-)
  
  
  ## Get the split nodes and calculate their frequency in a network
  # Restrict to the treatments columns 
  get_interv <- lapply(dataset_new, function(x) x[, startsWith(colnames(x), "t")])
  
  # Restrict to treatment columns and turn into pairwise (like in 'netmeta')
  #' Results are correct: get_interv was compared with pairwise_comp for some randomly selected networks with multi-arm studies
  pairwise_comp <- lapply(get_interv, function(x) convert_wide_to_pairwise(x))
  
  # Indicate the pairwise comparison in each study: t2 vs t1
  get_compar <- 
    lapply(pairwise_comp, function(x) apply(x[, c(2, 1)], 1, function(y) {paste(y, collapse = " vs ")}))
  
  # Count the number of studies for each unique pairwise comparison in each network
  num_studies_by_compar <- lapply(get_compar, function(x) as.data.frame(table(x)))
  
  # Get the data-frame of split nodes per network 
  find_split_nodes <- 
    lapply(1:length(dataset_new), 
           function(x) {message(paste(x, "out of", length(dataset_final), "networks (extracting the split nodes)")); suppressMessages(
             summary_network_chars(data = dataset_new[[x]], 
                                   measure = "OR"))})
  
  # Indicate the pairwise comparison in each split node: t2 vs t1
  node_into_compar <- 
    lapply(find_split_nodes, function(x) apply(x, 1, function(y) {paste(y, collapse = " vs ")}))
  
  # Match the observed comparisons in a network with the corresponding split nodes
  match_compar_with_node0 <- 
    lapply(1:length(num_studies_by_compar), 
           function(x) num_studies_by_compar[[x]][is.element(num_studies_by_compar[[x]]$x, node_into_compar[[x]]), ])
  
  # Name the columns of each data-frame
  #' Results are correct: num_studies_by_compar, node_into_compar and match_compar_with_node were compared for some randomly selected networks
  match_compar_with_node <- lapply(match_compar_with_node0, function(x) {colnames(x) <- c("comparison", "Freq"); x})

  
  ## Apply the proposed index and decision framework
  # Get the average KLD (aKLD) values per split node
  index_res0 <-
    lapply(1:length(node_res), 
           function(x) {message(paste(x, "out of", length(dataset_final), "networks"));
             suppressMessages(
               kld_inconsistency(node = node_res[[x]],
                                 threshold = 0.64)$average_KLD)})
  
  # Add further results for each split node 
  index_res <- lapply(1:length(index_res0), 
                      function(x) {
                        #' Add the conclusion per *node* for likely consistency (aKLD < 0.64); otherwise, inconsistency.
                        index_res0[[x]]$node_conclusion <- ifelse (index_res0[[x]][, "kld_value"] < 0.64, "Consistency", "Inconsistency"); 
                        
                        #' Also add conclusion per *network*
                        index_res0[[x]]$net_conclusion <- if (any(index_res0[[x]][, "kld_value"] >= 0.64)) "Inconsistency" else "Consistency"; 
                        
                        # Posterior mean direct estimate per split node
                        index_res0[[x]]$direct_mean <- node_res[[x]]$direct[, "mean"]; 
                        
                        # Posterior SD of direct estimate per split node
                        index_res0[[x]]$direct_sd <- node_res[[x]]$direct[, "sd"];
                        
                        # Posterior mean indirect estimate per split node
                        index_res0[[x]]$indirect_mean <- node_res[[x]]$indirect[, "mean"]; 
                        
                        # Posterior SD of indirect estimate per split node
                        index_res0[[x]]$indirect_sd <- node_res[[x]]$indirect[, "sd"]; 
                        
                        # Posterior mean inconsistency factor estimate per split node
                        index_res0[[x]]$diff_mean <- node_res[[x]]$diff[, "mean"];
                        
                        # Posterior SD of inconsistency factor estimate per split node
                        index_res0[[x]]$diff_sd <- node_res[[x]]$diff[, "sd"];
                        
                        # Posterior median between-study standard deviation per split node
                        index_res0[[x]]$tau_median <- node_res[[x]]$tau[, "50%"];
                        
                        # Posterior SD of between-study standard deviation per split node
                        index_res0[[x]]$tau_sd <- node_res[[x]]$tau[, "sd"];
                        
                        # Two-sided Bayesian p-value for 'diff'
                        index_res0[[x]]$p_value <- node_res[[x]]$p_value[, "p_value"]; 
                        
                        # Conclusion per *node* using p-value for 'diff' and significance level at 5%
                        index_res0[[x]]$node_standard_5 <- ifelse (index_res0[[x]][, "p_value"] < 0.05, "Conclusive", "Inconclusive"); 
                        
                        # Conclusion per *node* using p-value for 'diff' and significance level at 10%
                        index_res0[[x]]$node_standard_10 <- ifelse (index_res0[[x]][, "p_value"] < 0.10, "Conclusive", "Inconclusive"); 
                        
                        # Conclusion per *network* using p-value for 'diff' and significance level at 5%
                        index_res0[[x]]$net_standard_5 <- if (any(index_res0[[x]][, "p_value"] >= 0.05)) "Inconclusive" else "Conclusive"; 
                        
                        # Conclusion per *network* using p-value for 'diff' and significance level at 10%
                        index_res0[[x]]$net_standard_10 <- if (any(index_res0[[x]][, "p_value"] >= 0.10)) "Inconclusive" else "Conclusive"; index_res0[[x]]})
  
  # Merge 'index_res' with the number of studies in each split node
  #' Results are correct: complete_res0, index_res and match_compar_with_node were compared for some randomly selected networks
  complete_res0 <- 
    lapply(1:length(index_res), 
           function(x) merge(match_compar_with_node[[x]], index_res[[x]], by = "comparison"))

  
  ## Prepare dataset for ggplot2
  # Turn 'complete_res' into one data-frame using 'do.call'
  complete_res <- bind_rows(complete_res0, .id = "column_label")
  
  # Rename the first column
  colnames(complete_res)[1] <- "network_id"
  
  # Add an indicator on whether a node is informed by a single study only
  complete_res$single_study <- factor(ifelse(complete_res$Freq == 1, "Yes", "No"), levels = c("Yes", "No"))
  
  # Add an indication on whether the posterior mean of (in)direct effects is plausible
  #complete_res$implausible = ifelse((complete_res$direct_mean > -log(5) & complete_res$direct_mean < log(5)) & 
  #                                    (complete_res$indirect_mean > -log(5) & complete_res$indirect_mean < log(5)), "No", "Yes")
  
  # Keep only the split nodes with implausible == "No"
  #complete_res_fin <- subset(complete_res, implausible == "No")
  
  # Return a message on the number of final, eligible networks and split nodes
  message(paste(length(unique(complete_res$network_id)), "networks and", dim(complete_res)[1], "split nodes were included in the analysis."))
  
  return(complete_res)
}

