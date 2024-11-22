#*******************************************************************************
#*
#*                Checking convergence and results plausibility                                                                                                                                                                       
#* 
#* Date: November 2024
#*******************************************************************************


## Load function ----
source("./30_Analysis/Functions/function.collection_function.R")
source("./30_Analysis/Functions/complete analysis results_function.R")


## Load analysis database ----
# List of analysis (final) datasets
load("./31_Database/Analysed database/dataset final.RData")

# Results from node-splitting model
load(file = "./30_Analysis/Node-splitting results.RData")


## Complete analysis results ----
complete_res <- complete_analysis_results()


## Check convergence from RE node-splitting ----
# Direct estimates
converged_dir <- lapply(node_res, function(x) rhat_run_nodesplit(x)$decision_dir)
table(unlist(converged_dir)) # lack of convergence: 0

# Indirect estimates
converged_indir <- lapply(node_res, function(x) rhat_run_nodesplit(x)$decision_indir)
table(unlist(converged_indir)) # lack of convergence: 0

# Inconsistency factor
converged_diff <- lapply(node_res, function(x) rhat_run_nodesplit(x)$decision_diff)
table(unlist(converged_diff)) # lack of convergence: 0

# Between-study standard deviation
converged_tau <- lapply(node_res, function(x) rhat_run_nodesplit(x)$decision_tau)
table(unlist(converged_tau)) # lack of convergence: 0


## Find networks with very large Ds (e.g., > 15, corresponding to outliers that deviate substantially from other outliers)
# Restrict dataset
restrict <- subset(complete_res, kld_value > 15)

# Unique networks
restrict_net <- as.numeric(unique(restrict$network_id))

# Get network plot
par(mfrow = c(2, 2))
rnmamod::netplot(dataset_final[[3]], edge_label_cex = 1, node_label_cex = 1.5)
rnmamod::netplot(dataset_final[[5]], edge_label_cex = 1, node_label_cex = 1.5)
rnmamod::netplot(dataset_final[[53]], edge_label_cex = 1, node_label_cex = 1.5)
rnmamod::netplot(dataset_final[[57]], edge_label_cex = 1, node_label_cex = 1.5)
par(mfrow = c(1, 1))
