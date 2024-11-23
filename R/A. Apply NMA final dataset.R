#*******************************************************************************
#*
#*     Apply random-effects network meta-analysis to the analysed database                                                                                                                                                 
#*                       (Use of the 'rnmamod' R package)  
#*                       
#* Date: November 2024
#*******************************************************************************


## Load library
list.of.packages <- c("rnmamod", "dplyr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load analysis database ----
# List of analysis (final) datasets
load("./data/Analysis datasets/dataset final.RData")

# Data-frame with characteristics of the (final) datasets
load("./data/Analysis datasets/nmalist final.RData") 


## Data-frame with tailored predictive distributions for tau2 ----
# Define outcome and comparison types
comparison_type <- c("pharmacological vs placebo", "pharmacological vs pharmacological", "non-pharmacological vs any")
outcome_type <- c("Objective", "Semi-objective", "Subjective")

# Prepare data-frame for binary and continuous outcomes
pred_distr <- expand.grid(comparison_type, outcome_type)
colnames(pred_distr) <- c("Intervention.comparison.type", "Objectivity.of.outcome")

# Add the mean and standard deviation
pred_distr$mean <- c(-3.95, -4.18, -2.92, -2.14, -2.37, -1.11, -1.77, -2.00, -0.74) 
pred_distr$sd <- c(1.34, 1.41, 1.02, 1.74, 1.79, 1.50, 1.52, 1.58, 1.24) 

# Get data-frame with parameters of the predictive distributions for binary outcomes
nmalist_final_pred <- 
  inner_join(nmalist_final[, c("Intervention.comparison.type", "Objectivity.of.outcome", "Type.of.Outcome.")], pred_distr)


## Conduct RE-NMA and save specific results ----
# Vectors of elements to save
nma_save <- c("data", "measure", "model", "assumption", "heter_prior", "mean_misspar", "var_misspar", "ref", "indic")

# Apply RE-NMA
#' The number of iterations (and relevant) are irrelevant since I won't use the NMA results!
nma_res <- 
  lapply(1:length(dataset_final), 
         function(x) {closeAllConnections();
                      message(paste(x, "out of", length(dataset_final), "networks")); 
    
                      run_model(data = dataset_final[[x]],
                                measure = "OR",
                                model = "RE",
                                heter_prior = list("lognormal", 
                                                   nmalist_final_pred[x, "mean"], 
                                                   1 / (nmalist_final_pred[x, "sd"]^2)),
                                D = ifelse(nmalist_final[x, "Harmful.Beneficial.Outcome"] == "Beneficial", 1, 0),
                                n_chains = 3,
                                n_iter = 100,
                                n_burnin = 10,
                                n_thin = 10)[nma_save]})

# Add the class
for (i in 1:length(dataset_final)) {
  class(nma_res[[i]]) <- "run_model"
}

# Save results in .RData
#save(nma_res, file = "./NMA results.RData")

