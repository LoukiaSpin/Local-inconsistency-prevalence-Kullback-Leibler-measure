#*******************************************************************************
#*
#*                       Creating Supplementary Figure 7      
#*                                          
#* Author: Loukia Spineli                                                                                                                                                                   
#* Date: November 2024
#*******************************************************************************


## Load libraries ----
library(rnmamod)


## Load function ----
source("./R/Functions/complete analysis results_function.R")


## Complete analysis results ----
complete_res <- complete_analysis_results()


## Prepare dataset for scatter plot
# Restrict to the split nodes with D < 0.64 and fairly high or extreme tau
complete_res_fin <- subset(complete_res, kld_value < 0.64 & tau_median >= 0.5)

# Keep the necessary columns
data_set <- complete_res_fin[, c("comparison", "direct_mean", "direct_sd", "indirect_mean", "indirect_sd", "diff_mean", "diff_sd")]

# Rename the levels of 'comparison' to be unique
data_set$comparison <- 
  ave(as.character(data_set$comparison), as.character(data_set$comparison),
      FUN = function(x) if (length(x) > 1) paste0(x[1], "(", seq_along(x), ")") else x[1])

# Panel of density plots; save Figure S7
tiff("./Figures/Figure S7.tiff", 
     height = 18, 
     width = 38, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
kld_inconsistency_user(dataset = data_set,
                       threshold = 0.64)$Density_plot
dev.off()
