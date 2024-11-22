#*******************************************************************************
#*
#*          Apply the node-splitting approach to the analysed database                                                                                                                                            
#*                       (Use of the 'rnmamod' R package) 
#*                                                                                                                                                                   
#* Date: November 2024
#*******************************************************************************


## Load library
library(rnmamod)


## Load function ----
source("./30_Analysis/Functions/function.collection_function.R")


## Load analysis database ----
load("./30_Analysis/NMA results.RData")


## Apply RE node-splitting ----
node_res <- 
  lapply(1:length(nma_res), 
         function(x) {closeAllConnections();
                      message(paste(x, "out of", length(nma_res), "networks")); 
    
                      run_nodesplit(full = nma_res[[x]],
                                    n_chains = 3,
                                    n_iter = 20000,
                                    n_burnin = 2000,
                                    n_thin = 10)})

# Save results in .RData
save(node_res, file = "./30_Analysis/Node-splitting results.RData")
