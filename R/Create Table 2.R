#*******************************************************************************
#*
#*                        Creating Table 2 of Manuscript                                                                                                                        
#*                                                                                                                                                                   
#* Date: November 2024
#*******************************************************************************


## Load library ----
list.of.packages <- c("rnmamod") #, "dplyr"
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)

## Load functions ----
source("./R/Functions/function.collection_function.R")
source("./R/Functions/convert_wide_to_pairwise_function.R")


## Load analysis database ----
# List of analysis (final) datasets
load("./data/Analysis datasets/dataset final.RData")

# Data-frame with characteristics of the (final) datasets
load("./data/Analysis datasets/nmalist final.RData") 


## Sort in decreasing order the treatments within study  (where applicable)
prepare_dataset <- 
  lapply(1:length(dataset_final), 
         function(x) {message(paste(x, "out of", length(dataset_final), "networks")); suppressMessages(
           data_preparation(data = dataset_final[[x]], 
                            measure = "OR"))})


## Get the networks after the above within-study treatment re-ordering
dataset_new <- lapply(prepare_dataset, function(x) data.frame(x$t, x$N, x$r))

# Check that the treatments are indeed sorted in ascending order within each study
table(unlist(lapply(dataset_new, function(x) apply(x[, c("t1", "t2")], 1, function(y) !is.unsorted(na.omit(y)))))) # All TRUE! :-)


## Summarise the number of studies ----
# Count number of studies per network
num_studies <- unlist(lapply(dataset_new, function(x) dim(x)[1]))

# Summarise across *all* networks
summary(num_studies)


## Summary of two-arm studies ----
# Restrict to the treatments columns 
get_interv <- lapply(dataset_new, function(x) x[, startsWith(colnames(x), "t")])

# Number of arms per study 
num_arms <- lapply(get_interv, function(x) apply(x, 1, function(y) {length(na.omit(y))}))

# Number of two-arm studies per network
num_two_arms <- unlist(lapply(num_arms, function(x) length(x[x == 2])))

# Percentage of two-arm studies per network
perc_two_arms <- round((num_two_arms / num_studies) * 100, 0)

# Summarise across *all* networks
summary(perc_two_arms)

# Number of networks containing only two-arm studies
length(which(perc_two_arms == 100)) # 13


## Summary of multi-arm studies ----
# Number of multi-arm studies per network
num_multi_arms <- unlist(lapply(num_arms, function(x) length(x[x > 2])))

# Find networks *with* multi-arm studies
length(which(num_multi_arms > 0)) # 44 networks

# Percentage of multi-arm studies per network
perc_multi_arms <- round((num_multi_arms / num_studies) * 100, 0)

# Summarise across *all* networks
summary(perc_multi_arms[perc_multi_arms > 0])

# Find networks where multi-arm trials *dominate*
which(num_multi_arms/num_two_arms > 1) # No such networks


## Summarise the sample size of the studies ----
# First, sum the arm size per study
total_size <- 
  lapply(dataset_new, function(x) {x$sum_N <- apply(x[, startsWith(colnames(x), "n")], 1, function(y) {sum(y, na.rm = TRUE)}); x})

# Use only the 'sum_N' from each network to sumamrise across the networks
sample_size <- lapply(total_size, function(x) x$sum_N)

# Summarise across *all* networks
summary(unlist(sample_size))


## Summarise the number of treatments ----
# Find the number of unique treatments per network
num_interv <- lapply(get_interv, function(x) length(unique(na.omit(unlist(x)))))

# Summarise across *all* networks
summary(unlist(num_interv))


## Summarise observed comparisons ----
# Restrict to treatment columns and turn into pairwise (like in 'netmeta')
pairwise_comp <- lapply(get_interv, function(x) convert_wide_to_pairwise(x))

# Indicate the pairwise comparison in each study: t1 vs t2
get_compar <- 
  lapply(pairwise_comp, function(x) apply(x[, c(2, 1)], 1, function(y) {paste(y, collapse = "vs")}))

# Number of unique observed pairwise comparisons in each study
num_obs_comp <- lapply(get_compar, function(x) length(unique(x))); summary(unlist(num_obs_comp))

# Percentage of observed comparisons per network
perc_obs_comp <- 
  round((unlist(num_obs_comp) / sapply(num_interv, function(x) dim(combn(x, 2))[2])) * 100, 0)

# Summarise across *all* networks
summary(perc_obs_comp)

# Find the fully connected networks
which(perc_obs_comp == 100)   # One fully connected network

# Now, summarise again without this one fully connected network
summary(perc_obs_comp[perc_obs_comp < 100]) 


## Summarise single-study comparisons ----
# Count the single-study comparisons in each network
num_single_comp <- 
  sapply(pairwise_comp, function(x) length(which(table(paste(x[, 2], "vs", x[, 1])) == 1)))

# Percentage of single-study comparisons in each network
perc_single_comp <- round((num_single_comp / unlist(num_obs_comp)) * 100, 0)

# Find all networks *with* single-study comparisons
length(which(perc_single_comp > 0)) # 56 networks 

# Summarise across *all* networks *with* single-study comparisons
summary(perc_single_comp[perc_single_comp > 0]) # Excluding datasets without single-study comparisons

# Find networks with *only* single-study comparisons
which(perc_single_comp == 100) # 1 Network

# Now, summarise again without this one  network
summary(perc_single_comp[perc_single_comp < 100 & perc_single_comp > 0]) 


## Summarise the number split nodes (all networks have at least one split node) ----
# Get the data-frame of split nodes per network 
find_split_nodes <- 
  lapply(1:length(dataset_new), 
         function(x) {message(paste(x, "out of", length(dataset_new), "networks")); suppressMessages(
           summary_network_chars(data = dataset_new[[x]], 
                                 measure = "OR"))})

# Number of split nodes per network  
num_split_nodes <- unlist(lapply(find_split_nodes, function(x) dim(x)[1]))

# Summarise across *all* networks
summary(num_split_nodes)

# Number of networks containing only one split node
length(which(num_split_nodes == 1)) # 16 networks


## Summarise single-study split nodes ----
# Count the number of studies for each unique pairwise comparison in each network
num_studies_by_compar <- lapply(get_compar, function(x) as.data.frame(table(x)))

# Indicate the pairwise comparison in each split node: t1 vs t2
node_into_compar <- 
  lapply(find_split_nodes, function(x) apply(x, 1, function(y) {paste(y, collapse = "vs")}))

# Match the observed comparisons in a network with the corresponding split nodes
#' Results are correct: num_studies_by_compar, node_into_compar and match_compar_with_node were compared for some randomly selected networks
match_compar_with_node <- 
  lapply(1:length(num_studies_by_compar), 
         function(x) num_studies_by_compar[[x]][is.element(num_studies_by_compar[[x]]$x, node_into_compar[[x]]), ])

# Restrict to single-study split nodes and count their frequency in each network
single_study_nodes <- 
  unlist(lapply(match_compar_with_node, function(x) dim(subset(x, Freq == 1))[1]))

# percentage of single-study split node in each network
perc_single_study_nodes <-
  round((single_study_nodes / sapply(find_split_nodes, function(x) dim(x)[1])) * 100, 0)

# Summarise across *all* networks *with at least one* single-study split node
summary(perc_single_study_nodes[perc_single_study_nodes > 0])

# Number of networks with also single-study split nodes
length(which(perc_single_study_nodes > 0)) # 49 networks

# Find networks *with* only single-study split nodes
length(which(perc_single_study_nodes == 100)) # 14 networks


## Summarise the outcome type (as 'Beneficial' and 'Harmful') ----
# Absolute frequencies across *all* networks
(freq1 <- table(nmalist_final$Harmful.Beneficial.Outcome)) 

# Relative frequencies across *all* networks 
round(prop.table(freq1) * 100, 0)                          


## Summarise the outcome type (as 'Objective', 'Semi-objective' and 'Subjective') ----
# Absolute frequencies across *all* networks 
(freq2 <- table(nmalist_final$Objectivity.of.outcome)) 

# Relative frequencies across *all* networks 
round(prop.table(freq2) * 100, 0)                      


## Summarise the treatment-comparison type ----
# Re-order the levels as wished
nmalist_final$Intervention.comparison.type <- 
  factor(nmalist_final$Intervention.comparison.type, 
         levels = c("pharmacological vs placebo", 
                    "pharmacological vs pharmacological",
                    "non-pharmacological vs any"))

# Absolute frequencies across *all* networks
(freq3 <- table(nmalist_final$Intervention.comparison.type)) 

# Relative frequencies *all* networks
round(prop.table(freq3) * 100, 0)    

