#*******************************************************************************
#*
#*                        Creating Table 1 of Manuscript                                                                                                                                                
#*              Descriptive statistics of network characteristics                                                                                           
#*                                                                                                                                                                   
#* Date: November 2024
#*******************************************************************************


## Load library
list.of.packages <- c("nmadb", "netmeta", "gemtc", "rnmamod")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load functions 
source("./30_Analysis/Functions/convert_long_to_wide_function.R")
source("./30_Analysis/Functions/function.collection_function.R")


## Load full database
# List of datasets
load("./31_Database/Complete nmadb database/REDcap NMA dataset.RData") # Downloaded from nmadb on 07/10/2024

# Data-frame with characteristics of the datasets
load("./31_Database/Complete nmadb database/Data-frame characteristics datasets.RData") 


              #*************************************************#           
              #*                                               *#                            
              #*     Remove networks with unavailable data     *#
              #*                                               *#
              #*************************************************#

## A) Find and remove networks with 'empty' datasets (NULL is returned)
# Count 'empty' datasets 
table(unlist(lapply(list.database, function(x) is.null(x)))) # FALSE: 296, TRUE: 157

# Find 'empty' (NULL) datasets (Correct! :-))
empty_datasets <- which(lapply(list.database, function(x) is.null(x)) == TRUE) 

# Number of datasets *removed* for having unavailable data
length(empty_datasets) # 157


#************************************************************************************************
#' There are *167* networks with empty data; however, the data-frame of the collected networks 
#' (here, named as 'nmalist.reduced') considers *157* networks with 'Data.available == "False"'.
#' This inconsistency pertains to 10 networks with 'Record.ID' 479911, 480611, 480617, 501237, 
#' 501274, 501308, 501316, 501333, 501410, 501426. These networks contain contrast-based data, 
#' which are not considered in the present study.

# Record.ID of networks in 'nmalist.reduce' where 'Data.available == "False"' (167 networks)
get_nmalist_id <- subset(nmalist.reduced, Data.available == "False")[, 1]; length(get_nmalist_id)

# Record.ID of networks in 'list.database' with NULL (157 networks)
get_empty_id <- as.numeric(names(empty_datasets)); length(get_empty_id)

# Get the Record.ID of networks where 'get_nmalist_id' and 'get_empty_id' disagree
get_nmalist_id[!is.element(get_nmalist_id, get_empty_id)]

#************************************************************************************************


# Remove empty cells, which leads to 296 extracted datasets
#' (From 'list.database')
with_data <- Filter(Negate(is.null), list.database)

# Number of *eligible* networks *with* data after removing those without data
length(with_data) 

#' (From 'nmalist.reduced')
nmalist_with_data <- nmalist.reduced[match(names(with_data), nmalist.reduced$Record.ID), ]; dim(nmalist_with_data)


         #*******************************************************************#                               
         #*                                                                 *#                            
         #*     Remove networks with continuous, rate and survival data     *#
         #*                                                                 *#
         #*******************************************************************#

## B) Find and remove networks with 'Continuous', 'Rate' or 'Survival' outcomes (use 'Type.of.Outcome.' in 'nmalist.reduced')
# Extract the 'Record.ID' of datasets with 'Continuous', 'Rate' or 'Survival' outcomes 
get_candidate_ID <- 
  subset(nmalist_with_data, is.element(Type.of.Outcome., c("Continuous", "Rate", "Survival")))[, "Record.ID"]

# Number of networks with *ineligible* outcome type ('Continuous', 'Rate' or 'Survival' outcomes )
length(get_candidate_ID) # 101

# Remove these networks
with_data <- with_data[!names(with_data) %in% get_candidate_ID]

# Number of *eligible* networks with *binary* outcome only
length(with_data)


         #************************************************************#                                               
         #*                                                          *#                            
         #*     Remove networks with contrast-based outcome data     *#
         #*                                                          *#
         #************************************************************#

## C) Remove networks with contrast-based data (I will apply one-stage node-splitting)
# Restrict to networks *without* arm level data ('effect' appears in the columns)
with_contrast_based <- 
  Filter(function(x) length(x[, startsWith(colnames(x), "effect") & endsWith(colnames(x), "effect")]) > 0, with_data)

# Number of networks *removed* for having contrast-based outcome data
length(with_contrast_based) # 10

# Ensure that only contrast-based data exist in 'with_contrast_based': 'effect', and 'se' *must* appear in the columns
without <- lapply(with_contrast_based, function(x) colnames(x)) 

# Restrict to networks with arm level data ('effect' does *not* appear in the columns)
with_arm_level <- Filter(function(x) length(x[, startsWith(colnames(x), "effect") & endsWith(colnames(x), "effect")]) == 0, with_data)

# Number of *eligible* after removing those with contrast-based outcome data
length(with_arm_level)

# Ensure that only arm-level data exist in 'with_arm_level': 'effect', and 'se' *do not* appear in the columns
with <- lapply(with_arm_level, function(x) colnames(x))


         #*************************************************************#                                                               
         #*                                                           *#                            
         #*       Edit further the datasets with binary outcomes      *#   
         #*                                                           *#
         #*************************************************************#

## D) Edit further the networks 
# Remove studies with 'r' having non-numeric elements
with_binary_a <- Filter(function(x) is.numeric(x[, "r"]), with_arm_level) 

#**********************************************************************************
#* # Number of studies per network
num_stud_a <- unlist(lapply(with_binary_a, function(x) length(unique(x$id))))
#*
#* Summarise number of studies across networks
summary(num_stud_a) # Min.: 5
#* 
#* # Number of treatments per network
num_treat_a <- unlist(lapply(with_binary_a, function(x) length(unique(x$t))))
#*
#* Summarise number of unique treatments across networks
summary(num_treat_a) # Min.: 4
#*
#* Is there any network with 'num_stud_a' < 'num_treat_a'? NOPE!
length(which((num_stud_a / num_treat_a) < 1)) # No network!
#**********************************************************************************

# Remove studies with 0 number of randomised
with_binary_b <- lapply(with_binary_a, function(x) subset(x, n > 0)) 

#**********************************************************************************
#* # Number of studies per network
num_stud_b <- unlist(lapply(with_binary_b, function(x) length(unique(x$id))))
#*
#* Summarise number of studies across networks
summary(num_stud_b) # Min.: 5
#* 
#* # Number of treatments per network
num_treat_b <- unlist(lapply(with_binary_b, function(x) length(unique(x$t))))
#*
#* Summarise number of unique treatments across networks
summary(num_treat_b) # Min.: 4
#*
#* Is there any network with 'num_stud_b' < 'num_treat_b'? NOPE!
length(which((num_stud_b / num_treat_b) < 1)) # No network!
#**********************************************************************************

# Remove studies with at least one arm with risk < 0.15 (convergence and estimation issues due to separation)
with_binary_c0 <- lapply(lapply(with_binary_b, 
                               function(x) subset(x, (r / n >= 0.15))), # remove studies with at least one arm with risk < 0.15 
                        function(x) x[ave(rep(1, nrow(x)), x$id, FUN = length) > 1, ]) # remove single-arm studies

# Remove studies with at least one arm with risk = 1.00 (convergence and estimation issues due to separation)
with_binary_c <- lapply(lapply(with_binary_c0, 
                               function(x) subset(x, (r / n <= 0.85))), # remove studies with at least one arm with risk > 10.85
                        function(x) x[ave(rep(1, nrow(x)), x$id, FUN = length) > 1, ]) # remove single-arm studies

# Remove networks that lost all studies from the previous step (with_binary_c)
with_binary <- Filter(function(x) dim(x)[1] > 0, with_binary_c)

# Number of networks *removed* after step 'with_binary_c'
length(with_binary_c) - length(with_binary) # 38  

# Number of networks with at least one study with one or more arms with event risk < 5% or = 100%
length(which(unlist(lapply(with_binary_b, function(x) dim(x)[1])) - unlist(lapply(with_binary_c, function(x) dim(x)[1])) > 0))

# Rename the treatments after losing some treatments 
with_binary_d <- lapply(with_binary, function(x) {x$t <- as.numeric(as.factor(x$t)); x})

#**********************************************************************************
#* # Number of studies per network
num_stud_d <- unlist(lapply(with_binary_d, function(x) length(unique(x$id))))
#*
#* Summarise number of studies across networks
summary(num_stud_d) # Min.: 1
#* 
#* # Number of treatments per network
num_treat_d <- unlist(lapply(with_binary_d, function(x) length(unique(x$t))))
#*
#* Summarise number of unique treatments across networks
summary(num_treat_d) # Min.: 2
#*
#* Is there any network with 'num_stud_fin' < 'num_treat_fin'? 
length(which((num_stud_d / num_treat_d) < 1)) # 34 networks
#**********************************************************************************

# Remove networks with less than 3 treatments or more treatments than studies
with_binary_fin0 <- with_binary_d[-which(num_treat_d < 3 | num_stud_d <= num_treat_d)]

# Number of *removed* networks with less than 3 treatments or more treatments than studies
length(with_binary_d) - length(with_binary_fin0) # 51


                    #**************************************#                                                                                                    
                    #*                                    *#                            
                    #*     Check network connectivity     *# 
                    #*     <Using 'netmeta' R package>    *# 
                    #*                                    *#
                    #**************************************#

## E) Check network connectivity (ALL CONNECTED!)
# First turn into wide format
binary_wide <- lapply(with_binary_fin0, function(x) pairwise(treat = t, event = r, n = n, studlab = id, data = x))

# Then, find the disconnected networks
binary_disconnected <- 
  unlist(lapply(binary_wide, function(x) netconnection(t1, t2, data = as.data.frame(x))$n.subnet)); max(binary_disconnected)

# Remove disconnected networks
with_binary_fin <- with_binary_fin0[-which(binary_disconnected > 1)]

# Number of networks *removed* for being disconnected
length(which(binary_disconnected > 1)) # 6

# Number of *eligible* networks after removing the disconnected ones
length(with_binary_fin)


       #********************************************************************************************#                                                                                                    
       #*                                                                                          *#                            
       #*     Remove networks without closed loops or loops informed by multi-arm studies only     *# 
       #*                              <Using the 'gemtc' R package>                               *# 
       #*                                                                                          *#
       #********************************************************************************************#

## F) Check networks for closed loops
# Rename restricted datasets to apply the 'mtc.nodesplit.comparisons' gemtc function
binary_subset <- lapply(with_binary_fin, function(x) {x <- x[, c("id", "t", "r", "n")]; colnames(x) <- c("study", "treatment", "responders", "sampleSize"); x})

# Apply the 'mtc.nodesplit.comparisons' gemtc function
binary_split_nodes <- lapply(binary_subset, function(x) mtc.nodesplit.comparisons(mtc.network(x)))

# Find networks with split nodes (they have at least one closed-loop not informed by multi-arm studies exclusively)
binary_find_eligible <- which(lapply(binary_split_nodes, function(x) dim(x)[1] > 0) == TRUE)

# Keep only networks with split nodes 
binary_eligible <- binary_subset[binary_find_eligible]

# Number of networks *removed* for not having any closed-loop
length(binary_subset[-binary_find_eligible]) # 33

# Number of *eligible* networks after removing those without any closed-loop
length(binary_eligible) # 57


## G) Convert dataset from long to wide to apply the 'rnmamod' R package
# Rnmamod will resolve any inconsistency in the order of the treatments in each study
dataset_final <- lapply(binary_eligible, function(x) convert_long_to_wide(x)) 

# Save final dataset
#save(dataset_final, file = "./31_Database/Analysed database/dataset final.RData")


## H) Match 'Record.ID' in 'nmalist.reduced' with that in the 'dataset_final' (final dataset of networks)
nmalist_final <- nmalist_with_data[match(names(dataset_final), nmalist_with_data$Record.ID), ]; dim(nmalist_final)

# Save the final data-frame
#save(nmalist_final, file = "./31_Database/Analysed database/nmalist final.RData")
