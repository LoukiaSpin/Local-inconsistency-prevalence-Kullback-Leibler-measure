#*******************************************************************************
#*
#*                       Creating Supplementary Figure 3  
#*                                              
#* Author: Loukia Spineli                                                                                                                                                                 
#* Date: November 2024
#*******************************************************************************


## Load libraries ----
list.of.packages <- c("dplyr", "plyr", "ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load functions ----
source("./R/Functions/complete analysis results_function.R")


## Complete analysis results ----
complete_res <- complete_analysis_results()


## Stacked bar plots on (in)consistency conclusion from standard decision versus proposed index for *split nodes*
# Prepare dataset for *split nodes*
data_barplot_node <- data.frame(single_study = complete_res$single_study, 
                                node_kld = complete_res$node_conclusion, 
                                node_standard = complete_res$node_standard)
data_barplot_node$single_study <- revalue(data_barplot_node$single_study, 
                                          c("Yes" = "Split nodes with one study", "No" = "Split nodes with more studies"))

# Rename the levels of 'node_kld'
data_barplot_node$node_kld <- 
  revalue(data_barplot_node$node_kld, c("Consistency" = "Acceptably low", "Inconsistency" = "Material"))


# Calculate % conditionally on standard decision for *split nodes*
conclusion_node <- data_barplot_node %>%
  dplyr::group_by(single_study, node_standard, node_kld) %>%
  dplyr::count() %>%
  dplyr::group_by(single_study, node_standard) %>%
  dplyr::mutate(perc = n / sum(n))


# Stacked barplots 
tiff("./Figures/Figure S3.tiff", 
     height = 18, 
     width = 38, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(conclusion_node, 
       aes(x = node_standard,
           y = perc,
           fill = node_kld)) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(x = node_standard,
                y = perc,
                group = node_kld,
                label = ifelse(perc != 0, paste0(round(perc * 100, 0), "% (", n,")"), " ")),
            hjust = 0.5,
            vjust = 1.0,
            size = 4.5,
            position = "stack",
            colour = "white") +
  facet_grid(cols = vars(single_study)) + 
  labs(x = "Inconsistency based on 95% credible interval of inconsistency factor",
       y = "Percentage split nodes (%)",
       fill = "Inconsistency based on index") +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  scale_y_continuous(labels = scales::label_percent(suffix = " ")) +
  theme_classic() +
  theme(title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))
dev.off()
