#*******************************************************************************
#*
#*                       Creating Supplementary Figure 4   
#*                                             
#* Author: Loukia Spineli                                                                                                                                                                    
#* Date: November 2024
#*******************************************************************************


## Load libraries ----
list.of.packages <- c("reshape2", "ggplot2")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load functions ----
source("./R/Functions/complete analysis results_function.R")


## Complete analysis results ----
complete_res <- complete_analysis_results()


## Add descriptive statistics to the plots
# Summarise per group (remove outlying split node)
summary_group0 <- 
  subset(complete_res, kld_value < 40)[, c("kld_value", "single_study")] %>%
  group_by(single_study) %>%
  summarize(median = median(kld_value),
            first = quantile(kld_value, 0.25),
            third = quantile(kld_value, 0.75),
            min = min(kld_value),
            max = max(kld_value),
            lower = quantile(kld_value, 0.025),
            upper = quantile(kld_value, 0.975))
summary_group <- melt(as.data.frame(summary_group0))


## Get the violin plot with integrated box plot and dots and save Figure S4
tiff("./Figures/Figure S4.tiff", 
     height = 18, 
     width = 38, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(subset(complete_res, kld_value < 40),
       aes(x = single_study,
           y = kld_value,
           fill = single_study)) +
  geom_violin(trim = FALSE,
              alpha = 0.3) +
  geom_boxplot(outlier.alpha = 0.3,
               fill = "white",
               colour = "black",
               varwidth = TRUE,
               outlier.colour = "blue") +
  stat_boxplot(geom = 'errorbar',
               width = 0.2,
               linetype = "dashed") +
  geom_text(data = subset(summary_group, variable == "max"),
            aes(x = factor(single_study, levels = c("Yes", "No")),
                y = value,
                label = paste0("max. = ", sprintf("%.2f", value))),
            size = 3.0, 
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = -0.15,
            inherit.aes = FALSE) +
  geom_text(data = subset(summary_group, variable == "third"),
            aes(x = factor(single_study, levels = c("Yes", "No")),
                y = value,
                label = paste0("Q3 = ", sprintf("%.2f", value))),
            size = 3.0, 
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = -0.15,
            inherit.aes = FALSE) +
  geom_text(data = subset(summary_group, variable == "median"),
            aes(x = factor(single_study, levels = c("Yes", "No")),
                y = value,
                label = paste0("Q2 = ", sprintf("%.2f", value))),
            size = 3.0, 
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = -0.15,
            inherit.aes = FALSE) +
  geom_text(data = subset(summary_group, variable == "first"),
            aes(x = factor(single_study, levels = c("Yes", "No")),
                y = value,
                label = paste0("Q1 = ", sprintf("%.2f", value))),
            size = 3.0, 
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = -0.15,
            inherit.aes = FALSE) +
  geom_text(data = subset(summary_group, variable == "min"),
            aes(x = factor(single_study, levels = c("Yes", "No")),
                y = value,
                label = paste0("min. = ", sprintf("%.3f", value))),
            size = 3.0, 
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = 0.85,
            inherit.aes = FALSE) +
  scale_fill_manual(breaks = c("No", "Yes"),
                    values = c("#61D04F", "#DF536B")) +
  labs(x = "Single-study split nodes",
       y = expression(bold(paste("Interpretation index ", D^j)))) +
  theme_classic() +
  guides(fill = "none") +
  ggtitle(expression(bold(paste("After excluding the outlying split node with more studies (", D^j, "=48.73)")))) +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        legend.position = "bottom")
dev.off()
