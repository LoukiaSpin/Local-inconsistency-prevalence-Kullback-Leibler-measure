#*******************************************************************************
#*
#*                       Creating Figure 1 of Manuscript             
#*                                                                                                                                                                   
#* Date: November 2024
#*******************************************************************************


## Load library ----
list.of.packages <- c("reshape2", "dplyr", "ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load functions ----
source("./R/Functions/complete analysis results_function.R")


## Complete analysis results ----
complete_res <- complete_analysis_results()


## Prepare dataset for violin plots on the distribution of IF, direct and indirect estimates
# Data-frame with posterior means
restrict_dataset_mean <- melt(complete_res[, c("direct_mean", "indirect_mean", "diff_mean")])

# Data-frame with posterior SDs
restrict_dataset_sd <- melt(complete_res[, c("direct_sd", "indirect_sd", "diff_sd")])

# Bring together
restrict_dataset <- cbind(restrict_dataset_mean, restrict_dataset_sd[, 2])

# Rename the estimate type
restrict_dataset$variable <- rep(c("Direct effect", "Indirect effect", "Inconsistency"), each = dim(complete_res)[1])

# Add the network ID and repeat
restrict_dataset$network_id <- rep(complete_res$network_id, 3)

# Add the number of studies informing each node and repeat
restrict_dataset$study_num <- rep(complete_res$Freq, 3)

# Rename properly the first three columns
colnames(restrict_dataset)[1:3] <- c("estimate", "mean", "sd")


## Add descriptive statistics to the plots
# Summarise for mean
summary_mean0 <- 
  restrict_dataset[, c("estimate", "mean", "study_num")] %>%
  group_by(estimate, single_study = ifelse(study_num == 1, "Yes", "No")) %>%
  summarize(median = median(mean),
            first = quantile(mean, 0.25),
            third = quantile(mean, 0.75),
            min = min(mean),
            max = max(mean))
summary_mean <- melt(as.data.frame(summary_mean0))

# Summarise for sd
summary_sd0 <- 
  restrict_dataset[, c("estimate", "sd", "study_num")] %>%
  group_by(estimate, single_study = ifelse(study_num == 1, "Yes", "No")) %>%
  summarize(median = median(sd),
            first = quantile(sd, 0.25),
            third = quantile(sd, 0.75),
            min = min(sd),
            max = max(sd))
summary_sd <- melt(as.data.frame(summary_sd0))


## Found number of single-study split nodes
# Counts
(dist_single_nodes <- 
    table(factor(ifelse(subset(restrict_dataset, estimate == "Direct effect")$study_num == 1, "Yes", "No"), levels = c("Yes", "No"))))


## Box plots with integrated dots on the distribution of IF, direct and indirect estimates by split node group
# Posterior mean of parameters
plot_mean <- 
  ggplot(restrict_dataset,
         aes(x = factor(estimate, levels = c("Inconsistency", "Direct effect", "Indirect effect")),
             y = mean,
             colour = factor(ifelse(study_num == 1, "Yes", "No"), levels = c("Yes", "No")))) +
  geom_boxplot(size = 1, 
               position = position_dodge(1),
               outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, 
                                             dodge.width = 1),
             size = 1.3,
             alpha = .4) + 
  geom_text(data = subset(summary_mean, variable == "median"),
            aes(x = factor(estimate, levels = c("Inconsistency", "Direct effect", "Indirect effect")),
                y = value,
                group = factor(single_study, levels = c("Yes", "No")),
                label = paste0("Q2 = ", sprintf("%.2f", value))),
            size = 3.4, 
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = -0.15,
            inherit.aes = FALSE) +
  geom_text(data = subset(summary_mean, variable == "first"),
            aes(x = factor(estimate, levels = c("Inconsistency", "Direct effect", "Indirect effect")),
                y = value,
                group = factor(single_study, levels = c("Yes", "No")),
                label = paste0("Q1 = ", sprintf("%.2f", value))),
            size = 3.4,
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = -0.15,
            inherit.aes = FALSE) +
  geom_text(data = subset(summary_mean, variable == "third"),
            aes(x = factor(estimate, levels = c("Inconsistency", "Direct effect", "Indirect effect")),
                y = value,
                group = factor(single_study, levels = c("Yes", "No")),
                label = paste0("Q3 = ", sprintf("%.2f", value))),
            size = 3.4,
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = -0.15,
            inherit.aes = FALSE) +
  geom_text(data = subset(summary_mean, variable == "min"),
            aes(x = factor(estimate, levels = c("Inconsistency", "Direct effect", "Indirect effect")),
                y = value,
                group = factor(single_study, levels = c("Yes", "No")),
                label = paste0("min. = ", sprintf("%.2f", value))),
            size = 3.4,
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = 0.4,
            inherit.aes = FALSE) +
  geom_text(data = subset(summary_mean, variable == "max"),
            aes(x = factor(estimate, levels = c("Inconsistency", "Direct effect", "Indirect effect")),
                y = value,
                group = factor(single_study, levels = c("Yes", "No")),
                label = paste0("max. = ", sprintf("%.2f", value))),
            size = 3.4,
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = -0.4,
            inherit.aes = FALSE) +
  scale_color_manual(breaks = c("Yes", "No"),
                     labels = c(paste0("Yes (", dist_single_nodes[1], ")"), paste0("No (", dist_single_nodes[2], ")")), 
                     values = c("#DF536B", "#61D04F")) +
  labs(y = "Posterior mean (log odds ratio scale)", 
       x = "Investigated parameter",
       colour = "Single-study split node") +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14, face = "bold"))

# Posterior SD of parameters
plot_sd <- 
  ggplot(restrict_dataset,
         aes(x = factor(estimate, levels = c("Inconsistency", "Direct effect", "Indirect effect")),
             y = sd,
             colour = factor(ifelse(study_num == 1, "Yes", "No"), levels = c("Yes", "No")))) +
  geom_boxplot(size = 1, 
               position = position_dodge(1),
               outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, 
                                             dodge.width = 1),
             size = 1.3,
             alpha = .4) + 
  geom_text(data = subset(summary_sd, variable == "median"),
            aes(x = factor(estimate, levels = c("Inconsistency", "Direct effect", "Indirect effect")),
                y = value,
                group = factor(single_study, levels = c("Yes", "No")),
                label = paste0("Q2 = ", sprintf("%.2f", value))),
            size = 3.4, 
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = -0.15,
            inherit.aes = FALSE) +
  geom_text(data = subset(summary_sd, variable == "first"),
            aes(x = factor(estimate, levels = c("Inconsistency", "Direct effect", "Indirect effect")),
                y = value,
                group = factor(single_study, levels = c("Yes", "No")),
                label = paste0("Q1 = ", sprintf("%.2f", value))),
            size = 3.4,
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = -0.15,
            inherit.aes = FALSE) +
  geom_text(data = subset(summary_sd, variable == "third"),
            aes(x = factor(estimate, levels = c("Inconsistency", "Direct effect", "Indirect effect")),
                y = value,
                group = factor(single_study, levels = c("Yes", "No")),
                label = paste0("Q3 = ", sprintf("%.2f", value))),
            size = 3.4,
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = -0.15,
            inherit.aes = FALSE) +
  geom_text(data = subset(summary_sd, variable == "min"),
            aes(x = factor(estimate, levels = c("Inconsistency", "Direct effect", "Indirect effect")),
                y = value,
                group = factor(single_study, levels = c("Yes", "No")),
                label = paste0("min. = ", sprintf("%.2f", value))),
            size = 3.4,
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = 0.4,
            inherit.aes = FALSE) +
  geom_text(data = subset(summary_sd, variable == "max"),
            aes(x = factor(estimate, levels = c("Inconsistency", "Direct effect", "Indirect effect")),
                y = value,
                group = factor(single_study, levels = c("Yes", "No")),
                label = paste0("max. = ", sprintf("%.2f", value))),
            size = 3.4,
            position = position_dodge(width = 1),
            check_overlap = TRUE,
            hjust = 0.5, 
            vjust = -0.4,
            inherit.aes = FALSE) +
  scale_color_manual(breaks = c("Yes", "No"),
                     labels = c(paste0("Yes (", dist_single_nodes[1], ")"), paste0("No (", dist_single_nodes[2], ")")), 
                     values = c("#DF536B", "#61D04F")) +
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2),
                     labels = c(0, 0.5, 1, 1.5, 2),
                     limits = c(0, 2.3),
                     expand = c(0, 0.01)) +
  labs(y = "Posterior standard deviation", 
       x = "Investigated parameter",
       colour = "Single-study split node") +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14, face = "bold"))


## Bring together
tiff("./Figures/Figure 1.tiff", 
     height = 25, 
     width = 45, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(plot_mean, plot_sd,
          labels = c("A)", "B)"),
          common.legend = TRUE,
          legend = "bottom")
dev.off()
