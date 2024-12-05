#*******************************************************************************
#*
#*                       Creating Supplementary Figure S2                                                                                                                                                                                   
#* 
#* Author: Loukia Spineli  
#* Date: November 2024
#*******************************************************************************


## Load libraries
list.of.packages <- c("ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load functions ----
source("./R/Functions/complete analysis results_function.R")


## Load analysis database ----
complete_res <- complete_analysis_results()


## Assess posterior distributions
# Prepare dataset
data_set <- 
  complete_res[, c("single_study", "direct_mean", "direct_sd", "indirect_mean", "indirect_sd", "diff_mean", "diff_sd", "tau_median", "tau_sd")]

# Scatter plot of posterior SD of *direct* versus mean
plot_dir <-
  ggplot(data_set,
         aes(x = direct_mean,
             y = direct_sd,
             colour = factor(single_study, levels = c("Yes", "No")))) +
  geom_point(size = 3) +
  scale_colour_manual(breaks = c("Yes", "No"),
                      values = c("#DF536B", "#61D04F")) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0, 1.5),
                     limits = c(0, 1.6)) +
  labs(x = " ",
       y = "Posterior standard deviation",
       colour = "Single-study split node") +
  ggtitle("Direct effects") + 
  theme_classic() +
  theme(title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))

# Scatter plot of posterior SD of *indirect* versus mean
plot_indir <- 
  ggplot(data_set,
         aes(x = indirect_mean,
             y = indirect_sd,
             colour = factor(single_study, levels = c("Yes", "No")))) +
  geom_point(size = 3) +
  scale_colour_manual(breaks = c("Yes", "No"),
                      values = c("#DF536B", "#61D04F")) +
  labs(x = "Posterior mean",
       y = " ",
       colour = "Single-study split node") +
  scale_y_continuous(breaks = c(0, 0.5, 1.0, 1.5, 2.0),
                     limits = c(0, 2.0)) +
  ggtitle("Indirect effects") + 
  theme_classic() +
  theme(title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14))

# Scatter plot of posterior SD of *inconsistency* versus mean
plot_diff <- 
  ggplot(data_set,
         aes(x = diff_mean,
             y = diff_sd,
             colour = factor(single_study, levels = c("Yes", "No")))) +
  geom_point(size = 3) +
  scale_colour_manual(breaks = c("Yes", "No"),
                      values = c("#DF536B", "#61D04F")) +
  labs(x = "Posterior mean",
       y = "Posterior standard deviation",
       colour = "Single-study split node") +
  scale_y_continuous(breaks = c(0, 0.5, 1.0, 1.5, 2.0),
                     limits = c(0, 2.20)) +
  ggtitle("Inconsistency") + 
  theme_classic() +
  theme(title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14))

# Scatter plot of posterior SD of *tau* versus median
plot_tau <- 
  ggplot(data_set,
         aes(x = tau_median,
             y = tau_sd,
             colour = factor(single_study, levels = c("Yes", "No")))) +
  geom_point(size = 3) +
  scale_colour_manual(breaks = c("Yes", "No"),
                      values = c("#DF536B", "#61D04F")) +
  labs(x = "Posterior median",
       y = " ",
       colour = "Single-study split node") +
  scale_x_continuous(breaks = c(0, 0.3, 0.6, 0.9, 1.2),
                     limits = c(0, 1.2)) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6),
                     limits = c(0, 0.65)) +
  ggtitle("Between-study standard deviation") + 
  theme_classic() +
  theme(title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14))


## Bring all together and save Figure S2
tiff("./Figures/Figure S2.tiff", 
     height = 25, 
     width = 33, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(plot_dir, plot_indir, plot_diff, plot_tau,
          labels = c("a)", "b)", "c)", "d)"),
          nrow = 2,
          ncol = 2,
          legend = "bottom",
          common.legend = TRUE)
dev.off()

