#*******************************************************************************
#*
#*                       Creating Supplementary Figure 5    
#*                                            
#* Author: Loukia Spineli                                                                                                                                                                   
#* Date: November 2024
#*******************************************************************************


## Load libraries ----
list.of.packages <- c("patchwork", "ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load functions ----
source("./R/Functions/complete analysis results_function.R")


## Complete analysis results ----
complete_res <- complete_analysis_results()


## Prepare dataset for scatter plot
# Restrict to the split nodes with D >= 0.64
complete_res_mat <- subset(complete_res, kld_value >= 0.64)

# Restrict to the split nodes with D < 0.64
complete_res_low <- subset(complete_res, kld_value < 0.64)


## Found number of single-study split nodes
# Percentages for split nodes with D >= 0.64
get_count_mat <- table(complete_res_mat$single_study)

# Percentages for split nodes with D < 0.64
get_count_low <- table(complete_res_low$single_study)


## Posterior SD versus posterior mean of inconsistency: *Split nodes with D < 0.64*
# Scatter plot
plot_incon_low <-
  ggplot(complete_res_low,
         aes(x = diff_mean,
             y = diff_sd,
             colour = single_study)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", 
              se = TRUE) +
  geom_rug() +
  labs(x = "Posterior mean of inconsistency factor",
       y = "Posterior standard deviation of inconsistency factor",
       colour = "Single-study split node") + 
  scale_color_manual(breaks = c("Yes", "No"),
                     labels = c(paste0("Yes (", get_count_low[1], ")"), paste0("No (", get_count_low[2], ")")), 
                     values = c("#DF536B", "#61D04F")) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

# Density of posterior mean of inconsistency per split node size group
dens_incon_low1 <- 
  ggplot(complete_res_low, 
         aes(x = diff_mean,
             fill = single_study)) + 
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#DF536B", "#61D04F")) +
  theme_void() + 
  theme(legend.position = "none")

# Density of posterior SD of inconsistency per split node size group
dens_incon_low2 <- 
  ggplot(complete_res_low, 
         aes(x = diff_sd,
             fill = single_study)) + 
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#DF536B", "#61D04F")) +
  coord_flip() +
  theme_void() + 
  theme(legend.position = "none")

# Scatter plot with side histograms
together_incon_low <- 
  (dens_incon_low1 + 
  plot_spacer() + 
  plot_incon_low + 
  dens_incon_low2) +
  plot_layout(ncol = 2, 
              nrow = 2, 
              widths = c(4, 1), 
              heights = c(1, 4)) 
  

## Posterior SD versus posterior mean of inconsistency: *Split nodes with D >= 0.64*
# Scatter plot
plot_incon_mat <-
  ggplot(complete_res_mat,
         aes(x = diff_mean,
             y = diff_sd,
             colour = single_study)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", 
              se = TRUE) +
  geom_rug() +
  labs(x = "Posterior mean of inconsistency factor",
       y = "Posterior standard deviation of inconsistency factor",
       colour = "Single-study split node") + 
  scale_color_manual(breaks = c("Yes", "No"),
                     labels = c(paste0("Yes (", get_count_mat[1], ")"), paste0("No (", get_count_mat[2], ")")), 
                     values = c("#DF536B", "#61D04F")) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

# Density of posterior mean of inconsistency per split node size group
dens_incon_mat1 <- 
  ggplot(complete_res_mat, 
         aes(x = diff_mean,
             fill = single_study)) + 
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#DF536B", "#61D04F")) +
  theme_void() + 
  theme(legend.position = "none")

# Density of posterior SD of inconsistency per split node size group
dens_incon_mat2 <- 
  ggplot(complete_res_mat, 
         aes(x = diff_sd,
             fill = single_study)) + 
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#DF536B", "#61D04F")) +
  coord_flip() +
  theme_void() + 
  theme(legend.position = "none")

# Scatter plot with side histograms
together_incon_mat <- 
  (dens_incon_mat1 + 
     plot_spacer() + 
     plot_incon_mat + 
     dens_incon_mat2) +
  plot_layout(ncol = 2, 
              nrow = 2, 
              widths = c(4, 1), 
              heights = c(1, 4)) 


## Bring together and save Figure S5
tiff("./Figures/Figure S5.tiff", 
     height = 22, 
     width = 38, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(together_incon_mat, together_incon_low, 
          labels = c("a)", "b)"),
          ncol = 2,
          common.legend = TRUE,
          legend = "none")
dev.off()


## Summarise posterior mean of inconsistency based on split node with D >=0.64
# Split nodes with more studies
summary(subset(complete_res_mat, single_study == "No")$diff_mean)

# Single-studies split nodes
summary(subset(complete_res_mat, single_study == "Yes")$diff_mean)
