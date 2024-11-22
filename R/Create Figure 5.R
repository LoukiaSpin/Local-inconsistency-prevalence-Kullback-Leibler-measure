#*******************************************************************************
#*
#*                        Creating Figure 5 of Manuscript                           
#*             <Association of KLD with statistical heterogeneity>                                                         
#*                                                                                                                                                                   
#* Date: November 2024
#*******************************************************************************


## Load libraries ----
list.of.packages <- c("patchwork", "ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load functions ----
source("./30_Analysis/Functions/complete analysis results_function.R")


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


## Summary of posterior tau per split node group 
# Acceptably low inconsistency
summary(subset(complete_res_low, single_study == "Yes")$tau_median)
summary(subset(complete_res_low, single_study == "No")$tau_median)

# Material inconsistency
summary(subset(complete_res_mat, single_study == "Yes")$tau_median)
summary(subset(complete_res_mat, single_study == "No")$tau_median)


## Index D versus posterior median of tau: *Split nodes with D >= 0.64*
# Scatter plot 
plot_tau_mat <-
  ggplot(complete_res_mat,
         aes(x = tau_median,
             y = kld_value,
             colour = single_study)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", 
              se = FALSE) +
  geom_rug() +
  labs(x = expression(bold(paste("Posterior median of ", tau))),
       y = expression(bold(paste("Interpretation index ", D^j))),
       colour = "Single-study split node") + 
  scale_color_manual(breaks = c("Yes", "No"),
                     labels = c(paste0("Yes (", get_count_mat[1], ")"), paste0("No (", get_count_mat[2], ")")), 
                     values = c("#DF536B", "#61D04F")) +
  scale_x_continuous(breaks = c(0, 0.3, 0.6, 0.9, 1.2),
                     labels = c(0, 0.3, 0.6, 0.9, 1.2),
                     limits = c(0, 1.2)) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

# Density of posterior median of tau per split node size group
dens_tau_mat1 <- 
  ggplot(complete_res_mat, 
         aes(x = tau_median,
             fill = single_study)) + 
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#DF536B", "#61D04F")) +
  theme_void() + 
  theme(legend.position = "none")

# Density of index D per split node size group
dens_tau_mat2 <- 
  ggplot(complete_res_mat, 
         aes(x = kld_value,
             fill = single_study)) + 
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#DF536B", "#61D04F")) +
  coord_flip() +
  theme_void() + 
  theme(legend.position = "none")

# Scatter plot with side histograms
together_mat <- 
  (dens_tau_mat1 + 
  plot_spacer() + 
  plot_tau_mat + 
  dens_tau_mat2) +
  plot_layout(ncol = 2, 
              nrow = 2, 
              widths = c(4, 1), 
              heights = c(1, 4)) 
  

## Index D versus posterior median of tau: *Split nodes with D < 0.64*
# Scatter plot 
plot_tau_low <-
  ggplot(complete_res_low,
         aes(x = tau_median,
             y = kld_value,
             colour = single_study)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", 
              se = FALSE) +
  geom_rug() +
  labs(x = expression(bold(paste("Posterior median of ", tau))),
       y = " ",
       colour = "Single-study split node") + 
  scale_color_manual(breaks = c("Yes", "No"),
                     labels = c(paste0("Yes (", get_count_low[1], ")"), paste0("No (", get_count_low[2], ")")), 
                     values = c("#DF536B", "#61D04F")) +
  scale_x_continuous(breaks = c(0, 0.3, 0.6, 0.9, 1.2),
                     labels = c(0, 0.3, 0.6, 0.9, 1.2),
                     limits = c(0, 1.2)) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

# Density of posterior median of tau per split node size group
dens_tau_low1 <- 
  ggplot(complete_res_low, 
         aes(x = tau_median,
             fill = single_study)) + 
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#DF536B", "#61D04F")) +
  theme_void() + 
  theme(legend.position = "none")

# Density of index D per split node size group
dens_tau_low2 <- 
  ggplot(complete_res_low, 
         aes(x = kld_value,
             fill = single_study)) + 
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#DF536B", "#61D04F")) +
  coord_flip() +
  theme_void() + 
  theme(legend.position = "none")

# Scatter plot with side histograms
together_low <- 
  (dens_tau_low1 + 
     plot_spacer() + 
     plot_tau_low + 
     dens_tau_low2) +
  plot_layout(ncol = 2, 
              nrow = 2, 
              widths = c(4, 1), 
              heights = c(1, 4)) 


## Bring together and save Figure 5
tiff("./30_Analysis/Figure 5.tiff", 
     height = 22, 
     width = 38, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(together_mat, together_low, 
          labels = c("A)", "B)"),
          ncol = 2,
          common.legend = TRUE,
          legend = "none")
dev.off()
