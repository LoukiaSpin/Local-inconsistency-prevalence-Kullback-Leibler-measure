#*******************************************************************************
#*
#*                       Creating Figure 4 of Manuscript    
#*                                                                                                                
#* Author: Loukia Spineli                                                                                                                                                                   
#* Date: November 2024
#*******************************************************************************


## Load libraries ----
list.of.packages <- c("patchwork", "rnmamod", "ggplot2", "ggpubr") 
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load functions ----
source("./R/Functions/complete analysis results_function.R")
source("./R/Functions/bland.altman.plot_function.R")


## Complete analysis results ----
complete_res <- complete_analysis_results()


## Prepare dataset for scatter plot
# Approximate direct by indirect
complete_res$kld_dir <- kld_measure(mean_y = complete_res$indirect_mean,
                                    sd_y = complete_res$indirect_sd,
                                    mean_x = complete_res$direct_mean,
                                    sd_x = complete_res$direct_sd)$kld_x_true

# Approximate indirect by direct
complete_res$kld_indir <- kld_measure(mean_y = complete_res$indirect_mean,
                                      sd_y = complete_res$indirect_sd,
                                      mean_x = complete_res$direct_mean,
                                      sd_x = complete_res$direct_sd)$kld_y_true

# Dataset for ggplot2
data_set <- data.frame(kld_value = complete_res$kld_value, 
                       kld_diff_DvsI = complete_res$kld_dir - complete_res$kld_indir, # Difference between kld_dir and kld_indir
                       single_study = complete_res$single_study)


## Split dataset by size of split nodes
# Split nodes with one study
complete_res_single <- subset(complete_res, single_study == "Yes")

# Split nodes with many studies
complete_res_more <- subset(complete_res, single_study == "No")


## Summarise D_dir and D_ind per group
# Split nodes with one study 
summary(complete_res_single$kld_dir); quantile(complete_res_single$kld_dir, c(0.025, 0.975))
summary(complete_res_single$kld_indir); quantile(complete_res_single$kld_indir, c(0.025, 0.975))

# Split nodes with many studies
summary(complete_res_more$kld_dir); quantile(complete_res_more$kld_dir, c(0.025, 0.975))
summary(complete_res_more$kld_indir); quantile(complete_res_more$kld_indir, c(0.025, 0.975))


## Split nodes with single studies
# Scatter plot 
plot_single <-
  ggplot(subset(data_set, single_study == "Yes"),
         aes(x = kld_value,
             y = kld_diff_DvsI,
             colour = ifelse(kld_diff_DvsI > 0, "Direct effect", "Indirect effect"))) +
  geom_point(size = 2) +
  geom_abline(intercept = 0,
              slope = 0) +
  geom_rug() +
  scale_color_manual(breaks = c("Direct effect", "Indirect effect"),
                     values = c("#61D04F", "blue")) +
  labs(x = expression(bold(paste("Interpretation index ", D^j))),
       y = expression(bold(paste("KLD approximating direct versus indirect effect"))), 
       colour = "Larger KLD when approximating") + 
  ggtitle(paste0("Split nodes with one study", " (n = ", dim(complete_res_single)[1], ")")) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

# Density plot for x-axis
dens_single1 <- 
  ggplot(subset(data_set, single_study == "Yes"),
         aes(x = kld_value,
             fill = ifelse(kld_diff_DvsI > 0, "Direct effect", "Indirect effect"))) +
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Direct effect", "Indirect effect"),
                    values = c("#61D04F", "blue")) +
  theme_void() + 
  theme(legend.position = "none")

# Density plot for y-axis
dens_single2 <- 
  ggplot(subset(data_set, single_study == "Yes"),
         aes(x = kld_diff_DvsI,
             fill = ifelse(kld_diff_DvsI > 0, "Direct effect", "Indirect effect"))) +
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Direct effect", "Indirect effect"),
                    values = c("#61D04F", "blue")) +
  coord_flip() +
  theme_void() + 
  theme(legend.position = "none")

# Scatter plot with side histograms
together_single <- 
  dens_single1 + 
  plot_spacer() + 
  plot_single + 
  dens_single2 + 
  plot_layout(ncol = 2, 
              nrow = 2, 
              widths = c(4, 1), 
              heights = c(1, 4)) 


## Split nodes with more studies
# Scatter plot 
plot_more <-
  ggplot(subset(data_set, single_study == "No"),
         aes(x = kld_value,
             y = kld_diff_DvsI,
             colour = ifelse(kld_diff_DvsI > 0, "Direct effect", "Indirect effect"))) +
  geom_point(size = 1) +
  geom_abline(intercept = 0,
              slope = 0) +
  geom_rug() +
  scale_color_manual(breaks = c("Direct effect", "Indirect effect"),
                     values = c("#61D04F", "blue")) +
  labs(x = expression(bold(paste("Interpretation index ", D^j))),
       y = " ",
       colour = "Larger KLD when approximating") + 
  ggtitle(paste0("Split nodes with more studies", " (n = ", dim(complete_res_more)[1], ")")) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

# Density plot for x-axis
dens_more1 <- 
  ggplot(subset(data_set, single_study == "No"),
         aes(x = kld_value,
             fill = ifelse(kld_diff_DvsI > 0, "Direct effect", "Indirect effect"))) +
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Direct effect", "Indirect effect"),
                    values = c("#61D04F", "blue")) +
  theme_void() + 
  theme(legend.position = "none")

# Density plot for y-axis
dens_more2 <- 
  ggplot(subset(data_set, single_study == "No"),
         aes(x = kld_diff_DvsI,
             fill = ifelse(kld_diff_DvsI > 0, "Direct effect", "Indirect effect"))) +
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Direct effect", "Indirect effect"),
                    values = c("#61D04F", "blue")) +
  coord_flip() +
  theme_void() + 
  theme(legend.position = "none")

# Scatter plot with side histograms
together_more <- 
  dens_more1 + 
  plot_spacer() + 
  plot_more + 
  dens_more2 + 
  plot_layout(ncol = 2, 
              nrow = 2, 
              widths = c(4, 1), 
              heights = c(1, 4)) 


## Bring together and save Figure 4
tiff("./Figures/Figure 4.tiff", 
     height = 18, 
     width = 38, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(together_single, together_more, 
          labels = c("a)", "b)"),
          ncol = 2,
          legend = "none")
dev.off()
