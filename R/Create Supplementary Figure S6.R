#*******************************************************************************
#*
#*                       Creating Supplementary Figure 6                                                                                                                 
#*                                                                                                                                                                   
#* Date: November 2024
#*******************************************************************************


## Load libraries ----
list.of.packages <- c("patchwork", "ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load functions ----
source("./R/Functions/complete analysis results_function.R")
source("./R/Functions/bland.altman.plot_function.R")


## Complete analysis results ----
complete_res <- complete_analysis_results()


## Restrict to split nodes with acceptably low inconsistency
complete_res_fin <- subset(complete_res, kld_value < 0.64)


## Number and % (of analysed split nodes
num_nodes <- dim(complete_res_fin)[1]
perc_nodes <- round((dim(complete_res_fin)[1] / dim(complete_res)[1]) * 100, 0)


## Prepare dataset for scatter plot
# Approximate direct by indirect
complete_res_fin$kld_dir <- kld_measure(mean_y = complete_res_fin$indirect_mean,
                                        sd_y = complete_res_fin$indirect_sd,
                                        mean_x = complete_res_fin$direct_mean,
                                        sd_x = complete_res_fin$direct_sd)$kld_x_true

# Approximate indirect by direct
complete_res_fin$kld_indir <- kld_measure(mean_y = complete_res_fin$indirect_mean,
                                          sd_y = complete_res_fin$indirect_sd,
                                          mean_x = complete_res_fin$direct_mean,
                                          sd_x = complete_res_fin$direct_sd)$kld_y_true

# Dataset for ggplot2
data_set <- data.frame(kld_value = rep(complete_res_fin$kld_value, 2),
                       kld_effect = c(complete_res_fin$kld_dir, complete_res_fin$kld_indir),
                       kld_type = rep(c("Direct effect", "Indirect effect"), each = dim(complete_res_fin)[1]),
                       single_study = rep(complete_res_fin$single_study, 2))


## Split dataset by size of split nodes
# Split nodes with one study
complete_res_fin_single <- subset(complete_res_fin, single_study == "Yes")

# Split nodes with many studies
complete_res_fin_more <- subset(complete_res_fin, single_study == "No")


## Summarise D_dir and D_ind per group
# Split nodes with one study 
summary(complete_res_fin_single$kld_dir); quantile(complete_res_fin_single$kld_dir, c(0.025, 0.975))
summary(complete_res_fin_single$kld_indir); quantile(complete_res_fin_single$kld_indir, c(0.025, 0.975))

# Split nodes with many studies
summary(complete_res_fin_more$kld_dir); quantile(complete_res_fin_more$kld_dir, c(0.025, 0.975))
summary(complete_res_fin_more$kld_indir); quantile(complete_res_fin_more$kld_indir, c(0.025, 0.975))


## Split nodes with single studies
# Scatter plot 
plot_single <-
  ggplot(subset(data_set, single_study == "Yes"),
         aes(x = kld_effect,
             y = kld_value,
             colour = kld_type)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0,
              slope = 1) +
  geom_rug() +
  scale_color_manual(breaks = c("Direct effect", "Indirect effect"),
                     values = c("#61D04F", "blue")) +
  labs(x = "Kullback-Leibler divergence for approximating",
       y = expression(bold(paste("Interpretation index ", D^j))),
       colour = " ") + 
  ggtitle(paste0("Split nodes with one study", " (n = ", dim(complete_res_fin_single)[1], ")")) +
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
         aes(x = kld_effect,
             fill = kld_type)) +
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Direct effect", "Indirect effect"),
                    values = c("#61D04F", "blue")) +
  theme_void() + 
  theme(legend.position = "none")

# Density plot for y-axis
dens_single2 <- 
  ggplot(subset(data_set, single_study == "Yes"),
         aes(x = kld_value)) +
  geom_density(alpha = 0.4,
               fill = "grey") + 
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
         aes(x = kld_effect,
             y = kld_value,
             colour = kld_type)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0,
              slope = 1) +
  geom_rug() +
  scale_color_manual(breaks = c("Direct effect", "Indirect effect"),
                     values = c("#61D04F", "blue")) +
  labs(x = "Kullback-Leibler divergence for approximating",
       y = " ",
       colour = " ") + 
  ggtitle(paste0("Split nodes with more studies", " (n = ", dim(complete_res_fin_more)[1], ")")) +
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
         aes(x = kld_effect,
             fill = kld_type)) +
  geom_density(alpha = 0.4) + 
  scale_fill_manual(breaks = c("Direct effect", "Indirect effect"),
                    values = c("#61D04F", "blue")) +
  theme_void() + 
  theme(legend.position = "none")

# Density plot for y-axis
dens_more2 <- 
  ggplot(subset(data_set, single_study == "No"),
         aes(x = kld_value)) +
  geom_density(alpha = 0.4,
               fill = "grey") + 
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


## Bring together and save Figure S6
tiff("./Figures/Figure S6.tiff", 
     height = 18, 
     width = 38, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
plots <- ggarrange(together_single, together_more, 
                   labels = c("A)", "B)"),
                   ncol = 2,
                   legend = "none")
annotate_figure(plots, 
                top = text_grob(paste0("Restricting to ", num_nodes, " (", perc_nodes, "%) split nodes with acceptably low inconsistency"), 
                                color = "black", 
                                face = "bold", 
                                size = 14))
dev.off()
