#*******************************************************************************
#*
#*                        Creating Figure 3 of Manuscript                          
#*                                                                                                                                                                   
#* Date: November 2024
#*******************************************************************************


## Load library ----
list.of.packages <- c("plyr", "ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load functions ----
source("./R/Functions/complete analysis results_function.R")
source("./R/Functions/function.collection_function.R")


## Complete analysis results ----
complete_res <- complete_analysis_results()


## Prepare dataset for scatter plot
# Rename the levels of 'node_kld'
complete_res$node_conclusion <- 
  revalue(complete_res$node_conclusion, c("Consistency" = "Acceptably low", "Inconsistency" = "Material"))

# Get the parameters of parabola ('diff_mean' against 'kld_value') for *single-study* split nodes
parabola_res_single <- solve_quadratic_equ(x = subset(complete_res, Freq == 1)$diff_mean,
                                           y = subset(complete_res, Freq == 1)$kld_value,
                                           weight = NULL)

# Get the parameters of parabola ('diff_mean' against 'kld_value') for *non-single-study* split nodes
parabola_res_nonsingle <- solve_quadratic_equ(x = subset(complete_res, Freq > 1)$diff_mean,
                                              y = subset(complete_res, Freq > 1)$kld_value,
                                              weight = NULL)

# Get normalised diff values to use as weights
complete_res$norm_diff <- (abs(complete_res$diff_mean) - min(abs(complete_res$diff_mean))) / diff(range(abs(complete_res$diff_mean)))


## Split dataset by size of split nodes
# Split nodes with one study
complete_res_single <- subset(complete_res, single_study == "Yes")

# Split nodes with many studies
complete_res_more <- subset(complete_res, single_study == "No")


## Get scatter plot and save it
# Posterior *mean* of inconsistency against average KLD for *single-study* split nodes
kld_diff_single <-
  ggplot(complete_res_single,
         aes(x = diff_mean,
             y = kld_value)) +
  stat_smooth(method = "lm", 
              formula = "y~poly(x,2)", 
              se = FALSE,
              colour = "grey70") + 
  geom_hline(yintercept = 0.64,
             linetype = 2) +
  geom_vline(xintercept = 0,
             linetype = 2) +
  geom_point(aes(colour = node_standard_5,
                 fill = node_conclusion,
                 size = 1 / (diff_sd * diff_sd)), # precision
             stroke = 2,
             shape = 21) +  
  annotate(geom = "text",
           x = -3,
           y = 18,
           label = paste("y == ", sprintf("%.2f", coef(parabola_res_single)[1, 1]), " + ", sprintf("%.2f", coef(parabola_res_single)[2, 1]), "*x^2"),
           hjust = 0,
           vjust = 0,
           size = 5.5,
           col = "grey45",
           parse = TRUE) +
  labs(x = "Posterior mean of inconsistency",
       y =  expression(bold(paste("Interpretation index ", D^j))),
       fill = "Inconsistency based on index",
       colour = "Inconsistency based on p-value") + 
  scale_fill_manual(breaks = c("Acceptably low", "Material"),
                    values = c("#61D04F", "#DF536B")) +
  scale_colour_manual(breaks = c("Conclusive", "Inconclusive"),
                      values = c("#DF536B", "blue")) +
  scale_y_continuous(breaks = seq(0, 25, 5),
                     limits = c(0, 25),
                     expand = c(0.02, 0.0)) +
  guides(fill = guide_legend(override.aes = list(size = 4, shape = 21), order = 1),
         colour = guide_legend(override.aes = list(size = 4, shape = 21), order = 2),
         size = "none") + 
  ggtitle(paste0("Split nodes with one study", " (n = ", dim(complete_res_single)[1], ")")) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom")

# Posterior *mean* of inconsistency factor against average KLD for *non-single-study* split nodes
kld_diff_nonsingle <-
  ggplot(complete_res_more,
         aes(x = diff_mean,
             y = kld_value)) +
  stat_smooth(method = "lm", 
              formula = "y~poly(x,2)", 
              se = FALSE,
              colour = "grey70") + 
  geom_hline(yintercept = 0.64,
             linetype = 2) +
  geom_vline(xintercept = 0,
             linetype = 2) +
  geom_point(aes(colour = node_standard_5,
                 fill = node_conclusion,
                 size = 1 / (diff_sd * diff_sd)), # precision
             stroke = 2,
             shape = 21) +  
  annotate(geom = "text",
           x = -2.3,
           y = 20,
           label = paste("y == ", sprintf("%.2f", coef(parabola_res_nonsingle)[1, 1]), " + ", sprintf("%.2f", coef(parabola_res_nonsingle)[2, 1]), "*x^2"),
           hjust = 0,
           vjust = 0,
           size = 5.5,
           col = "grey45",
           parse = TRUE) +
  labs(x = "Posterior mean of inconsistency",
       y = "",
       fill = "Inconsistency based on index",
       colour = "Inconsistency based on p-value") + 
  scale_fill_manual(breaks = c("Acceptably low", "Material"),
                    values = c("#61D04F", "#DF536B")) +
  scale_colour_manual(breaks = c("Conclusive", "Inconclusive"),
                      values = c("#DF536B", "blue")) +
  scale_y_continuous(breaks = seq(0, 50, 10),
                     limits = c(0, 50),
                     expand = c(0.02, 0.0)) +
  guides(fill = guide_legend(override.aes = list(size = 4, shape = 21), order = 1),
         colour = guide_legend(override.aes = list(size = 4, shape = 21), order = 2),
         size = "none") + 
  ggtitle(paste0("Split nodes with more studies", " (n = ", dim(complete_res_more)[1], ")")) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom")

# Posterior *SD* of inconsistency against average KLD for *single-study* split nodes
kld_diff_sd_single <-
  ggplot(complete_res_single,
         aes(x = diff_sd,
             y = kld_value)) +
  geom_hline(yintercept = 0.64,
             linetype = 2) +
  geom_point(aes(colour = node_standard_5,
                 fill = node_conclusion,
                 size = norm_diff), 
             shape = 21,
             stroke = 2) +  
  labs(x = "Posterior standard deviation of inconsistency",
       y =  expression(bold(paste("Interpretation index ", D^j))),
       fill = "Using the proposed index",
       colour = "Using the standard approach") + 
  scale_fill_manual(breaks = c("Acceptably low", "Material"),
                    values = c("#61D04F", "#DF536B")) +
  scale_colour_manual(breaks = c("Conclusive", "Inconclusive"),
                      values = c("#DF536B", "blue")) +
  scale_y_continuous(breaks = seq(0, 25, 5),
                     limits = c(0, 25),
                     expand = c(0.02, 0.0)) +
  guides(fill = guide_legend(override.aes = list(size = 4, shape = 21), order = 1),
         colour = guide_legend(override.aes = list(size = 4, shape = 21), order = 2),
         size = "none") + 
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom")

# Posterior *SD* of inconsistency against average KLD for *non-single-study* split nodes
kld_diff_sd_nonsingle <-
  ggplot(complete_res_more,
         aes(x = diff_sd,
             y = kld_value)) +
  geom_hline(yintercept = 0.64,
             linetype = 2) +
  geom_point(aes(colour = node_standard_5,
                 fill = node_conclusion,
                 size = norm_diff), 
             shape = 21,
             stroke = 2) +  
  labs(x = "Posterior standard deviation of inconsistency",
       y = "",
       fill = "Using the proposed index",
       colour = "Using the standard approach") + 
  scale_fill_manual(breaks = c("Acceptably low", "Material"),
                    values = c("#61D04F", "#DF536B")) +
  scale_colour_manual(breaks = c("Conclusive", "Inconclusive"),
                      values = c("#DF536B", "blue")) +
  scale_y_continuous(breaks = seq(0, 50, 10),
                     limits = c(0, 50),
                     expand = c(0.02, 0.0)) +
  guides(fill = guide_legend(override.aes = list(size = 4, shape = 21), order = 1),
         colour = guide_legend(override.aes = list(size = 4, shape = 21), order = 2),
         size = "none") + 
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom")

# Bring together and save Figure 3
tiff("./Figures/Figure 3.tiff", 
     height = 25, 
     width = 45, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(kld_diff_single, kld_diff_nonsingle, kld_diff_sd_single, kld_diff_sd_nonsingle,
          labels = c("A)", "B)", "C)", "D)"),
          legend = "bottom",
          common.legend = TRUE)
dev.off()


## Summary of average KLD: D < 0.64
# Split nodes with more studies
summary(complete_res_more$diff_mean[complete_res_more$kld_value < 0.64])

# Split nodes with one study
summary(complete_res_single$diff_mean[complete_res_single$kld_value < 0.64])

