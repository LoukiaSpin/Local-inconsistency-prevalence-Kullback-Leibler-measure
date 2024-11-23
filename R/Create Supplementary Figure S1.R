#*******************************************************************************
#*
#*                       Creating Supplementary Figure S1                                       
#*                                                                                                                                                                   
#* Date: November 2024
#*******************************************************************************


## For the moment, please, use the development version of 'rnmamod'
#devtools::install_github("LoukiaSpin/rnmamod", force = TRUE)


## Load libraries
list.of.packages <- c("rnmamod", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Fictional dataset (log ORs)
dataset <- function (mean_dir, sd_dir, mean_ind, sd_indir) {
  
  data_set <- data.frame(comp = "B vs A", 
                         matrix(c(mean_dir, sd_dir, 
                                  mean_ind, sd_indir, 
                                  mean_dir - mean_ind, sqrt(sd_dir * sd_dir + sd_indir * sd_indir)), 
                                nrow = 1))
  colnames(data_set)[2:7] <- c("dir_mean", "dir_sd", "ind_mean", "ind_sd", "inc_mean", "inc_sd")
  
  return(data_set)
}


## Assuming *consistency* and good overlapping
p1 <- kld_inconsistency_user(dataset(mean_dir = log(1.50), sd_dir = 0.3,
                                     mean_ind = log(1.50), sd_indir = 0.35), 
                             outcome = "Odds ratio (logarithmic scale)",
                             show_incons = TRUE,
                             title_name = "Consistency with good overlapping",
                             axis_title_size = 14,
                             axis_text_size = 14,
                             strip_text_size = 14,
                             legend_title_size = 14,
                             legend_text_size = 14)$Density_plot


## Assuming *consistency* and poor overlapping 
p2 <- kld_inconsistency_user(dataset(mean_dir = log(1.50), sd_dir = 0.3,
                                     mean_ind = log(1.50), sd_indir = 0.9), 
                             outcome = "Odds ratio (logarithmic scale)",
                             show_incons = TRUE,
                             y_axis_name = FALSE,
                             title_name = "Consistency with poor overlapping",
                             axis_title_size = 14,
                             axis_text_size = 14,
                             strip_text_size = 14,
                             legend_title_size = 14,
                             legend_text_size = 14)$Density_plot


## Bring all together in a 1x2 panel
tiff("./Figures/Figure S1.tiff",
     height = 20,
     width = 40,
     units = "cm",
     compression = "lzw",
     res = 300)
ggarrange(p1, p2,
          ncol = 2, nrow = 1,
          labels = c("A)", "B)"),
          font.label = list(size = 12, color = "black", face = "bold", family = NULL),
          common.legend = TRUE,
          legend = "bottom")
dev.off()
