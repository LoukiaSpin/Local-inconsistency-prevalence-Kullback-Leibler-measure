## Create a Bland-Altman (or hybrid) plot


bland_altman_like_plot <- function(difference, 
                                   average, 
                                   single_study,
                                   vjust = -0.35,
                                   hjust = 0.5,
                                   breaks = NULL,
                                   x_title,
                                   y_title) {
  
  
  library("patchwork")
  library("reshape2")
  
  ## Calculate the necessary measures
  # Average bias per group
  bias = aggregate(difference, by = list(single_study), mean)
  
  # Standard deviation of differences per group
  sd_diff = aggregate(difference, by = list(single_study), sd)
  
  # Found number of single-study split nodes
  get_count <- table(single_study)
  
  
  ## Prepare dataset for Bland-Altman scatter plot
  data_bland_altman <- data.frame(bias = bias,
                                  sd_diff = sd_diff[, 2],
                                  lower_lim = bias[, 2] - 1.96 * sd_diff[, 2],
                                  upper_lim = bias[, 2] + 1.96 * sd_diff[, 2])
  colnames(data_bland_altman)[1:2] <- c("single_study", "bias")
  
  
  ## Bland-Altman scatter plot
  plot <- 
    ggplot(data.frame(average, difference, single_study), 
           aes(x = average,
               y = difference,
               colour = single_study)) + 
    geom_point(size = 2) + 
    geom_hline(yintercept = 0, 
               linetype = 2) + 
    geom_hline(data = data_bland_altman,
               aes(yintercept = bias,
                   colour = single_study), 
               linetype = 1) + 
    geom_hline(data = data_bland_altman,
               aes(yintercept = lower_lim,
                   colour = single_study), 
               linetype = 1) + 
    geom_hline(data = data_bland_altman,
               aes(yintercept = upper_lim,
                   colour = single_study), 
               linetype = 1) + 
    geom_text(data = subset(melt(data_bland_altman), variable != "sd_diff"),
              aes(x = 0,
                  y = value,
                  label = sprintf("%.2f", value)),
              size = 2.5,
              vjust = vjust,
              hjust = hjust,
              inherit.aes = FALSE) +
    labs(x = x_title, 
         y = y_title,
         colour = "Single-study split node") +
    scale_color_manual(breaks = c("Yes", "No"),
                       labels = c(paste0("Yes (", get_count[1], ")"), paste0("No (", get_count[2], ")")), 
                       values = c("#DF536B", "#61D04F")) +
    scale_x_continuous(expand = c(0.01, 0.0)) +
    scale_y_continuous(breaks = breaks,
                       labels = sprintf("%.2f", breaks)) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 14),
          legend.position = "bottom")
  
  # Density plot of x-axis
  dens1 <- 
    ggplot(data.frame(average, difference, single_study), 
           aes(x = average,
               fill = single_study)) + 
    geom_density(alpha = 0.4) + 
    scale_fill_manual(breaks = c("Yes", "No"),
                      values = c("#DF536B", "#61D04F")) +
    theme_void() + 
    theme(legend.position = "none")
  
  # Density plot of y-axis
  dens2 <- 
    ggplot(data.frame(average, difference, single_study), 
           aes(x = difference,
               fill = single_study)) + 
    geom_density(alpha = 0.4) + 
    scale_fill_manual(breaks = c("Yes", "No"),
                      values = c("#DF536B", "#61D04F")) +
    theme_void() + 
    coord_flip() +
    theme(legend.position = "none")
  
  # Scatter plot with side histograms
  together <- 
    dens1 + 
    plot_spacer() + 
    plot + 
    dens2 + 
    plot_layout(ncol = 2, 
                nrow = 2, 
                widths = c(4, 1), 
                heights = c(1, 4)) 
  
  return(together)
}

