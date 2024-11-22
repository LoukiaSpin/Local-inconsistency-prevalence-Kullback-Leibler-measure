#*******************************************************************************
#*
#*                       Creating Figure 2 of Manuscript                    
#*                                                                                                                                                                   
#* Date: November 2024
#*******************************************************************************


## Load libraries ----
list.of.packages <- c("dplyr", "plyr", "ggplot2", "ggpubr") 
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load functions ----
source("./30_Analysis/Functions/complete analysis results_function.R")


## Complete analysis results ----
complete_res <- complete_analysis_results()


## Stacked bar plots on (in)consistency conclusion from testing versus proposed index for *split nodes*
# Prepare dataset for *split nodes*
data_barplot_node <- data.frame(node_kld = rep(complete_res$node_conclusion, 2),
                                node_standard = c(complete_res$node_standard_5, complete_res$node_standard_10),
                                alpha = rep(c("5%", "10%"), each = dim(complete_res)[1]))

# Rename the levels of 'node_kld'
data_barplot_node$node_kld <- 
  revalue(data_barplot_node$node_kld, c("Consistency" = "Acceptably low", "Inconsistency" = "Material"))

# Calculate % conditionally on p-value decision for *split nodes*
conclusion_node <- data_barplot_node %>%
  dplyr::group_by(node_standard, node_kld, alpha) %>%
  dplyr::count() %>%
  dplyr::group_by(node_standard, alpha) %>%
  dplyr::mutate(perc = n / sum(n))


## Split nodes with *single* studies
# Create stacked barplot for *split nodes* (5% significance level) 
barplot_node_5 <-
  ggplot(subset(conclusion_node, alpha == "5%"),
         aes(x = node_standard,
             y = perc,
             fill = node_kld)) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(x = node_standard,
                y = perc,
                group = node_kld,
                label = ifelse(perc != 0, paste0(round(perc * 100, 0), "% (", n,")"), " ")),
            hjust = 0.5,
            vjust = 1.0,
            size = 4.5,
            position = "stack",
            colour = "white") +
  labs(x = " ",
       y = "Percentage split nodes (%)",
       fill = "Inconsistency based on index") +
  ggtitle("Significance level at 0.05") +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  scale_y_continuous(labels = scales::label_percent(suffix = " ")) +
  theme_classic() +
  theme(title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))

# Create stacked barplot for *split nodes* (10% significance level)
barplot_node_10 <-
  ggplot(subset(conclusion_node, alpha == "10%"),
         aes(x = node_standard,
             y = perc,
             fill = node_kld)) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(x = node_standard,
                y = perc,
                group = node_kld,
                label = ifelse(perc != 0, paste0(round(perc * 100, 0), "% (", n,")"), " ")),
            hjust = 0.5,
            vjust = 1.0,
            size = 4.5,
            position = "stack",
            colour = "white") +
  labs(x = " ",
       y = " ",
       fill = "Inconsistency based on index") +
  ggtitle("Significance level at 0.10") +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  scale_y_continuous(labels = scales::label_percent(suffix = " ")) +
  theme_classic() +
  theme(title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))


## Stacked bar plots on (in)consistency conclusions from testing versus index for *networks*
# Prepare dataset for *networks*
data_barplot_net0 <- data.frame(network = rep(complete_res$network_id, 2),
                                net_kld = rep(complete_res$net_conclusion, 2),
                                net_standard = c(complete_res$net_standard_5, complete_res$net_standard_10),
                                alpha = rep(c("5%", "10%"), each = dim(complete_res)[1]))

# Rename the levels of 'net_kld'
data_barplot_net0$net_kld <- 
  revalue(data_barplot_net0$net_kld, c("Consistency" = "Acceptably low", "Inconsistency" = "Material"))

# Keep one value per network
data_barplot_net <- data_barplot_net0 %>% 
  distinct(network, alpha, .keep_all = TRUE)

# Calculate % conditionally on p-value decision for *networks*
conclusion_net <- data_barplot_net %>%
  dplyr::group_by(net_standard, net_kld, alpha) %>%
  dplyr::count() %>%
  dplyr::group_by(net_standard, alpha) %>%
  dplyr::mutate(perc = n / sum(n))

# Create stacked barplot for *networks* (5% significance level) 
barplot_net_5 <-
  ggplot(subset(conclusion_net, alpha == "5%"),
         aes(x = net_standard,
             y = perc,
             fill = net_kld)) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(x = net_standard,
                y = perc,
                group = net_kld,
                label = ifelse(perc != 0, paste0(round(perc * 100, 0), "% (", n,")"), " ")),
            hjust = 0.5,
            vjust = 1.0,
            size = 4.5,
            position = "stack",
            colour = "white") +
  labs(x = "Inconsistency based on p-value",
       y = "Percentage networks (%)",
       fill = "Inconsistency based on index") +
  #ggtitle("Significance level at 0.05") +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  scale_y_continuous(labels = scales::label_percent(suffix = " ")) +
  theme_classic() +
  theme(title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))

# Create stacked barplot for *networks* (10% significance level) 
barplot_net_10 <-
  ggplot(subset(conclusion_net, alpha == "10%"),
         aes(x = net_standard,
             y = perc,
             fill = net_kld)) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(x = net_standard,
                y = perc,
                group = net_kld,
                label = ifelse(perc != 0, paste0(round(perc * 100, 0), "% (", n,")"), " ")),
            hjust = 0.5,
            vjust = 1.0,
            size = 4.5,
            position = "stack",
            colour = "white") +
  labs(x = "Inconsistency based on p-value",
       y = " ",
       fill = "Inconsistency based on index") +
  #ggtitle("Significance level at 0.10") +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  scale_y_continuous(labels = scales::label_percent(suffix = " ")) +
  theme_classic() +
  theme(title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))

# Bring together and save Figure 2
tiff("./30_Analysis/Figure 2.tiff", 
     height = 22, 
     width = 38, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(barplot_node_5, barplot_node_10, barplot_net_5, barplot_net_10,
          labels = c("A)", "B)"),
          nrow = 2,
          ncol = 2,
          common.legend = TRUE,
          legend = "bottom")
dev.off()

