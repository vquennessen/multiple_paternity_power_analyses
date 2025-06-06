#Q1 heatmap

# set working directory
setwd('~/Projects/multiple_paternity_power_analyses/code/')

# load libraries
library(viridis)
library(tidyverse)

# load probability dataframe
load("~/Projects/multiple_paternity_power_analyses/output/proportion_correct_all_1e+05.Rdata")
# output called 'proportion_correct_all'
props <- proportion_correct_all

# undo spread from hatchlings_to_sample.R
# DF <- props %>%
#   gather(key = 'Sample_Size', value = 'Probability', 3:4)

# make factor variables
props$Fathers <- as.factor(props$Fathers)
props$Paternal_Contribution_Mode <- factor(props$Paternal_Contribution_Mode, 
                                   levels = c('mixed_dominant', 'dominant90', 'dominant70',
                                              'dominant50', 'exponential', 'random'),
                                   labels = c('Mixed Dominant', 'Dominant 90', 'Dominant 70', 
                                              'Dominant 50', 'Exponential', 'Random'))
props$Sample_Size <- as.factor(props$Sample_Size)
props$Sample_Size <- as.factor(props$Sample_Size)

# subset by sample size
DF32 <- subset(props, Sample_Size == 32)
DF96 <- subset(props, Sample_Size == 96)

# start heatmap for sample size 32
fig3 <- ggplot(data = DF32, aes(x = Fathers, 
                                   y = Paternal_Contribution_Mode, 
                                   fill = Proportion_Correct)) +
  geom_tile(color = 'white',
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = hcl.colors(n = 5)[1],
                       mid = hcl.colors(n = 5)[3],
                       high = hcl.colors(n = 5)[5],
                       midpoint = 0.5,
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       limits = c(0, 1),
                       na.value = 'gray') +
  guides(fill = guide_colourbar(title = 'Proportion \n correct \n')) +
  xlab('Number of fathers') +
  ylab('Paternal contribution mode') +
  theme(panel.background = element_blank()) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 20), 
        axis.text = element_text(size = 15)) +
  theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
  theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
  
  # random
  annotate(geom = 'text', x = 1, y = 6.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 2, y = 6.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 3, y = 6.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 4, y = 6.1, label = '0.996', colour = 'black', size = 5) +
  
  annotate("segment", x = 0.52, xend = 1.01, y = 5.75, yend = 5.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.01, xend = 1.5, y = 5.75, yend = 5.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.85, y = 5.75, yend = 5.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.85, xend = 2.5, y = 5.75, yend = 5.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.77, y = 5.75, yend = 5.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.77, xend = 3.5, y = 5.75, yend = 5.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.72, y = 5.75, yend = 5.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.72, xend = 4.5, y = 5.75, yend = 5.75, colour = "white", 
           lwd = 3) +
  
  # exponential
  annotate(geom = 'text', x = 1, y = 5.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 2, y = 5.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 3, y = 5.1, label = '0.972', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 4, y = 5.1, label = '0.748', colour = 'black', size = 5) +
  
  annotate("segment", x = 0.52, xend = 1.01, y = 4.75, yend = 4.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.01, xend = 1.5, y = 4.75, yend = 4.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.77, y = 4.75, yend = 4.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.77, xend = 2.5, y = 4.75, yend = 4.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.635, y = 4.75, yend = 4.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.635, xend = 3.5, y = 4.75, yend = 4.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.5825, y = 4.75, yend = 4.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.5825, xend = 4.5, y = 4.75, yend = 4.75, colour = "white", 
           lwd = 3) +
  
  # dominant 50%
  annotate(geom = 'text', x = 1, y = 4.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 2, y = 4.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 3, y = 4.1, label = '0.991', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 4, y = 4.1, label = '0.945', colour = 'black', size = 5) +
  
  annotate("segment", x = 0.52, xend = 1.01, y = 3.75, yend = 3.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.01, xend = 1.5, y = 3.75, yend = 3.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.77, y = 3.75, yend = 3.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.77, xend = 2.5, y = 3.75, yend = 3.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.69, y = 3.75, yend = 3.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.69, xend = 3.5, y = 3.75, yend = 3.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.645, y = 3.75, yend = 3.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.645, xend = 4.5, y = 3.75, yend = 3.75, colour = "white", 
           lwd = 3) +
  
  # dominant 70
  annotate(geom = 'text', x = 1, y = 3.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 2, y = 3.1, label = '0.989', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 3, y = 3.1, label = '0.900', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 4, y = 3.1, label = '0.702', colour = 'black', size = 5) +
  
  annotate("segment", x = 0.52, xend = 0.82, y = 2.75, yend = 2.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 0.82, xend = 1.5, y = 2.75, yend = 2.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.67, y = 2.75, yend = 2.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.67, xend = 2.5, y = 2.75, yend = 2.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.62, y = 2.75, yend = 2.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.62, xend = 3.5, y = 2.75, yend = 2.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.595, y = 2.75, yend = 2.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.595, xend = 4.5, y = 2.75, yend = 2.75, colour = "white", 
           lwd = 3) +
  
  # dominant 90
  annotate(geom = 'text', x = 1, y = 2.1, label = '0.965', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 2, y = 2.1, label = '0.647', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 3, y = 2.1, label = '0.282', colour = 'white', size = 5) +
  annotate(geom = 'text', x = 4, y = 2.1, label = '0.088', colour = 'white', size = 5) +
  
  annotate("segment", x = 0.52, xend = 0.62, y = 1.75, yend = 1.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 0.62, xend = 1.5, y = 1.75, yend = 1.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.57, y = 1.75, yend = 1.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.57, xend = 2.5, y = 1.75, yend = 1.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.553, y = 1.75, yend = 1.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.553, xend = 3.5, y = 1.75, yend = 1.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.545, y = 1.75, yend = 1.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.545, xend = 4.5, y = 1.75, yend = 1.75, colour = "white", 
           lwd = 3) +
  
  # mixed dominant
  annotate(geom = 'text', x = 1, y = 1.1, label = '0.988', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 2, y = 1.1, label = '0.878', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 3, y = 1.1, label = '0.723', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 4, y = 1.1, label = '0.578', colour = 'black', size = 5) + 
  
  annotate("segment", x = 0.52, xend = 0.62, y = 0.75, yend = 0.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 0.62, xend = 0.82, y = 0.75, yend = 0.75, colour = "darkgrey", 
           lwd = 3) +
  annotate("segment", x = 0.82, xend = 1.02, y = 0.75, yend = 0.75, colour = "lightgrey", 
           lwd = 3) +
  annotate("segment", x = 1.02, xend = 1.5, y = 0.75, yend = 0.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.57, y = 0.75, yend = 0.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.57, xend = 1.67, y = 0.75, yend = 0.75, colour = "darkgrey", 
           lwd = 3) +
  annotate("segment", x = 1.67, xend = 1.77, y = 0.75, yend = 0.75, colour = "lightgrey", 
           lwd = 3) +
  annotate("segment", x = 1.77, xend = 2.5, y = 0.75, yend = 0.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.553, y = 0.75, yend = 0.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.553, xend = 2.62, y = 0.75, yend = 0.75, colour = "darkgrey", 
           lwd = 3) +
  annotate("segment", x = 2.62, xend = 2.687, y = 0.75, yend = 0.75, colour = "lightgrey", 
           lwd = 3) +
  annotate("segment", x = 2.687, xend = 3.5, y = 0.75, yend = 0.75, colour = "white", 
           lwd = 3) + 
  annotate("segment", x = 3.52, xend = 3.545, y = 0.75, yend = 0.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.545, xend = 3.595, y = 0.75, yend = 0.75, colour = "darkgrey", 
           lwd = 3) +
  annotate("segment", x = 3.595, xend = 3.645, y = 0.75, yend = 0.75, colour = "lightgrey", 
           lwd = 3) +
  annotate("segment", x = 3.645, xend = 4.5, y = 0.75, yend = 0.75, colour = "white", 
           lwd = 3) 

ggsave(fig3, 
     file = "C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/figures/figure 3.pdf", 
     height = 6, width = 12)

# start heatmap for sample size 96
figS1 <- ggplot(data = DF96, aes(x = Fathers, 
                                   y = Paternal_Contribution_Mode, 
                                   fill = Proportion_Correct)) +
  geom_tile(color = 'white',
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = hcl.colors(n = 5)[1],
                       mid = hcl.colors(n = 5)[3],
                       high = hcl.colors(n = 5)[5],
                       midpoint = 0.5,
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       limits = c(0, 1),
                       na.value = 'gray') +
  guides(fill = guide_colourbar(title = 'Proportion \n correct \n')) +
  xlab('Number of fathers') +
  ylab('Paternal contribution mode') +
  theme(panel.background = element_blank()) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 20), 
        axis.text = element_text(size = 15)) +
  theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
  theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
  # ggtitle('Proportion correct with sample size 96 \n and marginal contributions') +
  
  # random
  annotate(geom = 'text', x = 1, y = 6.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 2, y = 6.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 3, y = 6.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 4, y = 6.1, label = '1.000', colour = 'black', size = 5) +
  
  annotate("segment", x = 0.52, xend = 1.01, y = 5.75, yend = 5.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.01, xend = 1.5, y = 5.75, yend = 5.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.85, y = 5.75, yend = 5.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.85, xend = 2.5, y = 5.75, yend = 5.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.77, y = 5.75, yend = 5.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.77, xend = 3.5, y = 5.75, yend = 5.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.72, y = 5.75, yend = 5.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.72, xend = 4.5, y = 5.75, yend = 5.75, colour = "white", 
           lwd = 3) +
  
  # exponential
  annotate(geom = 'text', x = 1, y = 5.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 2, y = 5.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 3, y = 5.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 4, y = 5.1, label = '0.989', colour = 'black', size = 5) +
  
  annotate("segment", x = 0.52, xend = 1.01, y = 4.75, yend = 4.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.01, xend = 1.5, y = 4.75, yend = 4.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.77, y = 4.75, yend = 4.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.77, xend = 2.5, y = 4.75, yend = 4.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.635, y = 4.75, yend = 4.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.635, xend = 3.5, y = 4.75, yend = 4.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.5825, y = 4.75, yend = 4.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.5825, xend = 4.5, y = 4.75, yend = 4.75, colour = "white", 
           lwd = 3) +
  
  # dominant 50%
  annotate(geom = 'text', x = 1, y = 4.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 2, y = 4.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 3, y = 4.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 4, y = 4.1, label = '1.000', colour = 'black', size = 5) +
  
  annotate("segment", x = 0.52, xend = 1.01, y = 3.75, yend = 3.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.01, xend = 1.5, y = 3.75, yend = 3.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.77, y = 3.75, yend = 3.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.77, xend = 2.5, y = 3.75, yend = 3.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.69, y = 3.75, yend = 3.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.69, xend = 3.5, y = 3.75, yend = 3.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.645, y = 3.75, yend = 3.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.645, xend = 4.5, y = 3.75, yend = 3.75, colour = "white", 
           lwd = 3) +
  
  # dominant 70%
  annotate(geom = 'text', x = 1, y = 3.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 2, y = 3.1, label = '1.000', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 3, y = 3.1, label = '0.999', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 4, y = 3.1, label = '0.992', colour = 'black', size = 5) +
  
  annotate("segment", x = 0.52, xend = 0.82, y = 2.75, yend = 2.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 0.82, xend = 1.5, y = 2.75, yend = 2.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.67, y = 2.75, yend = 2.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.67, xend = 2.5, y = 2.75, yend = 2.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.62, y = 2.75, yend = 2.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.62, xend = 3.5, y = 2.75, yend = 2.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.595, y = 2.75, yend = 2.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.595, xend = 4.5, y = 2.75, yend = 2.75, colour = "white", 
           lwd = 3) +
  
  # dominant 90
  annotate(geom = 'text', x = 1, y = 2.1, label = '0.999', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 2, y = 2.1, label = '0.973', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 3, y = 2.1, label = '0.848', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 4, y = 2.1, label = '0.630', colour = 'black', size = 5) +
  
  annotate("segment", x = 0.52, xend = 0.62, y = 1.75, yend = 1.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 0.62, xend = 1.5, y = 1.75, yend = 1.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.57, y = 1.75, yend = 1.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.57, xend = 2.5, y = 1.75, yend = 1.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.553, y = 1.75, yend = 1.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.553, xend = 3.5, y = 1.75, yend = 1.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 3.52, xend = 3.545, y = 1.75, yend = 1.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.545, xend = 4.5, y = 1.75, yend = 1.75, colour = "white", 
           lwd = 3) +
  
  # mixed dominant
  annotate(geom = 'text', x = 1, y = 1.1, label = '0.999', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 2, y = 1.1, label = '0.991', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 3, y = 1.1, label = '0.949', colour = 'black', size = 5) +
  annotate(geom = 'text', x = 4, y = 1.1, label = '0.873', colour = 'black', size = 5) + 
  
  annotate("segment", x = 0.52, xend = 0.62, y = 0.75, yend = 0.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 0.62, xend = 0.82, y = 0.75, yend = 0.75, colour = "darkgrey", 
           lwd = 3) +
  annotate("segment", x = 0.82, xend = 1.02, y = 0.75, yend = 0.75, colour = "lightgrey", 
           lwd = 3) +
  annotate("segment", x = 1.02, xend = 1.5, y = 0.75, yend = 0.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 1.52, xend = 1.57, y = 0.75, yend = 0.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 1.57, xend = 1.67, y = 0.75, yend = 0.75, colour = "darkgrey", 
           lwd = 3) +
  annotate("segment", x = 1.67, xend = 1.77, y = 0.75, yend = 0.75, colour = "lightgrey", 
           lwd = 3) +
  annotate("segment", x = 1.77, xend = 2.5, y = 0.75, yend = 0.75, colour = "white", 
           lwd = 3) +
  annotate("segment", x = 2.52, xend = 2.553, y = 0.75, yend = 0.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 2.553, xend = 2.62, y = 0.75, yend = 0.75, colour = "darkgrey", 
           lwd = 3) +
  annotate("segment", x = 2.62, xend = 2.687, y = 0.75, yend = 0.75, colour = "lightgrey", 
           lwd = 3) +
  annotate("segment", x = 2.687, xend = 3.5, y = 0.75, yend = 0.75, colour = "white", 
           lwd = 3) + 
  annotate("segment", x = 3.52, xend = 3.545, y = 0.75, yend = 0.75, colour = "black", 
           lwd = 3) +
  annotate("segment", x = 3.545, xend = 3.595, y = 0.75, yend = 0.75, colour = "darkgrey", 
           lwd = 3) +
  annotate("segment", x = 3.595, xend = 3.645, y = 0.75, yend = 0.75, colour = "lightgrey", 
           lwd = 3) +
  annotate("segment", x = 3.645, xend = 4.5, y = 0.75, yend = 0.75, colour = "white", 
           lwd = 3) 

ggsave(figS1, 
       file = "C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/figures/figure S1.pdf", 
       height = 6, width = 12)

