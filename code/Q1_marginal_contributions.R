# new figure to make overlapping barplot for marginal contributions by paternal 
# contribution mode and number of total fathers

# load data
load("~/Projects/multiple_paternity_power_analyses/output/proportion_correct_all_1e+05.Rdata")

# load libraries
library(ggplot2)
library(tidyverse)
library(viridisLite)

# paternal contribution modes
PCMs <- c('random', 'exponential', 'dominant50', 'dominant70', 'dominant90')
PCM_labels <- c('Random', 'Exponential', 'Dominant 50', 'Dominant 70', 'Dominant 90')

marginal <- proportion_correct_all %>%
  select(Paternal_Contribution_Mode, Fathers, Marginal) %>%
  filter(Paternal_Contribution_Mode != 'mixed_dominant') %>%
  distinct()

# make paternal contribution mode and fathers factor variables
marginal$Paternal_Contribution_Mode <- factor(marginal$Paternal_Contribution_Mode,
                                              levels = PCMs, 
                                              labels = PCM_labels)
marginal$Fathers <- factor(marginal$Fathers)

# colors to plot
colors <- viridisLite::viridis(max(proportion_correct_all$Fathers))

# plot the thing
new_fig3 <- ggplot(data = marginal, 
                   aes(x = Paternal_Contribution_Mode, 
                       y = Marginal, 
                       fill = Fathers)) +
  geom_col(position = position_jitterdodge(dodge.width = 0.5,
                                           jitter.height = 0,
                                           jitter.width = 0,
                                           seed = 25)) +
  scale_fill_manual(values = colors) +
  xlab('Paternal contribution mode') +
  ylab('Marginal contribution') +
  theme_bw() +
  theme(text = element_text(size = 22), 
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 27),
        axis.title.x = element_text(vjust = -0.6), 
        axis.title.y = element_text(vjust = 2.5), 
        plot.margin = margin(0.5, 0, 0.5, 0.75, 'cm')) +
  labs(fill = 'Number of \n fathers')

ggsave(new_fig3, 
       file = "C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/submission 2/fig3.pdf", 
       height = 7, width = 14)
