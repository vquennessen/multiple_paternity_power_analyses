# Q2 heatmaps

# set working directory
setwd("~/Projects/multiple_paternity_power_analyses")

# load libraries
library(dplyr)
library(ggplot2)
library(viridisLite)
library(patchwork)

##### model parameters #########################################################

## paternal contribution modes
pcmodes <- c('random',
             # 'exponential',
             # 'dominant50',
             # 'dominant70',
             # 'mixed_dominant', 
             'dominant90')

# paternal contribution mode titles
pcmode_titles <- c('Random', # 'Exponential', 
                   # 'Dominant 50', 'Dominant 70', 'Dominant 90',
                   # 'Mixed Dominant')
                   'Dominant 90')

## data title
# data_title <- 'no_polygyny'
# data_title <- 'uniform_Mprob_no_polygyny'
folder <- '2025_04_10_N100_1000sims'
data_titles <- c('base_F_base_M', 
                 'base_F_uniform_M', 
                 'base_F_no_M', 
                 'uniform_F_base_M', 
                 'uniform_F_uniform_M', 
                 'uniform_F_no_M')

F_titles <- c('Decreasing polyandry', 'Decreasing polyandry', 'Decreasing polyandry', 
              'Uniform polyandry', 'Uniform polyandry', 'Uniform polyandry')

M_titles <- c('Decreasing polygyny', 'Uniform polygyny', 'No polygyny', 
              'Decreasing polygyny', 'Uniform polygyny', 'No polygyny')

# sample sizes
sample_sizes <- c(32, 96)

# number of simulations
nsims <- 1e+03

# dimensions
nS <- length(sample_sizes)

################################################################################

# initialize dataframe
DF <- data.frame()

for (d in 1:length(data_titles)) {
  
  # for each fertilization mode
  for (pc in 1:length(pcmodes)) {
    
    pcmode <- pcmodes[pc]
    pcmode_title <- pcmode_titles[pc]
    
    for (s in 1:length(sample_sizes)) {
      
      sample_size <- sample_sizes[s]
      
      # load data
      load(paste('~/Projects/multiple_paternity_power_analyses/output/', 
                 folder, '/', data_titles[d], '/', data_titles[d], '_', 
                 sample_size, 'samples_', pcmode, '_', nsims, 'sims.Rdata', 
                 sep = ''))
      
      DF1 <- output
      
      # fertilization mode and sample size vectors
      DF1$Paternal_Contribution_Mode <- factor(rep(pcmode_title, nrow(DF1)))
      DF1$Sample_Size <- factor(rep(sample_size, nrow(DF1)))
      DF1$Scenario <- data_titles[d]
      DF1$F_title <- F_titles[d]
      DF1$M_title <- M_titles[d]
      
      # add new scenario to DF
      DF <- rbind(DF, DF1)
      
    }
    
  }
  
}

# change 32 and 96 in DF to sample size 32 and sample size 96
DF$Sample_Label[DF$Sample_Size == 32] <- 'Sample Size 32'
DF$Sample_Label[DF$Sample_Size == 96] <- 'Sample Size 96'
DF$M_title <- factor(DF$M_title, 
                     levels = M_titles, 
                     labels = M_titles)

################################################################################
##### contour plots ############################################################
################################################################################

# add facet labels as 'label' in DF
DF$label <- rep(c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)'), 
                each = nrow(output)*length(sample_sizes)*length(pcmodes))

##### random, sample size 32 ###################################################
fig4_random_32 <- DF %>% 
  filter(Sample_Size == '32') %>%
  filter(Paternal_Contribution_Mode == 'Random') %>%
  ggplot(aes(x = PropClutches, 
             y = OSR, 
             z = Proportion)) +
  geom_contour_filled(bins = 5) +
  xlab('Proportion of clutches sampled') +
  ylab('Operational sex ratio') +
  scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  labs(fill = 'Proportion \n correct \n') +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 20), 
        axis.text = element_text(size = 15)) +
  geom_text(aes(x = 0.15, 
                y = 0.85, 
                label = label, 
                group = label), 
            size = 7, colour = 'white',
            inherit.aes = FALSE) +
  facet_grid(rows = vars(M_title), cols = vars(F_title)) +
  theme(panel.spacing.x = unit(1.5, "lines")) +
  theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
  theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
  ggtitle('Random PCM Sample Size 32')

fig <- 'fig4_random_32'

# save contour plot
ggsave(fig4_random_32,
       file = paste('C://Users/Vic//Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/', 
                    fig, '.png', sep = ''), 
       height = 11, width = 11)

##### random, sample size 96 ###################################################

fig4_random_96 <- DF %>% 
  filter(Sample_Size == '96') %>%
  filter(Paternal_Contribution_Mode == 'Random') %>%
  ggplot(aes(x = PropClutches, 
             y = OSR, 
             z = Proportion)) +
  geom_contour_filled(bins = 5) +
  xlab('Proportion of clutches sampled') +
  ylab('Operational sex ratio') +
  scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  labs(fill = 'Proportion \n correct \n') +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 20), 
        axis.text = element_text(size = 15)) +
  geom_text(aes(x = 0.15, 
                y = 0.85, 
                label = label, 
                group = label), 
            size = 7, colour = 'white',
            inherit.aes = FALSE) +
  facet_grid(rows = vars(M_title), cols = vars(F_title)) +
  theme(panel.spacing.x = unit(1.5, "lines")) +
  theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
  theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
  ggtitle('Random PCM Sample Size 96')

fig <- 'fig4_random_96'

# save contour plot
ggsave(fig4_random_96,
       file = paste('C://Users/Vic//Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/', 
                    fig, '.png', sep = ''), 
       height = 11, width = 11)

##### random, overlapping sample sizes 32 and 96 ###############################

DF32 <- DF %>%
  filter(Paternal_Contribution_Mode == 'Random') %>%
  filter(Sample_Size == 32)

DF96 <- DF %>%
  filter(Paternal_Contribution_Mode == 'Random') %>%
  filter(Sample_Size == 96)

fig4_random_overlap <- ggplot(data = DF32, aes(x = PropClutches, 
                                              y = OSR, 
                                              z = Proportion)) +
  geom_contour_filled(bins = 5) +
  geom_contour(data = DF96, 
               aes(x = PropClutches, 
                   y = OSR, 
                   z = Proportion), 
               bins = 5, 
               lwd = 1, 
               col = 'black') +
  xlab('Proportion of clutches sampled') +
  ylab('Operational sex ratio') +
  scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  labs(fill = 'Proportion \n correct \n') +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 20), 
        axis.text = element_text(size = 15)) +
  geom_text(aes(x = 0.15, 
                y = 0.85, 
                label = label, 
                group = label), 
            size = 7, colour = 'white',
            inherit.aes = FALSE) +
  facet_grid(rows = vars(M_title), cols = vars(F_title)) +
  theme(panel.spacing.x = unit(1.5, "lines")) +
  theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
  theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
  ggtitle('Random PCM Sample Sizes 32 and 96')

fig <- 'fig4_random_overlap'

# save contour plot
ggsave(fig4_random_overlap,
       file = paste('C://Users/Vic//Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/', 
                    fig, '.png', sep = ''), 
       height = 11, width = 11)

##### dominant 90, overlapping sample sizes 32 and 96 ##########################

DF32 <- DF %>%
  filter(Paternal_Contribution_Mode == 'Dominant 90') %>%
  filter(Sample_Size == 32)

DF96 <- DF %>%
  filter(Paternal_Contribution_Mode == 'Dominant 90') %>%
  filter(Sample_Size == 96)

fig4_dominant90_overlap <- ggplot(data = DF32, aes(x = PropClutches, 
                                               y = OSR, 
                                               z = Proportion)) +
  geom_contour_filled(bins = 5) +
  geom_contour(data = DF96, 
               aes(x = PropClutches, 
                   y = OSR, 
                   z = Proportion), 
               bins = 5, 
               lwd = 1, 
               col = 'black') +
  xlab('Proportion of clutches sampled') +
  ylab('Operational sex ratio') +
  scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  labs(fill = 'Proportion \n correct \n') +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 20), 
        axis.text = element_text(size = 15)) +
  geom_text(aes(x = 0.15, 
                y = 0.85, 
                label = label, 
                group = label), 
            size = 7, colour = 'white',
            inherit.aes = FALSE) +
  facet_grid(rows = vars(M_title), cols = vars(F_title)) +
  theme(panel.spacing.x = unit(1.5, "lines")) +
  theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
  theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
  ggtitle('Dominant 90 PCM Sample Sizes 32 and 96')

fig <- 'fig4_dominant90_overlap'

# save contour plot
ggsave(fig4_dominant90_overlap,
       file = paste('C://Users/Vic//Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/', 
                    fig, '.png', sep = ''), 
       height = 11, width = 11)

##### sample size 32, overlapping PCMs random and dominant 90 ##################

DFdominant90 <- DF %>%
  filter(Sample_Size == 32) %>%
  filter(Paternal_Contribution_Mode == 'Dominant 90')

DFrandom <- DF %>%
  filter(Sample_Size == 32) %>%
  filter(Paternal_Contribution_Mode == 'Random')

fig4_samplesize32_overlap <- ggplot(data = DFrandom, aes(x = PropClutches, 
                                                   y = OSR, 
                                                   z = Proportion)) +
  geom_contour_filled(bins = 5) +
  geom_contour(data = DFdominant90, 
               aes(x = PropClutches, 
                   y = OSR, 
                   z = Proportion), 
               bins = 5, 
               lwd = 1, 
               col = 'black') +
  xlab('Proportion of clutches sampled') +
  ylab('Operational sex ratio') +
  scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  labs(fill = 'Proportion \n correct \n') +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 20), 
        axis.text = element_text(size = 15)) +
  geom_text(aes(x = 0.15, 
                y = 0.85, 
                label = label, 
                group = label), 
            size = 7, colour = 'white',
            inherit.aes = FALSE) +
  facet_grid(rows = vars(M_title), cols = vars(F_title)) +
  theme(panel.spacing.x = unit(1.5, "lines")) +
  theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
  theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
  ggtitle('Sample size 32 random and dominant 90')

fig <- 'fig4_samplesize32_overlap'

# save contour plot
ggsave(fig4_samplesize32_overlap,
       file = paste('C://Users/Vic//Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/', 
                    fig, '.png', sep = ''), 
       height = 11, width = 11)

##### why are they all the same??? #############################################

to_plot <- DF %>%
  filter(Paternal_Contribution_Mode %in% c('Random', 'Dominant 90')) %>%
  filter(Scenario == 'uniform_F_no_M')

ggplot(data = to_plot, 
       aes(x = PropClutches, 
           y = OSR, 
           z = Proportion)) +
  geom_contour_filled(bins = 5) +
  facet_grid(rows = vars(Paternal_Contribution_Mode), 
             cols = vars(Sample_Size))
