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

## figure number
# fig <- 'figure 4'
# fig <- 'figure S2'
fig <- 'figure 4'

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

##### contour plot #############################################################

# add facet labels as 'label' in DF
DF$label <- rep(c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)'), 
                each = nrow(output))

fig4 <- DF %>% 
  filter(Sample_Size == '32') %>%
  filter(Paternal_Contribution_Mode == 'Random') %>%
  ggplot(aes(x = PropClutches, 
             y = OSR, 
             z = Proportion)) +
  geom_contour_filled(bins = 5) +
  xlab('Proportion of clutches sampled') +
  ylab('Operational sex ratio') +
  labs(fill = 'Proportion \n correct \n') +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 20), 
        axis.text = element_text(size = 15)) +
  geom_text(aes(x = 0.125, 
                y = 0.45, 
                label = label, 
                group = label), 
            size = 7, colour = 'white',
            inherit.aes = FALSE) +
  facet_grid(rows = vars(M_title), cols = vars(F_title)) +
  theme(panel.spacing.x = unit(1.5, "lines")) +
  theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
  theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm"))

fig4

# save contour plot
ggsave(fig2,
       file = paste('C://Users/Vic//Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/', 
                    fig, '.pdf', sep = ''), 
       height = 6, width = 12)

