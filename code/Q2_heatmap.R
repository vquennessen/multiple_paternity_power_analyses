# Q2 heatmaps

# set working directory
setwd("~/Projects/multiple_paternity_power_analyses")

# load libraries
library(dplyr)
library(ggplot2)
library(viridisLite)
library(patchwork)

# model parameters
# fertilization modes
fmodes <- c('random',
            # 'exponential',
            # 'dominant50',
            # 'dominant70',
            # 'mixed_dominant', 
            'dominant90')
# fmodes <- c('random')
# fmodes <- c('exponential')
# fmodes <- c('mixed_dominant')


# fertilization mode titles
fmode_titles <- c('Random', # 'Exponential', 
                  # 'Dominant 50', 'Dominant 70', 'Dominant 90',
                  # 'Mixed Dominant')
                  'Dominant 90')

# sample sizes
sample_sizes <- c(32, 96)

# number of simulations
nsims <- 1e+05

# dimensions
nS <- length(sample_sizes)

# initialize dataframe
DF <- data.frame()

# for each fertilization mode
for (f in 1:length(fmodes)) {
  
  fmode <- fmodes[f]
  fmode_title <- fmode_titles[f]
  
  for (s in 1:length(sample_sizes)) {
    
    sample_size <- sample_sizes[s]
    
    # load data
    load(paste('~/Projects/multiple_paternity_power_analyses/output/uniform_Mprob_and_Fprob/', 
               sample_size, '_nests_to_sample_', fmode, '_', nsims, '.Rdata', 
               sep = ''))
    DF1 <- output
    
    # fertilization mode and sample size vectors
    Fertilization_Mode <- factor(rep(fmode_title, nrow(DF1)))
    Sample_Size <- factor(rep(sample_size, nrow(DF1)))
    
    # add fertilization mode and sample size to output dataframe
    DF2 <- cbind(Fertilization_Mode, Sample_Size, DF1)
    
    # add new scenario to DF
    DF <- rbind(DF, DF2)
    
  }
  
}

# change 32 and 96 in DF to sample size 32 and sample size 96
DF$Sample_Label[DF$Sample_Size == 32] <- 'Sample Size 32'
DF$Sample_Label[DF$Sample_Size == 96] <- 'Sample Size 96'

##### heatmap #################################################################
fig4 <- ggplot(data = DF, aes(x = PropNests, y = OSR, fill = Proportion)) +
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
  xlab('Proportion of Nests Sampled') +
  ylab('Operational Sex Ratio') +
  labs(fill = 'Proportion \n') +
  # geom_text(aes(label = round(Proportion, 2))) +
  theme(text = element_text(size = 25)) +
  facet_grid(rows = vars(Fertilization_Mode), cols = vars(Sample_Size)) +
  theme(panel.spacing.x=unit(1.5, "lines")) +
  theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
  theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm"))



fig4

# save heatmap
ggsave(fig4,
       file = 'C://Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/figures/fig4_even_Mprobs_Fprobs.png',
       height = 6, width = 12)

##### contour plot #############################################################
fig5 <- ggplot(data = DF, aes(x = PropNests, 
                              y = OSR, 
                              z = Proportion)) +
  geom_contour_filled() +
  xlab('Proportion of clutches sampled') +
  ylab('Operational Sex Ratio') +
  labs(fill = 'Confidence') +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(text = element_text(size = 25)) +
  facet_grid(rows = vars(Fertilization_Mode), cols = vars(Sample_Label)) +
  theme(panel.spacing.x = unit(1.5, "lines")) +
  theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
  theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm"))

fig5

# save heatmap
ggsave(fig5,
       file = 'C://Users/Vic//Box Sync/Quennessen_Thesis/PhD Thesis/chapters/chapter 1/figures/fig4_even_Mprobs_Fprobs_contour.png',
       height = 6, width = 12)

