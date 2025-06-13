# Q2 heatmaps

# set working directory
setwd("~/Projects/multiple_paternity_power_analyses")

# load libraries
library(dplyr)
library(ggplot2)
library(viridisLite)
library(patchwork)
library(magrittr)

##### model parameters #########################################################

computer <- 'laptop'
# computer <- 'desktop'
# computer <- 'cluster'

user <- ifelse(computer == 'laptop', 'vique', 'Vic')

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
# nsims <- 1000
nsims <- c(10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000)

sample_sizes <- c(32, 96)

scenarios <- c('2025_04_22_N100_10000sims', 
               '2025_04_23_N200_10000sims',
               '2025_04_27_N500_10000sims',
               '2025_05_27_N1000_10000sims',
               '2025_05_05_N100_10000sims_minID0.9',
               '2025_05_06_N200_10000sims_minID0.9',
               '2025_05_09_N500_10000sims_minID0.9', 
               '2025_05_25_N1000_10000sims_minID0.9')

# minimum IDs
minIDs <- c(1.0, 1.0, 1.0, 1.0, 0.9, 0.9, 0.9, 0.9)

# folders <- paste(scenarios, '_', nsims, 'sims', sep = '')
folders <- scenarios

mating_systems <- c('base_F_base_M', 
                    'base_F_uniform_M', 
                    'base_F_no_M', 
                    'uniform_F_base_M', 
                    'uniform_F_uniform_M', 
                    'uniform_F_no_M')

F_titles <- c('Decreasing polyandry', 'Decreasing polyandry', 'Decreasing polyandry', 
              'Uniform polyandry', 'Uniform polyandry', 'Uniform polyandry')

M_titles <- c('Decreasing polygyny', 'Uniform polygyny', 'No polygyny', 
              'Decreasing polygyny', 'Uniform polygyny', 'No polygyny')

################################################################################

# initialize dataframe
DF <- data.frame()

for (f in 1:length(folders)) {
  
  for (ms in 1:length(mating_systems)) {
    
    # for each fertilization mode
    for (pc in 1:length(pcmodes)) {
      
      pcmode <- pcmodes[pc]
      pcmode_title <- pcmode_titles[pc]
      
      for (s in 1:length(sample_sizes)) {
        
        sample_size <- sample_sizes[s]
        
        # load data
        load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/', 
                   'model output/multiple paternity power analyses/', 
                   folders[f], '/', mating_systems[ms], '/', mating_systems[ms], 
                   '_', sample_size, 'samples_', pcmode, '_', nsims[f], 
                   'sims.Rdata', sep = ''))
        
        DF1 <- output
        
        # fertilization mode and sample size vectors
        DF1$Paternal_Contribution_Mode <- pcmode_titles[pc]
        DF1$Sample_Size <- sample_sizes[s]
        DF1$Mating_system <- mating_systems[ms]
        DF1$F_title <- F_titles[ms]
        DF1$M_title <- M_titles[ms]
        DF1$Pop_size <- readr::parse_number(substr(scenarios[f], 13, 16))
        DF1$Scenario <- scenarios[f]
        DF1$minID <- minIDs[f]
        
        # add new scenario to DF
        DF <- rbind(DF, DF1)
        
      }
      
    }
    
  }
  
}

# change 32 and 96 in DF to sample size 32 and sample size 96
DF$Sample_Label[DF$Sample_Size == 32] <- 'Sample Size 32'
DF$Sample_Label[DF$Sample_Size == 96] <- 'Sample Size 96'
DF$M_title <- factor(DF$M_title, 
                     levels = M_titles, 
                     labels = M_titles)
DF$bin <- cut(DF$Proportion, 
              breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.000000001), 
              labels = c('(0, 0.2]', '(0.2, 0.4]', '(0.4, 0.6]', 
                         '(0.6, 0.8]', '(0.8, 0.1)'),
              include_lowest = TRUE)

# add facet labels as 'label' in DF
DF$label <- rep(c('(A)', '(B)', '(C)', '(D)', '(E)', '(F)'), 
                each = nrow(output)*length(sample_sizes)*length(pcmodes))

# save DF as object
sims_results <- DF
save(sims_results, 
     file = '~/Projects/multiple_paternity_power_analyses/output/sims_results.Rda')

################################################################################
##### contour plots ############################################################
################################################################################

# load in data
load("~/Projects/multiple_paternity_power_analyses/output/sims_results.Rda")

# ##### random, sample size 32 ###################################################
# 
# # testing
#   ggplot(data = sims_results, 
#          aes(x = PropClutches, 
#              y = OSR)) +
#   geom_contour_filled(aes(z = Proportion), bins = 5)
# 
# 
# fig4_random_32 <- sims_results %>% 
#   filter(Sample_Size == '32') %>%
#   filter(Paternal_Contribution_Mode == 'Random') %>%
#   filter(Pop_size == 100) %>%
#   filter(minID == 1) %>%
#   ggplot(aes(x = PropClutches, 
#              y = OSR)) +
#   geom_contour_filled(aes(z = Proportion), 
#                       bins = 5) +
#   # geom_contour_filled(breaks = seq(0, 1, by = 0.20)) +
#   # geom_tile() +
#   # scale_fill_viridis_c() +
#   xlab('Proportion of clutches sampled') +
#   ylab('Operational sex ratio') +
#   scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
#   labs(fill = 'Proportion \n correct \n') +
#   guides(fill = guide_legend(reverse = TRUE)) + 
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank()) +
#   theme(text = element_text(size = 20), 
#         axis.text = element_text(size = 15)) +
#   geom_text(aes(x = 0.15, 
#                 y = 0.85, 
#                 label = label, 
#                 group = label), 
#             size = 7, colour = 'white',
#             inherit.aes = FALSE) +
#   facet_grid(rows = vars(M_title), cols = vars(F_title)) +
#   theme(panel.spacing.x = unit(1.5, "lines")) +
#   theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
#   theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
#   theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
#   ggtitle('Random PCM Sample Size 32')
# 
# fig4_random_32
# fig_name <- "fig4_random_32"
# 
# # save contour plot
# ggsave(fig4_random_32,
#        file = paste('figures/', fig_name, '.png', sep = ''), 
#        height = 11, width = 11)
# 
# ##### random, sample size 96 ###################################################
# 
# fig4_random_96 <- sims_results %>% 
#   filter(Sample_Size == '96') %>%
#   filter(Paternal_Contribution_Mode == 'Random') %>%
#   filter(Pop_size == 100) %>%
#   filter(minID == 1) %>%
#   ggplot(aes(x = PropClutches, 
#              y = OSR, 
#              z = Proportion)) +
#   geom_contour_filled(bins = 5) +
#   xlab('Proportion of clutches sampled') +
#   ylab('Operational sex ratio') +
#   scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
#   labs(fill = 'Proportion \n correct \n') +
#   guides(fill = guide_legend(reverse = TRUE)) + 
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank()) +
#   theme(text = element_text(size = 20), 
#         axis.text = element_text(size = 15)) +
#   geom_text(aes(x = 0.15, 
#                 y = 0.85, 
#                 label = label, 
#                 group = label), 
#             size = 7, colour = 'white',
#             inherit.aes = FALSE) +
#   facet_grid(rows = vars(M_title), cols = vars(F_title)) +
#   theme(panel.spacing.x = unit(1.5, "lines")) +
#   theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
#   theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
#   theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
#   ggtitle('Random PCM Sample Size 96')
# 
# fig4_random_96
# fig_name <- "fig4_random_96"
# 
# # save contour plot
# ggsave(fig4_random_96,
#        file = paste('figures/', fig_name, '.png', sep = ''), 
#        height = 11, width = 11)
# 
# ##### random, overlapping sample sizes 32 and 96 ###############################
# 
# DF <- sims_results %>%
#   filter(Paternal_Contribution_Mode == 'Random') %>%
#   filter(Pop_size == 100) %>%
#   filter(minID == 1)
# 
# DF32 <- DF %>% filter(Sample_Size == 32)
# DF96 <- DF %>% filter(Sample_Size == 96)
# 
# fig4_random_overlap <- ggplot(data = DF32, aes(x = PropClutches, 
#                                                y = OSR, 
#                                                z = Proportion)) +
#   geom_contour_filled(bins = 5, 
#                       alpha = 0.5) +
#   labs(fill = 'Proportion correct \n sample size 32') +
#   guides(fill = guide_legend(reverse = TRUE, 
#                              order = 1)) +   
#   geom_contour(data = DF96, 
#                aes(x = PropClutches,
#                    y = OSR,
#                    z = Proportion, 
#                    color = stat(level)),
#                bins = num_bins, 
#                lwd = 1,
#                alpha = 1) +
#   scale_color_viridis_c(guide = FALSE,
#                         labels = c('(0.0, 0.2]',
#                                    '(0.2, 0.4]',
#                                    '(0.4, 0.6]',
#                                    '(0.6, 0.8]',
#                                    '(0.8, 1.0]')) +
#   xlab('Proportion of clutches sampled') +
#   ylab('Operational sex ratio') +
#   scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank()) +
#   theme(text = element_text(size = 20), 
#         axis.text = element_text(size = 15)) +
#   geom_text(aes(x = 0.15, 
#                 y = 0.85, 
#                 label = label, 
#                 group = label), 
#             size = 7, colour = 'white',
#             inherit.aes = FALSE) +
#   facet_grid(rows = vars(M_title), cols = vars(F_title)) +
#   theme(panel.spacing.x = unit(1.5, "lines")) +
#   theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
#   theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
#   theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
#   ggtitle('Random PCM Sample Sizes 32 and 96') +
#   guides(
#     color = guide_legend(
#       title = 'Proportion correct \n sample size 96', 
#       override.aes = list(color = rev(viridis(5))), 
#       reverse = TRUE))
# 
# fig4_random_overlap
# fig_name <- "fig4_random_overlap"
# 
# # save contour plot
# ggsave(fig4_random_overlap,
#        file = paste('figures/', fig_name, '.png', sep = ''), 
#        height = 11, width = 11)
# 
# ##### dominant 90, overlapping sample sizes 32 and 96 ##########################
# 
# DF <- sims_results %>%
#   filter(Paternal_Contribution_Mode == 'Dominant 90') %>%
#   filter(Pop_size == 100) %>%
#   filter(minID == 1)
# 
# DF32 <- DF %>% filter(Sample_Size == 32)
# DF96 <- DF %>% filter(Sample_Size == 96)
# 
# fig4_dominant90_overlap <- ggplot(data = DF32, aes(x = PropClutches, 
#                                                y = OSR, 
#                                                z = Proportion)) +
#   geom_contour_filled(bins = 5, 
#                       alpha = 0.5) +
#   labs(fill = 'Proportion correct \n sample size 32') +
#   guides(fill = guide_legend(reverse = TRUE, 
#                              order = 1)) +   
#   geom_contour(data = DF96, 
#                aes(x = PropClutches,
#                    y = OSR,
#                    z = Proportion, 
#                    color = stat(level)),
#                bins = num_bins, 
#                lwd = 1,
#                alpha = 1) +
#   scale_color_viridis_c(guide = FALSE,
#                         labels = c('(0.0, 0.2]',
#                                    '(0.2, 0.4]',
#                                    '(0.4, 0.6]',
#                                    '(0.6, 0.8]',
#                                    '(0.8, 1.0]')) +
#   xlab('Proportion of clutches sampled') +
#   ylab('Operational sex ratio') +
#   scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank()) +
#   theme(text = element_text(size = 20), 
#         axis.text = element_text(size = 15)) +
#   geom_text(aes(x = 0.15, 
#                 y = 0.85, 
#                 label = label, 
#                 group = label), 
#             size = 7, colour = 'white',
#             inherit.aes = FALSE) +
#   facet_grid(rows = vars(M_title), cols = vars(F_title)) +
#   theme(panel.spacing.x = unit(1.5, "lines")) +
#   theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
#   theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
#   theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
#   ggtitle('Dominant 90 PCM Sample Sizes 32 and 96') +
#   guides(
#     color = guide_legend(
#       title = 'Proportion correct \n sample size 96', 
#       override.aes = list(color = rev(viridis(5))), 
#       reverse = TRUE))
# 
# fig4_dominant90_overlap
# fig_name <- "fig4_dominant90_overlap"
# 
# # save contour plot
# ggsave(fig4_dominant90_overlap,
#        file = paste('figures/', fig_name, '.png', sep = ''), 
#        height = 11, width = 11)

##### sample size 32, overlapping PCMs random and dominant 90 ##################

DF <- sims_results %>%
  filter(Sample_Size == 32) %>%
  filter(Pop_size == 100) %>%
  filter(minID == 1)

DFrandom <- DF %>% filter(Paternal_Contribution_Mode == 'Random')
DFdominant90 <- DF %>% filter(Paternal_Contribution_Mode == 'Dominant 90')

fig4_samplesize32_overlap <- ggplot(data = DFrandom, aes(x = PropClutches, 
                                                   y = OSR, 
                                                   z = Proportion)) +
  geom_contour_filled(bins = 5, 
                      alpha = 0.5) +
  labs(fill = 'Confidence \n Random PCM') +
  guides(fill = guide_legend(reverse = TRUE, 
                             order = 1)) +   
  geom_contour(data = DFdominant90, 
               aes(x = PropClutches,
                   y = OSR,
                   z = Proportion, 
                   color = stat(level)),
               bins = num_bins, 
               lwd = 1,
               alpha = 1) +
  scale_color_viridis_c(guide = FALSE,
                        labels = c('(0.0, 0.2]',
                                   '(0.2, 0.4]',
                                   '(0.4, 0.6]',
                                   '(0.6, 0.8]',
                                   '(0.8, 1.0]')) +
  xlab('Proportion of clutches sampled') +
  ylab('Operational sex ratio') +
  scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
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
  # ggtitle('Sample Size 32, Random and Dominant 90 PCMs') +
  guides(
    color = guide_legend(
      title = 'Confidence \n Dominant 90 PCM', 
      override.aes = list(color = rev(viridis(5))), 
      reverse = TRUE))

fig4_samplesize32_overlap
fig_name <- "fig4_samplesize32_overlap"

# save contour plot
ggsave(fig4_samplesize32_overlap,
       file = paste('figures/', fig_name, '.png', sep = ''), 
       height = 11, width = 11)

##### sample size 96, overlapping PCMs random and dominant 90 ##################

DF <- sims_results %>%
  filter(Sample_Size == 96) %>%
  filter(Pop_size == 100) %>%
  filter(minID == 1)

DFrandom <- DF %>% filter(Paternal_Contribution_Mode == 'Random')
DFdominant90 <- DF %>% filter(Paternal_Contribution_Mode == 'Dominant 90')

fig4_samplesize96_overlap <- ggplot(data = DFrandom, aes(x = PropClutches, 
                                                         y = OSR, 
                                                         z = Proportion)) +
  geom_contour_filled(bins = 5, 
                      alpha = 0.5) +
  labs(fill = 'Confidence \n Random PCM') +
  guides(fill = guide_legend(reverse = TRUE, 
                             order = 1)) +   
  geom_contour(data = DFdominant90, 
               aes(x = PropClutches,
                   y = OSR,
                   z = Proportion, 
                   color = stat(level)),
               bins = num_bins, 
               lwd = 1,
               alpha = 1) +
  scale_color_viridis_c(guide = FALSE,
                        labels = c('(0.0, 0.2]',
                                   '(0.2, 0.4]',
                                   '(0.4, 0.6]',
                                   '(0.6, 0.8]',
                                   '(0.8, 1.0]')) +
  xlab('Proportion of clutches sampled') +
  ylab('Operational sex ratio') +
  scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
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
  # ggtitle('Sample Size 96, Random and Dominant 90 PCMs') +
  guides(
    color = guide_legend(
      title = 'Confidence \n Dominant 90 PCM', 
      override.aes = list(color = rev(viridis(5))), 
      reverse = TRUE))

fig4_samplesize96_overlap
fig_name <- "fig4_samplesize96_overlap"

# save contour plot
ggsave(fig4_samplesize96_overlap,
       file = paste('figures/', fig_name, '.png', sep = ''), 
       height = 11, width = 11)

# ##### why are they all the same??? #############################################
# 
# load("~/Projects/multiple_paternity_power_analyses/output/random_32.Rda")
# random_32$PCM <- 'Random'
# random_32$sample_size <- 32
# 
# load("~/Projects/multiple_paternity_power_analyses/output/random_96.Rda")
# random_96$PCM <- 'Random'
# random_96$sample_size <- 96
# 
# load("~/Projects/multiple_paternity_power_analyses/output/dominant90_32.Rda")
# dominant90_32$PCM <- 'Dominant 90'
# dominant90_32$sample_size <- 32
# 
# load("~/Projects/multiple_paternity_power_analyses/output/dominant90_96.Rda")
# dominant90_96$PCM <- 'Dominant 90'
# dominant90_96$sample_size <- 96
# 
# to_plot <- rbind(random_32, random_96, dominant90_32, dominant90_96)
# 
# for (d in 1:length(data_titles)) {
#   
#   to_plot %>%
#   # <- DF %>%
#   #   filter(Paternal_Contribution_Mode %in% c('Random', 'Dominant 90')) %>%
#   #   # filter(OSR <= 0.5) %>%
#   #   filter(Scenario == data_titles[d]) %>%
#   #   mutate(bin = cut(Proportion,
#   #                    breaks = seq(from = -0.0001, to = 1, length = 6),
#   #                    include_lowest = TRUE)) %>%
#     # mutate(contour = as.numeric(sub("(\\(|\\[)([^,]+),.*", "\\2", 
#     #                                 levels(bin)))) %>%
#     ggplot(aes(x = PropClutches, 
#                y = OSR, 
#                z = Proportion,
#                fill = Proportion
#     )) +
#     geom_tile() +
#     # geom_contour_filled(bins = 5,
#     #                     alpha = 0.5) +
#     # geom_contour(aes(z = Proportion, color = stat(level)), 
#     #              bins = 5, 
#     #              lwd = 1) +
#     #              # breaks = seq(from = 0, to = 1, length = 15)) +
#     scale_fill_viridis_c() +
#     facet_grid(rows = vars(PCM), 
#                cols = vars(sample_size)) +
#     ggtitle('uniform F no M')
#   
#   print(to_plot)
#   
# }
# 
# 


# ##### different population sizes - random 32 base_F_base_M #####################
# 
# pop_sizes <- DF %>%
#   filter(Paternal_Contribution_Mode == 'Random') %>%
#   filter(Sample_Size == 32) %>%
#   filter(Mating_system == 'base_F_base_M') %>%
#   mutate(label = case_when(Pop_size == 100 & minID == 0.9 ~ '(a)', 
#                            Pop_size == 200 & minID == 0.9 ~ '(b)', 
#                            Pop_size == 500 & minID == 0.9 ~ '(c)', 
#                            Pop_size == 100 & minID == 1 ~ '(d)', 
#                            Pop_size == 200 & minID == 1 ~ '(e)', 
#                            Pop_size == 500 & minID == 1 ~ '(f)')) %>%
#   mutate(ID_label = case_when(minID == 0.9 ~ 'ID 90%+ of fathers', 
#                               minID == 1 ~ 'ID 100% of fathers'))
# 
# pop_sizes$Pop_size <- factor(pop_sizes$Pop_size, 
#                              levels = c(100, 200, 500), 
#                              labels = c('Population size 100', 
#                                         'Population size 200',
#                                         'Population size 500'))
# pop_sizes$ID_label <- factor(pop_sizes$ID_label, 
#                              levels = c('ID 90%+ of fathers', 
#                                         'ID 100% of fathers'))
# 
# colors <- viridisLite::viridis(5)
# 
# 
# fig4_pop_sizes_random <- ggplot(data = pop_sizes, 
#                                 aes(x = PropClutches,
#                                     y = OSR, 
#                                     z = Proportion)) +
#   geom_contour_filled(
#     # bins = n_distinct(DF$bin, na.rm = TRUE)
#     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.000000001)
#   ) +
#   # scale_color_viridis_d() +
#   xlab('Proportion of clutches sampled') +
#   ylab('Operational sex ratio') +
#   scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
#   labs(fill = 'Proportion \n correct \n') +
#   guides(fill = guide_legend(reverse = TRUE)) + 
#   scale_fill_manual(labels = c('(0, 0.2]', 
#                                '(0.2, 0.4]', 
#                                '(0.4, 0.6]', 
#                                '(0.6, 0.8]', 
#                                '(0.8, 0.1)'), 
#                     values = colors) +
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank()) +
#   theme(text = element_text(size = 20), 
#         axis.text = element_text(size = 15)) +
#   geom_text(aes(x = 0.15, 
#                 y = 0.85, 
#                 label = label, 
#                 group = label), 
#             size = 7, colour = 'white',
#             inherit.aes = FALSE) +
#   facet_grid(rows = vars(ID_label), 
#              cols = vars(Pop_size)) +
#   theme(panel.spacing.x = unit(1.5, "lines")) +
#   theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
#   theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
#   theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
#   ggtitle('Random 32 base_F_base_M')
# 
# fig4_pop_sizes_random
# fig_name <- "fig4_pop_sizes_random"
# 
# # save contour plot
# ggsave(fig4_pop_sizes_random,
#        file = paste('figures/', fig_name, '.png', sep = ''), 
#        height = 6, width = 12)

# ##### different population sizes - dominant90 32 base_F_base_M #################
# 
# pop_sizes <- DF %>%
#   filter(Paternal_Contribution_Mode == 'Dominant 90') %>%
#   filter(Sample_Size == 32) %>%
#   filter(Mating_system == 'base_F_base_M') %>%
#   mutate(label = case_when(Pop_size == 100 & minID == 0.9 ~ '(a)', 
#                            Pop_size == 200 & minID == 0.9 ~ '(b)', 
#                            Pop_size == 500 & minID == 0.9 ~ '(c)', 
#                            Pop_size == 100 & minID == 1 ~ '(d)', 
#                            Pop_size == 200 & minID == 1 ~ '(e)', 
#                            Pop_size == 500 & minID == 1 ~ '(f)')) %>%
#   mutate(ID_label = case_when(minID == 0.9 ~ 'ID 90%+ of fathers', 
#                               minID == 1 ~ 'ID 100% of fathers'))
# 
# pop_sizes$Pop_size <- factor(pop_sizes$Pop_size, 
#                              levels = c(100, 200, 500), 
#                              labels = c('Population size 100', 
#                                         'Population size 200',
#                                         'Population size 500'))
# pop_sizes$ID_label <- factor(pop_sizes$ID_label, 
#                              levels = c('ID 90%+ of fathers', 
#                                         'ID 100% of fathers'))
# 
# colors <- viridisLite::viridis(5)
# 
# 
# fig4_pop_sizes_dominant90 <- ggplot(data = pop_sizes, 
#                                     aes(x = PropClutches,
#                                         y = OSR, 
#                                         z = Proportion)) +
#   geom_contour_filled(
#     # bins = n_distinct(DF$bin, na.rm = TRUE)
#     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.000000001)
#   ) +
#   # scale_color_viridis_d() +
#   xlab('Proportion of clutches sampled') +
#   ylab('Operational sex ratio') +
#   scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
#   labs(fill = 'Proportion \n correct \n') +
#   guides(fill = guide_legend(reverse = TRUE)) + 
#   scale_fill_manual(labels = c('(0, 0.2]', 
#                                '(0.2, 0.4]', 
#                                '(0.4, 0.6]', 
#                                '(0.6, 0.8]', 
#                                '(0.8, 0.1)'), 
#                     values = colors) +
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank()) +
#   theme(text = element_text(size = 20), 
#         axis.text = element_text(size = 15)) +
#   geom_text(aes(x = 0.15, 
#                 y = 0.85, 
#                 label = label, 
#                 group = label), 
#             size = 7, colour = 'white',
#             inherit.aes = FALSE) +
#   facet_grid(cols = vars(ID_label), 
#              rows = vars(Pop_size)) +
#   theme(panel.spacing.x = unit(1.5, "lines")) +
#   theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
#   theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
#   theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
#   ggtitle('Dominant 90 32 base_F_base_M')
# 
# fig4_pop_sizes_dominant90
# fig_name <- "fig4_pop_sizes_dominant90"
# 
# # save contour plot
# ggsave(fig4_pop_sizes_dominant90,
#        file = paste('figures/', fig_name, '.png', sep = ''), 
#        height = 6, width = 12)

##### different population sizes - dominant90 32 uniform_F_uniform_M ###########

# mating_system <- 'uniform_F_uniform_M'
mating_system <- 'uniform_F_no_M'

pop_sizes <- DF %>%
  filter(Sample_Size == 32) %>%
  filter(Mating_system == mating_system) %>%
  mutate(label = case_when(Pop_size == 100 & minID == 0.9 ~ '(A)', 
                           Pop_size == 200 & minID == 0.9 ~ '(B)', 
                           Pop_size == 500 & minID == 0.9 ~ '(C)',
                           Pop_size == 1000 & minID == 0.9 ~ '(D)',
                           
                           Pop_size == 100 & minID == 1 ~ '(E)', 
                           Pop_size == 200 & minID == 1 ~ '(F)', 
                           Pop_size == 500 & minID == 1 ~ '(G)', 
                           Pop_size == 1000 & minID == 1 ~ '(H)', 
  )) %>%
  mutate(ID_label = case_when(minID == 0.9 ~ 'ID 90%+ of fathers', 
                              minID == 1 ~ 'ID 100% of fathers'))

pop_sizes$Pop_size <- factor(pop_sizes$Pop_size, 
                             levels = c(100, 200, 500, 1000), 
                             labels = c('N = 100', 
                                        'N = 200',
                                        'N = 500', 
                                        'N = 1000'))
pop_sizes$ID_label <- factor(pop_sizes$ID_label, 
                             levels = c('ID 90%+ of fathers', 
                                        'ID 100% of fathers'))

colors <- viridisLite::viridis(5)

pop_sizes_random <- pop_sizes %>% 
  filter(Paternal_Contribution_Mode == 'Random')
pop_sizes_dominant90 <- pop_sizes %>% 
  filter(Paternal_Contribution_Mode == 'Dominant 90')


# fig5_pop_sizes_overlap <- ggplot(data = pop_sizes_random, 
figS2_pop_sizes_overlap <- ggplot(data = pop_sizes_random, 
                                 aes(x = PropClutches,
                                     y = OSR, 
                                     z = Proportion)) +
  geom_contour_filled(
    # bins = n_distinct(DF$bin, na.rm = TRUE)
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.000000001), 
    alpha = 0.5
  ) +
  labs(fill = 'Confidence \n Random PCM') +
  guides(fill = guide_legend(reverse = TRUE, 
                             order = 1)) + 
  scale_fill_manual(labels = c('(0, 0.2]', 
                               '(0.2, 0.4]', 
                               '(0.4, 0.6]', 
                               '(0.6, 0.8]', 
                               '(0.8, 0.1)'), 
                    values = colors) +
  geom_contour(data = pop_sizes_dominant90,
               aes(x = PropClutches,
                   y = OSR,
                   z = Proportion,
                   color = after_stat(level)),
               # breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0000000001), 
               binwidth = 0.2,
               lwd = 1,
               alpha = 1) +
  scale_color_viridis_c(guide = FALSE,
                        labels = c('(0.0, 0.2]',
                                   '(0.2, 0.4]',
                                   '(0.4, 0.6]',
                                   '(0.6, 0.8]',
                                   '(0.8, 1.0]')) +
  xlab('Proportion of clutches sampled') +
  ylab('Operational sex ratio') +
  scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
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
  facet_grid(cols = vars(ID_label), 
             rows = vars(Pop_size)) +
  theme(panel.spacing.x = unit(1.5, "lines")) +
  theme(axis.title.y = element_text(vjust = 3, hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = -1, hjust = 0.5)) +
  theme(plot.margin = margin(1, 0, 0.75, 0.75, "cm")) +
  guides(
    color = guide_legend(
      title = 'Confidence \n Dominant 90 PCM', 
      override.aes = list(color = rev(viridis(5))), 
      reverse = TRUE))

# fig5_pop_sizes_overlap
# fig_name <- paste('fig5_pop_sizes_overlap_', mating_system, sep = '')

figS2_pop_sizes_overlap
fig_name <- paste('figS2_pop_sizes_overlap_', mating_system, sep = '')

# save contour plot
# ggsave(fig5_pop_sizes_overlap,
#        file = paste('figures/', fig_name, '.png', sep = ''), 
#        height = 14, width = 11)

ggsave(figS2_pop_sizes_overlap,
       file = paste('figures/', fig_name, '.pdf', sep = ''), 
       height = 14, width = 11)

