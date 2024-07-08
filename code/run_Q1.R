# run power analyses for Q1

# # set working directory - desktop
# setwd('~/Projects/multiple_paternity_power_analyses')

# set working directory - cluster
setwd('/home/quennessenv/multiple_paternity_power_analyses')

# load libraries
library(viridis)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# source function
source('code/hatchlings_to_sample.R')

# function parameters
hatchlings_mu <- 100.58                  # number of eggs per nest, mean
hatchlings_sd <- 22.61                   # number of eggs per nest, SD
max_males <- 5                           # max # of M F can mate with
n_sims <- 1e6                            # number of simulations to run

n_sizes <- c(32, 96)                     # sample sizes to calculate probs for  
computer <- 'cluster'                    # computer
fertilization_modes <- c('random',       # fertilization modes
                         'exponential', 
                         'dominant50', 
                         'dominant70', 
                         'dominant90', 
                         'mixed_dominant')
      
# initialize total dataframe of probabilities
probs <- data.frame(NULL)               

# intialize list of figures
figs <- list(NULL)

# run the function for each fertilization mode
for (f in 1:nf) {
  
  # generate output
  output <- hatchlings_to_sample(hatchlings_mu,      # number of eggs per nest, mean
                                 hatchlings_sd,      # number of eggs per nest, SD
                                 max_males,          # max M each F can mate with
                                 fertilization_mode = fertilization_modes[f], 
                                 n_sims,             # number of simulations to run
                                 n_sizes,            # sample sizes to run 
                                 computer)           # which computer is this
  
  # append figure to figs list
  figs[[f]] <- output[[1]]
  
  # append DF to probs
  probs1 <- rbind(probs, output[[2]])
  
  # append DFsamples to probs
  sample_probs <- rbind(probs, output[[3]])
  
  # append DFsamples2 to probs
  sample_probs2 <- rbind(probs, output[[4]])
  
  # spit out progress update
  print(paste('Done with ', fertilization_modes[f], ' fertilization mode!'))
  
}

# save objects into power analyses output folder

# save figures
save(figs, 
     file = paste('figures/', n_sims, '.Rdata', sep = ''))

# save table
save(probs1, 
     file = paste('data/probabilities_', n_sims, '.Rdata', sep = ''))

# save table
save(sample_probs, 
     file = paste('data/sample_probabilities_', n_sims, '.Rdata', sep = ''))

# save table
save(sample_probs2, 
     file = paste('data/sample_probabilities2_', n_sims, '.Rdata', sep = ''))