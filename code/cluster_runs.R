### cluster runs
# cluster
setwd('/home/quennessenv/multiple_paternity_power_analyses')

# load libraries
library(dplyr)
library(tidyr)
library(parallel)
library(remotes)
remotes::install_github('vquennessen/MultiplePaternityPowerAnalyses')
library(MultiplePaternityPowerAnalyses)

# source function
source('code/run_Q2.R')

# model parameters
sample_sizes <- c(32, 96)                     

paternal_contribution_modes <- c( 
                                 'random',
                                 # 'exponential',
                                 # 'dominant50',
                                 # 'dominant70',
                                 # 'mixed_dominant'
                                 'dominant90'
                                 )

scenarios <- c('base_F_no_M', 
               'uniform_F_no_M', 
               'base_F_uniform_M', 
               'uniform_F_uniform_M',
               'base_F_base_M',
               'uniform_F_base_M'
               )

n_sims <- c(1e+03)

pop_size <- c(1000)

minimum_id <- 0.9

# create probabilities object
probabilities <- probability_id_fathers(hatchlings_mu = 100.58,
                                        hatchlings_sd = 22.61,
                                        max_fathers = 5,
                                        n_sims = 1e+05,
                                        sample_sizes = sample_sizes,
                                        paternal_contribution_modes =
                                          paternal_contribution_modes,
                                        min_clutch_size = 10)[[1]]

# save probabilities object
save(probabilities,
     file = 'output/probabilities.Rda')

# make dataframe of all combinations of arguments
DF <- expand.grid(sample_sizes, 
                  paternal_contribution_modes, 
                  scenarios, 
                  n_sims, 
                  pop_size, 
                  minimum_id)

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {
  
  arguments[i] <- list(DF[i, ])
  
}

########### do the runs ########################################################
mclapply(X = arguments, 
         FUN = run_Q2, 
         mc.cores = 50)
