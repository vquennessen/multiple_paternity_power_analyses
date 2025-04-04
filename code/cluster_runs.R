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

# load probabilities object
load('output/proportion_correct_all_1e+05.Rdata')

# model parameters
sample_sizes <- c(32, 96)                     

paternal_contribution_modes <- c('random', 
                                 'exponential', 
                                 'dominant50', 
                                 'dominant70', 
                                 'dominant90', 
                                 'mixed_dominant')

scenarios <- c('base_Fprob_no_Mprob', 'uniform_Fprob_no_Mprob', 
               'base_Fprob_uniform_Mprob', 'uniform_Fprob_uniform_Mprob',
               'base_Fprob_base_Mprob', 'uniform_Fprob_base_Mprob')


# make dataframe of all combinations of arguments
DF <- expand.grid(sample_sizes, paternal_contribution_modes, scenarios)

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
