# load libraries
library(dplyr)
library(magrittr)
library(stats)
library(remotes)
remotes::install_github(repo = 'vquennessen/MultiplePaternityPowerAnalyses')
library(MultiplePaternityPowerAnalyses)



load("~/Projects/multiple_paternity_power_analyses/output/probabilities.Rda")

clutches_to_sample(n_sims = 10,
                   pop_size = 100,
                   sample_size = 32,
                   paternal_contribution_mode = 'random',
                   Fprob = c(0.463, 0.318, 0.157, 0.034, 0.028),
                   Mprob = c(1),
                   clutches_mu = 4.95,
                   clutches_sd = 2.09,
                   probs_id = probabilities,
                   scenario = 'base_F_no_M')

# function parameters
n_sims = 10
pop_size = 100
sample_size = 32
paternal_contribution_mode = 'random'
Fprob <- c(0.463, 0.318, 0.157, 0.034, 0.028)
Mprob <- c(1)
clutches_mu = 4.95
clutches_sd = 2.09
probs_id = probabilities
scenario = 'base_F_no_M'
