n_sims = 1
pop_size = 100
sample_size = 32
paternal_contribution_mode = 'random'
Fprob <- c(0.463, 0.318, 0.157, 0.034, 0.028)
Mprob <- c(1)
clutches_mu = 4.95
clutches_sd = 2.09

prop_correct = proportion_correct_all
scenario = 'base_Fprob_no_Mprob'

library(dplyr)
library(magrittr)
library(stats)


source('code/clutches_to_sample.R')

load("~/Projects/multiple_paternity_power_analyses/output/proportion_correct_all_1e+05.Rdata")
library(remotes)
remotes::install_github(repo = 'vquennessen/MultiplePaternityPowerAnalyses')
library(MultiplePaternityPowerAnalyses)
clutches_to_sample(n_sims = 1,
                   pop_size = 100,
                   sample_size = 32,
                   paternal_contribution_mode = 'random',
                   Fprob = c(0.463, 0.318, 0.157, 0.034, 0.028),
                   Mprob = c(1),
                   clutches_mu = 4.95,
                   clutches_sd = 2.09,
                   prop_correct = proportion_correct_all,
                   scenario = 'base_Fprob_no_Mprob')

