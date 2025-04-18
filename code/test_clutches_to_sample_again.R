# load libraries
library(magrittr)
library(dplyr)

# load probabilities object
load("../output/proportion_correct_all_1e+05.Rdata")

# pull out probabilities of IDing different numbers of fathers
prop_correct <- proportion_correct_all %>%
  dplyr::filter(Sample_Size == sample_size) %>%
  dplyr::filter(Paternal_Contribution_Mode == paternal_contribution_mode)

# take 1
output1 <- clutches_to_sample(n_sims = 100, 
                              pop_size = 100, 
                              sample_size = 32, 
                              paternal_contribution_mode = 'dominant90', 
                              Fprob = c(0.2, 0.2, 0.2, 0.2, 0.2), 
                              Mprob = c(1), 
                              clutches_mu = 4.95, 
                              clutches_sd = 2.09, 
                              prop_correct, 
                              scenario = 'uniform_F_no_M')

save(output1, file = 'test/take1.Rda')