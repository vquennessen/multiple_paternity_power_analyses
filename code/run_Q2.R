# run Q2

run_Q2 <- function(arguments) {
  
  # function arguments
  sample_size                 <- arguments$Var1
  paternal_contribution_mode  <- arguments$Var2
  scenario                    <- arguments$Var3
  nsims                       <- 100
  
  # model parameters
  pop_size <- 100                               # total population size
  
  # Fprob and Mprob based on scenario
  if (scenario == 'base_F_no_M') {
    
    Fprob <- c(0.463, 0.318, 0.157, 0.034, 0.028)
    Mprob <- c(1)
    
  } 
  
    if (scenario == 'base_F_uniform_M') {
    
    Fprob <- c(0.463, 0.318, 0.157, 0.034, 0.028)
    Mprob <- c(0.2, 0.2, 0.2, 0.2, 0.2)   
    
    }
  
  if (scenario == 'base_F_base_M') {
    
    Fprob <- c(0.463, 0.318, 0.157, 0.034, 0.028)
    Mprob <- c(0.463, 0.318, 0.157, 0.034, 0.028)
    
  }
  
  if (scenario == 'uniform_F_no_M') {
    
    Fprob <- c(0.2, 0.2, 0.2, 0.2, 0.2)   
    Mprob <- c(1)
    
  } 
  
  if (scenario == 'uniform_F_uniform_M') {
    
    Fprob <- c(0.2, 0.2, 0.2, 0.2, 0.2)   
    Mprob <- c(0.2, 0.2, 0.2, 0.2, 0.2)   
    
  }
  
  if (scenario == 'uniform_F_base_M') {
    
    Fprob <- c(0.2, 0.2, 0.2, 0.2, 0.2)   
    Mprob <- c(0.463, 0.318, 0.157, 0.034, 0.028)
    
  }  

  
  clutches_mu <- 4.95                              # average # of nests per F
  clutches_sd <- 2.09                              # sd # of nests per F
  
  # pull out probabilities of IDing different numbers of fathers
  prop_correct <- proportion_correct_all %>%
    dplyr::filter(Sample_Size == sample_size) %>%
    dplyr::filter(Paternal_Contribution_Mode == paternal_contribution_mode)
  
  # write to progress text file
  update <- paste(Sys.time(), 
                  ' - ',
                  scenario,
                  ' - sample size ', 
                  sample_size, 
                  ' - ', 
                  paternal_contribution_mode, 
                  ' - ', 
                  nsims, 
                  ' sims', 
                  sep = '')
  
  write(update, file = 'progress.txt', append = TRUE)
  
  # run sample_clutches
  output <- clutches_to_sample(nsims, 
                               pop_size, 
                               sample_size, 
                               paternal_contribution_mode,
                               Fprob, 
                               Mprob, 
                               clutches_mu, 
                               clutches_sd, 
                               prop_correct, 
                               scenario)
  
  # save output
  save(output, 
       file = paste('../output/',
                    scenario, 
                    '/', 
                    scenario, 
                    '_',
                    sample_size, 
                    'samples_',
                    paternal_contribution_mode, 
                    '_', 
                    nsims, 
                    'sims.Rdata', 
                    sep = ''))
  
}
