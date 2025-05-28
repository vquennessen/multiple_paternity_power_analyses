# run Q2

run_Q2 <- function(arguments) {
  
  # function arguments
  sample_size                 <- arguments$Var1
  paternal_contribution_mode  <- arguments$Var2
  scenario                    <- arguments$Var3
  n_sims                      <- arguments$Var4
  pop_size                    <- arguments$Var5
  minimum_id                  <- arguments$Var6

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

  # average and sd numbers of clutches per mother
  clutches_mu <- 4.95
  clutches_sd <- 2.09
  
  # load probabilities object
  load('output/probabilities.Rda')
  
  # pull out probabilities of IDing different numbers of fathers
  probs_id <- probabilities %>%
    dplyr::filter(Sample_Size == sample_size) %>%
    dplyr::filter(Paternal_Contribution_Mode == paternal_contribution_mode)
  
  # write to progress text file
  update <- paste(lubridate::now(), 
                  ' - ', scenario, ' - N', pop_size, 
                  ' - sample size ', sample_size, ' - ', 
                  paternal_contribution_mode, ' - ', 
                  n_sims, ' sims - started!', 
                  sep = '')
  
  write(update, file = 'progress.txt', append = TRUE)
  
  # run sample_clutches
  output <- clutches_to_sample(n_sims, 
                               pop_size, 
                               sample_size, 
                               paternal_contribution_mode,
                               Fprob, 
                               Mprob, 
                               clutches_mu, 
                               clutches_sd, 
                               probs_id, 
                               scenario, 
                               minimum_id)
  
  # save output
  save(output, 
       file = paste('output/2025_05_27_N1000_10000sims/',
                    scenario, 
                    '/', 
                    scenario, 
                    '_',
                    sample_size, 
                    'samples_',
                    paternal_contribution_mode, 
                    '_', 
                    n_sims, 
                    'sims.Rdata', 
                    sep = ''))
  
}
