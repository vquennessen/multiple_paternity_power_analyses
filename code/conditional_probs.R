conditional_probs <- function(number_of_males) {
  
  # # load in dataframe created through number_of_males function
  # load(number_of_males)
  
  # extract fertilization modes
  fertilization_modes <- unique(number_of_males$Fertilization_mode)
  
  # extract max number of males
  max_males <- max(number_of_males$Males_contributing)
  
  # extract sample sizes
  sample_sizes <- unique(number_of_males$Sample_size)  
  
  # initialize dataframe
  conditional_probs <- data.frame()
  
  # for each fertilization mode
  for (f in 1:length(fertilization_modes)) {
    
    # for each sample size
    for (s in 1:length(sample_sizes)) {
      
      # create subset from data
      # pull out fertilization mode and the sample size
      subset1 <- number_of_males %>%
        filter(Fertilization_mode == fertilization_modes[f]) %>%
        filter(Sample_size == sample_sizes[s])        
      
      # initialize dataframe
      DF <- data.frame(Fertilization_Mode = fertilization_modes[f], 
                       Sample_size = sample_sizes[s], 
                       Males_identified = unlist(mapply(rep, 
                                                        1:max_males, 
                                                        max_males:1)), 
                       Males_contributed = unlist(mapply(seq, 
                                                         1:max_males, 
                                                         max_males)), 
                       Probability = NA)
      
      # probabilities of 1:max_males males contributing - assume equal
      PA <- 1/(max_males - 1)
      
      # probabilities of identifying 1:max_males males
      probs <- subset1 %>% 
        group_by(Males_identified) %>% 
        summarize(total = sum(Probability))
      
      # marginal probability for each number of males identified
      PBs <- probs$total / sum(subset1$Probability)
      
      # probabilities of males identified given contributed
      
      # index restart
      index <- 0
      
      # for i males identified
      for (i in 1:max_males) {
        
        # for c males contributed
        for (c in i:max_males) {
          
          # if i = 1, then c = 1 doesn't exist, set to 0
          if (c == 1) {
            
            PBA <- 0
            
          } else {
            
            subset2 <- subset1 %>%
              filter(Males_identified == i) %>%
              filter(Males_contributing == c)
            
            # probability of i males identified given c contributed
            PBA <- subset2$Probability
            
          }
          
          # calculate PAB
          PAB <- PA * PBA / PBs[i]
          
          # index
          index <- index + 1
          
          # add the PAB to the dataframe
          DF$Probability[index] <- PAB
          
        }
        
      }
      
      # # remove first row
      # DF <- DF[-1, ]
      
      # get rid of any NaN values if they exist
      if (sum(is.nan(DF$Probability)) > 1) {
        
        # replace values that are not numbers with NA
        DF[which(is.nan(DF$Probability)), ]$Probability <- 0
        
      }
      
      # round probability to 3 digits
      DF$Probability <- round(DF$Probability, 3)
      
      # add 
      conditional_probs <- rbind(conditional_probs, DF)
      
    }
    
  }
  
  # save output
  save(conditional_probs, file = 'conditional_probs_output.Rdata')
  
  # make it a less obnoxiously long table
  prettier_conditional_probs <- conditional_probs %>%
    pivot_wider(names_from = 'Males_contributed', values_from = 'Probability') %>%
    arrange(Fertilization_Mode, Sample_size, Males_identified)
  
  # save output
  save(prettier_conditional_probs, file = 'prettier_conditional_probs_output.Rdata')
    
  output <- list(conditional_probs, prettier_conditional_probs)

  return(output)
  
}

setwd('~/Projects/multiple_paternity_power_analyses/output')

library(dplyr)
library(tidyr)

load("~/Projects/multiple_paternity_power_analyses/data/number_of_males.Rdata")

conditional_probs(number_of_males)
