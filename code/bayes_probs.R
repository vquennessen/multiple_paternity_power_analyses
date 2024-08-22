bayes_probs <- function(number_of_males, 
                        fertilization_mode = NULL, 
                        males_identified = NULL, 
                        sample_size = NULL) {
  
  # load in results
  load(number_of_males)
  
  # extract fertilization modes
  fertilization_modes <- unique(number_of_males$Fertilization_mode)
  
  # extract max number of males
  max_males <- max(number_of_males$Males_contributing)
  
  # extract sample sizes
  sample_sizes <- unique(number_of_males$Sample_size)  
  
  # # dimensions
  # FM <- length(fertilization_modes)
  # MM <- max_males
  
  # if no specific parameters are given, give all the parameters for all the 
  # possible combinations
  if (is.null(fertilization_mode) & 
      is.null(males_identified) & 
      is.null(sample_size)) {
    
    # initialize dataframe
    bayes_probs_DF <- data.frame()
    
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
        
        # remove first row
        DF <- DF[-1, ]
        
        # get rid of any NaN values if they exist
        if (sum(is.nan(DF$Probability)) > 1) {
          
          # replace values that are not numbers with NA
          DF[which(is.nan(DF$Probability)), ]$Probability <- 0
          
        }
        
        # round probability to 3 digits
        DF$Probability <- round(DF$Probability, 3)
        
        # add 
        bayes_probs_DF <- rbind(bayes_probs_DF, DF)
        
      }
      
    }
    
    output <- bayes_probs_DF
    
    # save output
    save(bayes_probs_DF, file = 'bayes_probs_output.Rdata')
    
  } else {
    
    output <- number_of_males %>%
      filter(Fertilization_Mode == fertilization_mode) %>%
      filter(Males_identified == males_identified)
    
  }
  
  return(output)
  
}

setwd('~/Projects/multiple_paternity_power_analyses/data')

library(dplyr)
library(tidyr)

number_of_males <- "number_of_males.Rdata"

fertilization_mode <- NULL
males_identified <- NULL
sample_size <- NULL

bayes_probs(number_of_males, 
            fertilization_mode = NULL, 
            males_identified = NULL, 
            sample_size = NULL)

load("~/Projects/multiple_paternity_power_analyses/data/bayes_probs_output.Rdata")

# make it a less obnoxiously long table
prettier_bayes_probs <- bayes_probs_DF %>%
  pivot_wider(names_from = 'Males_contributed', values_from = 'Probability') %>%
  arrange(Fertilization_Mode, Sample_size, Males_identified)

# save output
save(prettier_bayes_probs, file = 'prettier_bayes_probs_output.Rdata')
