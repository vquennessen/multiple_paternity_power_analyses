bayes_probs <- function(number_of_males, 
                        fertilization_modes, 
                        max_males, 
                        fertilization_mode = NULL, 
                        males_identified = NULL, 
                        sample_size = NULL) {
  
  # dimensions
  FM <- length(fertilization_modes)
  MM <- max_males
  
  # load in results
  load(number_of_males)
  
  # sample sizes
  sample_sizes <- unique(number_of_males$Sample_size)
  
  if (is.null(fertilization_mode) & 
      is.null(males_identified) & 
      is.null(sample_size)) {
    
    # initialize dataframe
    bayes_probs_DF <- data.frame()
    
    for (f in 1:length(fertilization_modes)) {
      
      for (m in 1:max_males) {
        
        for (s in 1:length(sample_sizes)) {
          
          # pull out fertilization mode and the number of males detected
          subset <- number_of_males %>%
            filter(Fertilization_Mode == fertilization_modes[f]) %>%
            filter(Males_detected == m) %>%
            filter(Sample_size == sample_sizes[s])
          
          # add 
          rbind(bayes_probs_DF, subset)
          
        }
        
      }
      
    }
    
    
  } else {
    
    output <- number_of_males %>%
      filter(Fertilization_Mode == fertilization_mode) %>%
      filter(Males_identified == males_identified)
    
  }
  
  return(output)
  
}