bayes_probs <- function(number_of_males, fertilization_modes, max_males) {
  
  # dimentions
  FM <- length(fertilization_modes)
  MM <- max_males
  
  # initialize dataframe
  bayes_probs_DF <- data.frame(Fertilization_mode = rep(fertilization_modes, 
                                                        each = MM*MM), 
                               Males_detected = rep(c(1:max_males), 
                                                    times = FM, each = MM),
                               Males_contributed = rep(c(1:max_males), 
                                                       times = FM*MM),
                               Probability = NA)
  
  
}