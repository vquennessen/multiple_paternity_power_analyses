#' conditional_probabilities {multiple_paternity_power_analyses}
#'
#' @param probabilities a data frame that has columns for 
#' 
#' Paternal_Contribution_Mode - vector of character values of the different 
#' paternal contribution modes to analyse. Default values are c('random', 
#' 'exponential', 'dominant50', 'dominant70', 'dominant90', 'mixed_dominant').
#' 
#' Fathers_Actual - the integer number of actual fathers for a clutch. Default 
#' values are from 1 to 5. 
#' 
#' Sample_Size - the integer number of hatchlings to sample from a clutch. 
#' Default values are 32 and 96. 
#' 
#' Fathers_Observed - the integer number of fathers that were identified.
#' 
#' Probability - the numeric probability of Fathers_Observed given 
#' Fathers_Actual. Values must be between 0 and 1, inclusive.
#' 
#' @returns two data frames of conditional probabilities for the number of 
#' actual fathers (Fathers_Actual) given the number of fathers identified 
#' (Fathers_Observed). The first data frame has columns for 
#' Paternal_Contribution_Mode, Sample_Size, Fathers_Observed, Fathers_Actual, 
#' and a Probability column that is the new conditional probability. The second
#' data frame is wider so that the conditional probability values are displayed 
#' by rows of Paternal_Contribution_Mode and Fathers_Observed with columns for 
#' Fathers_Actual. 
#' 
#' @export
#' 
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' probabilities <- probability_id_fathers(hatchlings_mu = 100.58, 
#'                                         hatchlings_sd = 22.61, 
#'                                         max_fathers = 5, 
#'                                         n_sims = 1e5, 
#'                                         n_sizes = c(32, 96), 
#'                                         Paternity_contribution_mode = 
#'                                                          c('random', 
#'                                                            'exponential', 
#'                                                            'dominant50', 
#'                                                            'dominant70', 
#'                                                            'dominant90', 
#'                                                            'mixed_dominant'), 
#'                                         min_clutch_size = 10)
#'      
#' conditional_probabilities(probabilities)

conditional_probabilities <- function(probabilities) {
  
  # extract paternal contribution modes
  PCMs <- unique(probabilities$Paternal_Contribution_Mode)
  
  # extract max number of fathers
  max_fathers <- max(probabilities$Fathers_Actual)
  
  # extract sample sizes
  sample_sizes <- unique(probabilities$Sample_Size)  
  
  # initialize dataframe
  conditional_probabilities <- data.frame()
  
  # for each paternal contribution mode
  for (p in 1:length(PCMs)) {
    
    # for each sample size
    for (s in 1:length(sample_sizes)) {
      
      # create subset from data
      # pull out paternal contribution mode and the sample size
      subset1 <- probabilities %>%
        filter(Paternal_Contribution_Mode == PCMs[p]) %>%
        filter(Sample_Size == sample_sizes[s])        
      
      # initialize dataframe
      DF <- data.frame(Paternal_Contribution_Mode = PCMs[p], 
                       Sample_Size = sample_sizes[s], 
                       Fathers_Observed = unlist(mapply(rep, 1:max_fathers, 
                                                        max_fathers:1)), 
                       Fathers_Actual = paste(unlist(mapply(seq, 1:max_fathers, 
                                                            max_fathers)), 
                                              ' Actual Father(s)', 
                                              sep = ''), 
                       Conditional_Probability = NA)
      
      # probabilities of 1:max_fathers Fathers_Actual - assume equal
      PA <- 1/max_fathers
      
      # probabilities of  1:max_fathers Fathers_Observed across all potential 
      # Fathers_Actual
      observed_probabilities <- subset1 %>% 
        group_by(Fathers_Observed) %>% 
        summarize(total = sum(Probability))
      
      # marginal probability for each number of Fathers_Actual
      PBs <- observed_probabilities$total / sum(subset1$Probability)
      
      # index restart
      index <- 0
      
      # for i Fathers_Observed
      for (i in 1:max_fathers) {
        
        # for c Fathers_Actual
        for (c in i:max_fathers) {
          
          # # if i = 1, then c = 1 doesn't exist, set to 0
          # if (c == 1) {
          #   
          #   PBA <- 0
          #   
          # } else {
          
          subset2 <- subset1 %>%
            filter(Fathers_Actual == c) %>%
            filter(Fathers_Observed == i)
          
          # probability of i Fathers_Observed given c Fathers_Actual
          PBA <- subset2$Probability
          
          # calculate PAB (Probability of Fathers_Actual given Fathers_Observed)
          PAB <- PA * PBA / PBs[i]
          
          # index
          index <- index + 1
          
          # # troubleshooting
          # print(index)
          
          # add the PAB to the data frame
          DF$Conditional_Probability[index] <- PAB
          
        }
        
      }
      
      # get rid of any NaN values if they exist
      if (sum(is.nan(DF$Probability)) > 1) {
        
        # replace values that are not numbers with NA
        DF[which(is.nan(DF$Conditional_Probability)), ]$Conditional_Probability <- 0
        
      }
      
      # round probability to 3 digits
      DF$Conditional_Probability <- round(DF$Conditional_Probability, 3)
      
      # add 
      conditional_probabilities <- rbind(conditional_probabilities, DF)
      
      
    }
    
  }
  
  # save output
  save(conditional_probabilities, 
       file = 'output/conditional_probabilities.Rdata')
  
  # make it a less obnoxiously long table
  prettier_conditional_probabilities <- conditional_probabilities %>%
    pivot_wider(names_from = 'Fathers_Actual', 
                values_from = 'Conditional_Probability') %>%
    arrange(Paternal_Contribution_Mode, Sample_Size, Fathers_Observed)
  
  # save output
  save(prettier_conditional_probabilities, 
       file = 'output/prettier_conditional_probabilities.Rdata')
  
  output <- list(conditional_probabilities, prettier_conditional_probabilities)
  
  return(output)
  
}
