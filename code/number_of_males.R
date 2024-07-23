#' number_of_males
#'
#' \code{number_of_males} saves a dataframe recording the probabilities of 
#' detecting 1 - max_males males given a specific number of potential males, 
#' sample size, and fertilization mode. 
#'
#' @param hatchlings_mu numeric value, the mean number of hatchlings produced in
#'    a nest. Default value is 100.58. 
#' @param hatchlings_sd numeric value, the standard deviation of the number of 
#'    hatchlings produced in a nest. Default value is 22.61. 
#' @param max_males integer value, the maximum number of males that females can 
#'    mate with. Default value is 5. 
#' @param n_sims integer value, the number of simulations to run. Default value 
#'    is 1e6. 
#' @param n_sizes vector of integer values, the sample size(s) to collect. 
#'    Default value is c(32, 96). 
#' @param fertilization_modes vector of character values, the different modes of
#'    fertilization to analyse. Default value is c('random', 'exponential', 
#'    'dominant50', 'dominant70', 'dominant90', 'mixed_dominant'). 
#' @param min_nest_size integer value, the number of hatchlings to be assigned 
#'    to a nest where 0 or fewer was returned from the normal distribution based 
#'    on hatchlings_mu and hatchlings_sd. 
#'
#' @return saves object where probabilities of identifying 
#' @export
#'
#' @examples
#' number_of_males(hatchlings_mu = 100.58, hatchlings_sd = 22.61, max_males = 5, 
#'                 n_sims = 1e5, n_sizes = c(32, 96), 
#'                 fertilization_modes = c('random', 'exponential', 'dominant50', 
#'                 'dominant70', 'dominant90', 'mixed_dominant'), 
#'                 min_nest_size = 10)

number_of_males <- function(hatchlings_mu = 100.58, 
                            hatchlings_sd = 22.61, 
                            max_males = 5, 
                            n_sims = 1e6, 
                            n_sizes = c(32, 96), 
                            fertilization_modes = c('random', 'exponential', 
                                                    'dominant50', 'dominant70', 
                                                    'dominant90', 'mixed_dominant'), 
                            min_nest_size = 10) 
  
{
  
  ###### Error handling ########################################################
  
  # classes of variables
  if (!is.numeric(hatchlings_mu)) {stop('hatchlings_mu must be a numeric value.')}
  if (!is.numeric(hatchlings_sd)) {stop('hatchlings_sd must be a numeric value.')}
  if (max_males %% 1 != 0) {stop('max_males must be an integer value.')}
  if (n_sims %% 1 != 0) {stop('n_sims must be an integer value.')}
  if (n_sizes %% 1 != 0) {stop('n_sizes must be an integer value.')}
  if (!is.character(fertilization_modes)) 
    {stop('fertilization_modes must be a character vector.')}
  if (min_nest_size %% 1 != 0) {stop('min_nest_size must be an integer value.')}
  
  # acceptable values
  if (hatchlings_mu <= 0) {stop('hatchlings_mu must be greater than 0.')}
  if (hatchlings_sd <= 0) {stop('hatchlings_sd must be greater than 0.')}
  if (max_males < 2) {stop('max_males must be greater than 1.')}
  if (n_sims <= 0) {stop('n_sims must be greater than 0.')}
  if (n_sizes <= 0) {stop('n_sizes must be greater than 0.')}
  if (n_sizes > 96) {stop('n_sizes must be less than 97.')}
  if (! (fertilization_modes) %in% c('random', 'exponential', 'dominant50', 
                                     'dominant70', 'dominant90', 
                                     'mixed_dominant')) 
  {stop('fertilization_modes given are not recognized')}
  if (min_nest_size <= 0) {stop('min_nest_size must be greater than 0.')}
  
  
  # relational values
  if(min_nest_size > (hatchlings_mu - 3*hatchlings_sd)) 
  {stop('min_nest_size is too large given the distribution of hatchlings per nest.')}
  
  ##############################################################################
  
  # dimensions
  nF <- length(fertilization_modes)
  nM <- max_males - 1
  nS <- length(n_sizes)
  
  # pre-allocate data frame
  DF <- data.frame(Fertilization_mode = NA,
                   Males_contributing = NA, 
                   Sample_size = NA,
                   Males_identified = NA,
                   Probability = NA)
  
  # for each fertilization mode
  for (f in 1:nF) {
    
    # fertilization mode
    fertilization_mode <- fertilization_modes[f]
    
    # for each number of males that contribute to a nest:
    for (m in 2:max_males) {
      
      # set contributions per males
      if (fertilization_mode == 'dominant90') {
        MC <- 0.90
        contributions <- c(MC, rep((1 - MC)/(m - 1), (m - 1)))
        
      } else if (fertilization_mode == 'dominant70') {
        MC <- 0.70
        contributions <- c(MC, rep((1 - MC)/(m - 1), (m - 1)))
        
      } else if (fertilization_mode == 'dominant50') {
        MC <- 0.50
        contributions <- c(MC, rep((1 - MC)/(m - 1), (m - 1)))
        
      } else if (fertilization_mode == 'exponential') {
        MC <- 0.5
        contributions <- 0.5^c(1:(m-1))
        contributions <- c(contributions, contributions[m-1])
        
      } else if (fertilization_mode == 'random') {
        contributions <- rep(1/m, m)
        
      } else if (fertilization_mode == 'mixed_dominant') {
        doms <- sample(c(0.50, 0.70, 0.90), size = n_sims, replace = TRUE)
        M1 <- matrix(doms, nrow = n_sims, ncol = 1)
        M2 <- matrix(rep((1 - doms) / (m - 1), m - 1), nrow = n_sims, ncol = m - 1)
        probs <- cbind(M1, M2)
        
      }
      
      # for each sample size
      for (s in 1:nS) {
        
        # pre-allocate vector of males identified
        males_identified <- rep(NA, n_sims)
        
        # initialize nest sizes
        # pull numbers of hatchlings from normal distribution
        n_hatchlings <- rnorm(n = n_sims, mean = hatchlings_mu, sd = hatchlings_sd)
        
        # set any nests with 0 or fewer eggs to 10 eggs
        n_hatchlings[which(n_hatchlings <= 0)] <- min_nest_size
        
        for (j in 1:n_sims) {
          
          # if mixed dominant, extract contributions
          if (fertilization_mode == 'mixed_dominant') { contributions <- probs[j, ]}
          
          # make nest with i fathers
          nest <- sample(x = 1:m, 
                         size = n_hatchlings[j], 
                         replace = TRUE,
                         prob = contributions)
          # }
          
          # take sample of size j from nest (or the whole nest if < j)
          sample_size <- min(n_sizes[s], length(nest))
          samples <- sample(x = nest, 
                            size = sample_size,
                            replace = FALSE)
          
          # how many males were identified
          males_identified[j] <- length(unique(samples))
          # estimate[k] <- length(unique(samples))
          
        }
        
        # # for troubleshooting, if needed
        # calculate index in data frame
        # index <- (i - 2)*(max(n_sizes)) + j
        # print(index)
        
        # probabilities for each # of males
        props <- rep(NA, m)
        
        for (k in 1:m) {
          
          props[k] <- round(length(males_identified[males_identified == k]) / 
                              length(males_identified), digits = 4)
          
        }
        
        # pre-allocate data frame
        DF2 <- data.frame(Fertilization_mode = rep(fertilization_modes[f], times = m),
                          Males_contributing = rep(m, times = m),
                          Sample_size = rep(n_sizes[s], times = m),
                          Males_identified = 1:m,
                          Probability = props)
        
        # attach to DF
        DF <- rbind(DF, DF2)
        
      }
      
    }
    
  }
  
  # remove row of NAs at the top
  number_of_males <- DF[-1, ]
  
  # save object
  save(number_of_males, 
       file = 'number_of_males.Rdata')
  
}
