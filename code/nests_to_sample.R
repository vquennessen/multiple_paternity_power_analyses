# function to determine our confidence in OSR estimate given how many nests we 
# sample to robustly estimate the OSR for that year

#' nests_to_sample
#' 
#' \code{nests_to_sample} Samples nests across a whole breeding season to 
#'    determine if all of the males that contributed were identified. 
#'
#' @param nsims integer value, the number of simulations to run. Default value 
#'    is 1e6.
#' @param pop_size integer value, the population size of all breeding adults. 
#'    Default value is 100.
#' @param sample_size integer value, the sample size to collect. Default value 
#'    is 32. .
#' @param fertilization_mode a character value defining the distribution of 
#'    male contributions to fertilizing a single nest. Potential values 
#'    include random', 'exponential', dominant50', 'dominant70', 
#'    'dominant90', 'mixed_dominant'). Default value is 'random'.
#' @param Mprob a numeric vector, the probabilities of females mating with 1+ 
#'    males. 
#' @param Fprob a numeric vector, the probabilities of males mating with 1+ 
#'    females.
#' @param nests_mu an integer value, the mean number of nests a female lays in 
#'    one nesting season. Default value is 4.95. 
#' @param nests_sd an integer value, the standard deviation of the number of 
#'    nests a female lays in one nesting season. Default value is 2.09. 
#' @param id_probs a dataframe with columns "Fertilization Mode", 
#'    "Males" (number of contributing males), "Sample Size" (1 - 96), 
#'    "Proportion Correct" (how many simulations correctly identified all males), 
#'    and "Marginal" (the marginal fertilization contributions of the last male). 
#'
#' @return
#' @export
#'
#' @examples
#' hatchlings_to_sample(hatchlings_mu = 100.58, hatchlings_sd = 22.61, 
#'                      max_males = 5, nsims = 1e5, n_sizes = c(32, 96), 
#'                      fertilization_mode = 'random')
#' num_males <- number_of_males(hatchlings_mu = 100.58, hatchlings_sd = 22.61, 
#'    max_males = 5, nsims = 1e4, n_sizes = c(32, 96), 
#'    fertilization_modes = c('random', 'exponential', 'dominant50', 
#'                            'dominant70', 'dominant90', 'mixed_dominant'), 
#'    min_nest_size = 10)
#' id_probs <- num_males %>%
#'    filter(Sample_size == sample_size) %>%
#'    filter(Fertilization_mode == fertilization_mode) %>%
#'    select(Males_contributing, Males_identified, Probability)
#' nests_to_sample(nsims = 1e4, pop_size = 100, sample_size = 32, 
#'    fertilization_mode = 'random', 
#'    Mprob = c(0.463, 0.318, 0.157, 0.034, 0.028), 
#'    Fprob = c(1), nests_mu = 4.95, nests_sd = 2.09, id_prob = DF)

nests_to_sample <- function(nsims = 1e5,            
                            pop_size = 100,        
                            sample_size = 32,
                            fertilization_mode = 'random',
                            Mprob,             
                            Fprob,              
                            nests_mu = 4.95,  
                            nests_sd = 2.09,  
                            id_probs)         
  
{
  
  ###### Error handling ########################################################
  
  # classes of variables
  if (nsims %% 1 != 0) {stop('nsims must be an integer value.')}
  if (pop_size %% 1 != 0) {stop('pop_size must be an integer value.')}
  if (sample_size %% 1 != 0) {stop('sample_size must be an integer value.')}
  if (!is.character(fertilization_modes)) 
  {stop('fertilization_modes must be a character.')}
  if (!is.numeric(Mprob)) {stop('Mprob must be a numeric value.')}
  if (!is.numeric(Fprob)) {stop('Fprob must be a numeric value.')}
  if (!is.numeric(nests_mu)) {stop('nests_mu must be a numeric value.')}
  if (!is.numeric(nests_sd)) {stop('nests_sd must be a numeric value.')}
  if (!is.data.frame(id_probs)) {stop('id_probs must be a data frame.')}
  
  # acceptable values
  if (nsims <= 0) {stop('nsims must be greater than 0.')}
  if (pop_size <= 0) {stop('pop_size must be greater than 0.')}
  if (sample_size <= 0) {stop('sample_size must be greater than 0.')}
  if (!(fertilization_mode) %in% c('random', 'exponential', 'dominant50', 
                                   'dominant70', 'dominant90', 
                                   'mixed_dominant'))   
    {stop('fertilization mode given is not recognized.')}
  if (sum(Mprob < 0) > 0) {stop('Mprob values cannot be below zero.')}  
  if (sum(Mprob > 1) > 0) {stop('Mprob values cannot be above 1.')}  
  if (sum(Fprob < 0) > 0) {stop('Fprob values cannot be below zero.')}  
  if (sum(Fprob > 1) > 0) {stop('Fprob values cannot be above 1.')} 
  if (nests_mu <= 0) {stop('nests_mu must be greater than 0.')}
  if (nests_sd <= 0) {stop('nests_sd must be greater than 0.')}
  # if (sum(id_probs < 0) > 0) {stop('id_probs values cannot be below zero.')}  
  # if (sum(id_probs > 1) > 0) {stop('id_probs values cannot be above 1.')} 

  ##############################################################################
  
  # dimensions
  maxM <- length(Mprob) # max number of males a female can mate with
  maxF <- length(Fprob) # max number of females a male can mate with
  
  # operational sex ratios
  OSR <- seq(from = 0.05, to = 0.5, by = 0.05)
  nO <- length(OSR)
  
  # proportion of nests sampled
  propNests <- seq(from = 0.05, to = 1, by = 0.05)
  nPN <- length(propNests)
  
  # pre-allocate data frame for results
  DF2 <- data.frame(OSR = rep(OSR, each = nPN), 
                    PropNests = rep(propNests, times = nO), 
                    Proportion = NA)
  
  # for each OSR population
  for (o in 1:nO) {
    
    # make population of males and females
    nM <- pop_size*OSR[o]
    nF <- pop_size - nM
    
    # # make breeding pool of males
    # BPm <- rep(1:nM, each = maxM)
    
    # for each proportion of nests sampled
    for (pn in 1:nPN) {
      
      # initialize vector of whether or not all males were identified
      ID <- rep(NA, nsims)
      
      # initialize number of nests
      nNests <- matrix(round(rnorm(n = nF*nsims, mean = nests_mu, sd = nests_sd)),
                       nrow = nF, ncol = nsims)
      
      # make sure there aren't any negative or 0 nests
      nNests[nNests < 1] <- 1
      
      # initialize number of males
      nMales <- matrix(sample(1:maxM, 
                              size = nF*nsims, 
                              prob = Mprob, 
                              replace = TRUE), 
                       nrow = nF, ncol = nsims)
      
      # proportion of nests sampled
      prop <- propNests[pn]
      
      # for each simulation
      for (i in 1:nsims) {
        
        # how many females per male
        nFemales <- sample(1:maxF, size = nM, prob = Fprob, replace = TRUE)
        
        # make breeding pool of males
        BPm <- rep(1:nM, times = nFemales)
        
        # initialize nests list
        nests <- NA
        
        # for each female
        for (f in 1:nF) {
          
          # how many nests for this female
          nN_f <- nNests[f, i]
          
          # how many males for this female
          nM_f <- nMales[f, i]
          
          # if there are no males left, stop the loop for the simulation    
          if (n_distinct(na.omit(BPm)) == 0) { break; break }
          
          # if there are not enough unique males left in the breeding pool 
          # for this female
          if (n_distinct(BPm) < nM_f) {
            
            # change the number of males with however many unique males are left
            nM_f <- n_distinct(BPm)
            
          }
          
          # who are the contributing males themselves, sample from breeding pool 
          # without duplicates
          males_f <- sample(unique(BPm), size = nM_f, replace = FALSE)
          
          # new breeding pool for males
          BPm <- BPm[-match(males_f, BPm)]
          
          # if there's only 1 male
          if (nM_f == 1) {
            
            # append identified male to nests nN_f times
            nests <- append(nests, rep(list(males_f), times = nN_f))
            
          } else {
            
            # probability of identification of all possible males for this female
            sub <- subset(id_probs, Males_contributing == nM_f & Probability > 0)
            
            # if there's only one possible number of males identified for any nest
            if (nrow(sub) == 1) {
              
              nests <- append(nests, rep(list(males_f), times = nN_f))
              
            } else {
              
              # how many males were identified in each nest for this female?
              nM_id <- sample(sub$Males_identified,
                              size = nN_f,
                              prob = sub$Probability,
                              replace = TRUE)
              
              # if there's only 1 nest
              if (nN_f == 1) {
                
                nests <- append(nests, list(sample(males_f,
                                                   size = nM_id,
                                                   replace = TRUE)))
                
              } else {
                
                for (n in 1:nN_f) {
                  
                  nests <- append(nests, list(sample(males_f,
                                                     size = nM_id[n],
                                                     replace = FALSE)))
                  
                }
                
              }
              
            }
            
          }
          
        }
        
        # remove NA from nests
        nests <- nests[-1]
        
        # number of males actually represented across all nests, i.e. breeding
        num_males <- n_distinct(unlist(nests))
        
        # number of nests total
        num_nests <- length(nests)
        
        # how many nests were sampled
        num_nests_sampled <- round(num_nests*prop)
        
        # if no nests end up getting sampled, sample 1 nest
        if (num_nests_sampled < 1) { num_nests_sampled <- 1 }
        
        # sample all possible nests for proportion
        indices <- sample(1:num_nests, 
                          size = num_nests_sampled, 
                          replace = FALSE)
        
        # WHICH males were identified, add to identified males vector
        identified_males <- unlist(nests[indices])
        
        # were all males identified?
        ID[i] <- ifelse(n_distinct(identified_males) == as.integer(num_males), 1, 0)
        
      }
      
      # calculate index
      index <- (o - 1)*nPN + pn
      
      # write to progress text file
      if ((index/2) %% 10 == 0) {
        update <- paste(Sys.time(), 
                        ' - sample size ', sample_size, ' - ', 
                        fertilization_mode, ' - ', 
                        nsims, ' sims - ', index/2, '% done!', 
                        sep = '')
        write(update, file = 'progress.txt', append = TRUE)
        
      }
      
      # proportion of simulations where all males were identified
      all_males_ID <- mean(ID, na.rm = TRUE)
      
      # add ID to dataframe
      DF2$Proportion[index] <- all_males_ID
      
    }
    
  }
  
  # return output
  return(DF2)
  
}