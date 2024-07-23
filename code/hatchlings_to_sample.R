#' hatchlings_to_sample
#'
#' \code{hatchlings_to_sample}  samples hatchlings from nests to determine the
#'    confidence of identifying all of the males that contributed for 
#'    populations that exhibit multiple paternity. 
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
#' @param fertilization_mode a character value defining the distribution of 
#'    male contributions to fertilizing a single nest. Potential values 
#'    include random', 'exponential', dominant50', 'dominant70', 
#'    'dominant90', 'mixed_dominant'). 
#'
#' @return creates and saves figures to plot confidence of identifying all males
#'    given different numbers of contributing males and sample sizes. Creates 
#'    and saves table of confidences for sample sizes of n_sizes specifically.
#' @export
#'
#' @examples
#' hatchlings_to_sample(hatchlings_mu = 100.58, hatchlings_sd = 22.61, 
#'                      max_males = 5, n_sims = 1e5, n_sizes = c(32, 96), 
#'                      fertilization_mode = 'random')
#' 
hatchlings_to_sample <- function(hatchlings_mu,
                                 hatchlings_sd,      # number of eggs per nest
                                 max_males,          # max # of M F can mate with
                                 n_sims,             # number of simulations to run
                                 n_sizes,            # sample sizes to run  
                                 fertilization_mode) # fertilization mode
  
{
  
  ###### Error handling ########################################################
  
  # classes of variables
  if (!is.numeric(hatchlings_mu)) {stop('hatchlings_mu must be a numeric value.')}
  if (!is.numeric(hatchlings_sd)) {stop('hatchlings_sd must be a numeric value.')}
  if (max_males %% 1 != 0) {stop('max_males must be an integer value.')}
  if (n_sims %% 1 != 0) {stop('n_sims must be an integer value.')}
  if (n_sizes %% 1 != 0) {stop('n_sizes must be an integer value.')}
  if (!is.character(fertilization_modes)) 
  {stop('fertilization_modes must be a character.')}
  
  # acceptable values
  if (hatchlings_mu <= 0) {stop('hatchlings_mu must be greater than 0.')}
  if (hatchlings_sd <= 0) {stop('hatchlings_sd must be greater than 0.')}
  if (max_males < 2) {stop('max_males must be greater than 1.')}
  if (n_sims <= 0) {stop('n_sims must be greater than 0.')}
  if (n_sizes <= 0) {stop('n_sizes must be greater than 0.')}
  if (n_sizes > 96) {stop('n_sizes must be less than 97.')}
  if (!(fertilization_mode) %in% c('random', 'exponential', 'dominant50', 
                                   'dominant70', 'dominant90', 
                                   'mixed_dominant')) 
  {stop('fertilization_mode given is not recognized')}
  
  ##############################################################################
  
  
  # pre-allocate data frame
  DF <- data.frame(Fertilization_mode = fertilization_mode,
                   Males = rep(2:max_males, each = max(n_sizes)), 
                   Sample_size = rep(c(1:max(n_sizes)), times = (max_males - 1)), 
                   Proportion_correct = rep(NA, dim = (max_males - 1)*(max(n_sizes) - 1))) 
  
  # for each number of males that contribute to a nest:
  for (i in 2:max_males) {
    
    # set contributions per males
    if (fertilization_mode == 'dominant90') {
      MC <- 0.90
      contributions <- c(MC, rep((1 - MC)/(i - 1), (i - 1)))
      title <- paste('Dominant (90%) fertilization mode', sep = '')
      
    } else if (fertilization_mode == 'dominant70') {
      MC <- 0.70
      contributions <- c(MC, rep((1 - MC)/(i - 1), (i - 1)))
      title <- paste('Dominant (70%) fertilization mode', sep = '')
      
    } else if (fertilization_mode == 'dominant50') {
      MC <- 0.50
      contributions <- c(MC, rep((1 - MC)/(i - 1), (i - 1)))
      title <- paste('Dominant (50%) fertilization mode', sep = '')
      
    } else if (fertilization_mode == 'exponential') {
      MC <- 0.5
      contributions <- 0.5^c(1:(i-1))
      contributions <- c(contributions, contributions[i-1])
      title <- 'Exponential (1/2) fertilization mode'
      
    } else if (fertilization_mode == 'random') {
      contributions <- rep(1/i, i)
      title <- 'Random fertilization mode'
      
    } else if (fertilization_mode == 'mixed_dominant') {
      doms <- sample(c(0.50, 0.70, 0.90), size = n_sims, replace = TRUE)
      M1 <- matrix(doms, nrow = n_sims, ncol = 1)
      M2 <- matrix(rep((1 - doms) / (i - 1), i - 1), nrow = n_sims, ncol = i - 1)
      probs <- cbind(M1, M2)
      title <- 'Mixed dominant fertilization mode'
      
    }
    
    # proportion_correct array
    prop_correct <- rep(NA, n_sims)
    
    # for each sample size
    for (j in 1:max(n_sizes)) {
      
      # pre-allocate correct identifications of number of males
      correct <- rep(NA, n_sims)
      # estimate <- rep(NA, n_sims)
      
      # initialize nest sizes
      # pull numbers of hatchlings from normal distribution
      n_hatchlings <- rnorm(n = n_sims, mean = hatchlings_mu, sd = hatchlings_sd)
      
      # set any nests with 0 or fewer eggs to 10 eggs
      n_hatchlings[which(n_hatchlings <= 0)] <- 10
      
      for (k in 1:n_sims) {
        
        # if mixed dominant, extract contributions
        if (fertilization_mode == 'mixed_dominant') { contributions <- probs[k, ]}
        
        # make nest with i fathers
        nest <- sample(x = 1:i, 
                       size = n_hatchlings[k], 
                       replace = TRUE,
                       prob = contributions)
        # }
        
        # take sample of size j from nest (or the whole nest if < j)
        sample_size <- min(j, length(nest))
        samples <- sample(x = nest, 
                          size = sample_size,
                          replace = FALSE)
        
        # correct allocation of number of males?
        correct[k] <- length(unique(samples)) == i
        # estimate[k] <- length(unique(samples))
        
      }
      
      # calculate index in data frame
      index <- (i - 2)*(max(n_sizes)) + j 
      # print(index)
      
      # stick proportion in data frame
      DF$Proportion_correct[index] <- mean(correct)
      # DF$Avg_detected[index] <- mean(estimate)
      
      # grab column means of probs for dominant mixed fertilization
      if (fertilization_mode == 'mixed_dominant') {
        contributions2 <- colMeans(probs)
      } else { contributions2 <- contributions }
      
      # marginal contribution of last male
      DF$Marginal[index] <- contributions2[i]
      
    }
    
  }
  
  #### plot results
  
  # color-blind friendly color palette
  colors <- viridis(max_males)
  
  # plot results - proportion correct
  fig1 <- ggplot(DF, aes(x = Sample_size, y = Proportion_correct, 
                         col = as.factor(Males))) +
    geom_hline(yintercept = 0.8, linetype = 2) +
    geom_path(lwd = 1) +
    labs(col = 'Number \n of Males') +
    scale_color_manual(values = colors) +
    ylab('Proportion Correct') +
    xlab('Hatchlings Sampled') +
    geom_vline(xintercept = c(n_sizes), linetype = 3) +
    ggtitle(title)
  
  # save plot
  ggsave(plot = fig1, 
         filename = paste(fertilization_mode, '_fig1_proportion_correct.png', sep = ''),
         path = paste('~/multiple_paternity_power_analyses/figures', sep = ''),
         width = 6, height = 4)
  
  # What's our confidence if we sample 32 percent of the eggs?
  DFsamples <- DF %>% filter(Sample_size %in% n_sizes)
  DFsamples2 <- DFsamples %>% spread(Sample_size, Proportion_correct)
  
  # save confidence table
  png(filename = paste('~/multiple_paternity_power_analyses/', 
                       fertilization_mode, '_conf_table.png', sep = ''), 
      width = 200, height = 200)
  grid.table(DFsamples, rows = NULL)
  dev.off()
  
  # save plot
  ggsave(plot = fig1, 
         filename = paste(fertilization_mode, '_fig1_proportion_correct.png', sep = ''),
         path = '~/multiple_paternity_power_analyses/figures',
         width = 6, height = 4)
  
  # What's our confidence if we sample 32 percent of the eggs?
  DFsamples <- DF %>% filter(Sample_size %in% n_sizes)
  DFsamples2 <- DFsamples %>% spread(Sample_size, Proportion_correct)
  
  # save confidence table
  png(filename = paste('~/multiple_paternity_power_analyses/data/', 
                       fertilization_mode, '_conf_table.png', sep = ''), 
      width = 200, height = 200)
  grid.table(DFsamples, rows = NULL)
  dev.off()
  
  # what will the code produce
  output <- list(fig1, DF, DFsamples, DFsamples2)
  
  return(output)
  
}
