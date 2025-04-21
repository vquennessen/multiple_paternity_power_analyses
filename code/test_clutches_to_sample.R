# load libraries
library(dplyr)
library(magrittr)
library(stats)
# library(remotes)
# remotes::install_github(repo = 'vquennessen/MultiplePaternityPowerAnalyses')
# library(MultiplePaternityPowerAnalyses)

load("~/Projects/multiple_paternity_power_analyses/output/probabilities.Rda")

# clutches_to_sample(n_sims = 10,
#                    pop_size = 100,
#                    sample_size = 32,
#                    paternal_contribution_mode = 'random',
#                    Fprob = c(0.463, 0.318, 0.157, 0.034, 0.028),
#                    Mprob = c(1),
#                    clutches_mu = 4.95,
#                    clutches_sd = 2.09,
#                    probs_id = probabilities,
#                    scenario = 'base_F_no_M')

# function parameters
n_sims = 100
pop_size = 100
sample_size = 32
paternal_contribution_mode = 'random'
Fprob <- c(0.2, 0.2, 0.2, 0.2, 0.2)
Mprob <- c(1)
clutches_mu = 4.95
clutches_sd = 2.09
probs_id = probabilities
scenario = 'uniform_F_no_M'

# dimensions
# max number of fathers that can fertilize eggs from a single mother
maxFathers <- length(Fprob)
# max number of mothers whose eggs a single father can fertilize
maxMothers <- length(Mprob)

# operational sex ratios
OSRs <- seq(from = 0.05, to = 0.5, by = 0.05)
nOSR <- length(OSRs)

# proportion of clutches sampled
propClutches <- seq(from = 0.05, to = 1, by = 0.05)
nPC <- length(propClutches)

# pre-allocate data frame for results
DF2 <- data.frame(OSR = rep(OSRs, each = nPC),
                  PropClutches = rep(propClutches, times = nOSR),
                  Proportion = NA)

    # initialize vector of whether or not all fathers were identified
    ID <- array(rep(NA, times = nOSR * nPC * n_sims), 
                dim = c(nOSR, nPC, n_sims))

# for each OSR population
for (osr in 1:nOSR) {
  
  # make population of Fathers and Mothers
  nF <- as.integer(pop_size*OSRs[osr])
  nM <- as.integer(pop_size - nF)
  
  # for each proportion of clutches sampled
  for (pc in 1:nPC) {
    
    # initialize number of clutches based on number of mothers and population
    # parameters
    nClutches <- matrix(round(stats::rnorm(n = nM*n_sims,
                                           mean = clutches_mu,
                                           sd = clutches_sd)),
                        nrow = nM,
                        ncol = n_sims)
    
    # make sure there aren't any negative or 0 clutches, replace with 1 clutch
    nClutches[nClutches < 1] <- 1
    
    # initialize number of fathers for each mother and each simulation
    nFathers <- matrix(sample(1:maxFathers,
                              size = nM*n_sims,
                              prob = Fprob,
                              replace = TRUE),
                       nrow = nM,
                       ncol = n_sims)
    
    # proportion of clutches sampled
    prop <- propClutches[pc]
    
    # for each simulation
    for (i in 1:n_sims) {
      
      # how many mothers eggs are fertilized by each father in the population
      # vector of 1s for populations with no polygyny
      nMothers <- sample(1:maxMothers,
                         size = nF,
                         prob = Mprob,
                         replace = TRUE)
      
      # make breeding pool of fathers
      BPf <- rep(1:nF, times = nMothers)
      
      # initialize clutches list
      clutches <- NA
      
      # for each mother
      for (m in 1:nM) {
                
        # if there are no fathers left, stop the loop for the simulation
        if (dplyr::n_distinct(stats::na.omit(BPf)) == 0) { break; break }
        
        # print(paste('m = ', m, sep = ''))
        # print(BPf)
        
        # how many clutches for this mother
        nC_m <- nClutches[m, i]
        
        # print(paste('number of clutches: ', nClutches[m, i], sep = ''))
        
        # how many fathers for this mother
        nF_m <- nFathers[m, i]
        
        # print(paste('number of fathers (original): ', nFathers[m, i], sep = ''))
      
        # if there are not enough unique fathers left in the breeding pool for
        # this mother
        if (dplyr::n_distinct(BPf) < nF_m) {
          
          # change the number of fathers to how many unique fathers are left
          nF_m <- dplyr::n_distinct(BPf)
          
        }
        
        # print(paste('number of fathers (adjusted): ', nF_m, sep = ''))
        
        # who are the contributing fathers themselves,
        # sample from breeding pool without duplicates
        fathers_m <- sample(unique(BPf),
                            size = nF_m,
                            replace = FALSE)
        
        # print('fathers for this mother: ')
        # print(fathers_m)
        
        # updated breeding pool for fathers minus the ones that already bred
        BPf <- BPf[-match(fathers_m, BPf)]
        
        # if there's only 1 father
        if (nF_m == 1) {
          
          # append identified father to clutches nN_m times, since it will
          # automatically get identified
          clutches <- append(clutches, rep(list(fathers_m), times = nC_m))
          
        } else {
          
          # probability of identification of all possible fathers for this
          # mother, pulled from probs_id data frame given
          sub <- probs_id %>%
            dplyr::filter(Paternal_Contribution_Mode == paternal_contribution_mode, 
                          Fathers_Actual == nF_m,
                          Sample_Size == sample_size,
                          Probability > 0)
          
          # if total fathers are automatically identified for any clutch
          if (nrow(sub) == 1) {
            
            clutches <- append(clutches, rep(list(fathers_m), times = nC_m))
            
          } else {
            
            # how many fathers were identified in each clutch for this mother?
            nF_id <- sample(sub$Fathers_Observed,
                            size = nC_m,
                            prob = sub$Probability,
                            replace = TRUE)
            
            # print('number of fathers identified per clutch:')
            # print(nF_id)
            
            # # if there's only 1 clutch
            # if (nC_m == 1) {
            #   
            #   # add the fathers identified from the clutch
            #   clutches <- append(clutches, list(sample(fathers_m,
            #                                            size = nF_id,
            #                                            replace = FALSE)))
            #   
            # } else {
              
              # for each clutch
              for (n in 1:nC_m) {
                
                # add the fathers identified from the clutches for this mother
                clutches <- append(clutches, list(sample(fathers_m,
                                                         size = nF_id[n],
                                                         replace = FALSE)))
                
              }
              
            # }
            
          }
          
        }

      }
      
      # print(clutches)

      # remove NA from clutches
      clutches <- clutches[-1]
      
      # number of fathers actually represented across all clutches for this
      # population in this simulation
      num_fathers <- dplyr::n_distinct(unlist(clutches))
      
      # number of clutches total
      num_clutches <- length(clutches)
      
      # how many clutches were sampled
      num_clutches_sampled <- round(num_clutches*prop)
      
      # if no clutches end up getting sampled, sample 1 clutch
      if (num_clutches_sampled < 1) { num_clutches_sampled <- 1 }
      
      # sample clutches
      indices <- sample(1:num_clutches,
                        size = num_clutches_sampled,
                        replace = FALSE)
      
      # WHICH fathers were identified, add to identified fathers vector
      identified_fathers <- unlist(clutches[indices])
      
      # were all fathers identified?
      ID[i] <- ifelse(
        dplyr::n_distinct(identified_fathers) == as.integer(num_fathers), 1, 0
      )
      
      
      
    }
    
    # calculate index
    index <- (osr - 1)*nPC + pc
    
    # proportion of simulations where all fathers were identified
    all_fathers_ID <- mean(ID, na.rm = TRUE)
    
    # add ID to dataframe
    DF2$Proportion[index] <- all_fathers_ID
    
    # print progress while running
    update1 <- paste(lubridate::now(), ' - ', scenario, ' - sample size ',
                     sample_size, ' - ', paternal_contribution_mode, ' - ',
                     n_sims, ' sims - OSR ', OSRs[osr], ' - PC ',
                     propClutches[pc], ' - done!', sep = '')
    
    write(update1, file = 'progress.txt', append = TRUE)
    
  }
  
}

random_32 <- DF2
save(random_32, file = 'output/random_32.Rda')
