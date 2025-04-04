nM = 40
n_sims = 1
clutches_mu <- 4.95                              
clutches_sd <- 2.09

set.seed(seed = 7)

output <- stats::rnorm(n = nM*n_sims,
             mean = clutches_mu,
             sd = clutches_sd)

sum(output)

length(output)


