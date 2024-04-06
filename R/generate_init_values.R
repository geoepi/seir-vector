generate_init_values <- function(num_chains) {
  init_list <- vector("list", num_chains)
  
  for (i in 1:num_chains) {
    init_list[[i]] <- list(
      beta_h = rlnorm(1, log(0.2), 0.1),
      gamma_h = rlnorm(1, log(0.085), 0.1),
      sigma_h = rlnorm(1, log(0.15), 0.1),                         
      sigma_v = rlnorm(1, log(0.3), 0.1),
      rho_v = rbeta(1, 10, 500),
      beta_v = rlnorm(1, log(0.12), 0.1),
      mu_v = rlnorm(1, log(0.35), 0.1),
      obs_bias = rbeta(1, 10, 100),                         
      i_0 = runif(1, 1, 2),
      e_0 = runif(1, 1, 2),
      ev_0 = runif(1, 1, 2),
      iv_0 = runif(1, 1, 2)
    )
  }
  
  return(init_list)
}