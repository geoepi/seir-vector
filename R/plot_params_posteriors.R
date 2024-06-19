plot_params_posteriors <- function(stanmodel, ret_R0 = TRUE, pars = c("beta_h", "beta_v", "rho_h", "rho_v", "mu_v", 
                                                                        "gamma_h", "upsilon_h", "upsilon_v", "duration", "incub_h", "incub_v")) {
  
  param_df <- as.data.frame(rstan::extract(stanmodel, pars = pars))
  
  if(ret_R0 == TRUE){
    param_df$R0_h <- param_df$beta_h*param_df$upsilon_h/param_df$gamma_h
  }
  
  param_df_long <- reshape2::melt(param_df, variable.name = 'parameter')
  
  p <- ggplot(param_df_long, aes(x = value)) + 
    geom_density(alpha = 0.75, fill = "orange") + 
    labs(title = " ",
         x = "Parameter Estimate",
         y = "Posterior Density") +
    theme_minimal() +
    coord_flip() +
    facet_wrap(~parameter, scales = "free", ncol = 3) +
    theme_minimal() +
    theme(plot.margin = unit(c(0.5, 0.2, 0.5, 0.2), "cm"),
          legend.direction = "horizontal",
          legend.position = "none", 
          strip.text = element_text(size = 13, face = "bold"),
          strip.background = element_blank(),
          legend.key.size = unit(2, "line"),
          legend.key.width = unit(3, "line"),
          legend.text = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 18, face = "bold"),
          axis.title.x = element_text(size = 24, face = "bold"),
          axis.title.y = element_text(size = 18, face = "bold"),
          axis.text.x = element_text(face = "bold", size = 14, vjust = 0.5, 
                                     hjust = 0.5, angle = 0),
          axis.text.y = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 22, face = "bold"))
  
  # medians
  medians <- param_df_long %>%
    group_by(parameter) %>%
    summarise(median_value = median(value))
  
  # vertical lines for medians
  p <- p + geom_vline(data = medians, aes(xintercept = median_value),
                      color = "blue", linetype = "dashed", linewidth = 1) +
    geom_text(data = medians, aes(x = median_value, y = Inf, label = round(median_value, 2)),
              color = "blue", vjust = -0.5, hjust = 1.5)
  
  return(p)
}