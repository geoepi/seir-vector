plot_sample_trajectories <- function(stanmodel, truth_df, variable = "pred_infected", 
                                     n_draws = 100, ymax_prob = 0.975) {
  
  pred_df <- as.data.frame(rstan::extract(stanmodel, pars = variable)[[variable]])
  
  sampled_indices <- sample(nrow(pred_df), n_draws, replace = TRUE)
  sampled_trajectories <- pred_df[sampled_indices, ]
  
  sampled_trajectories$max_values <- apply(sampled_trajectories, 1, max)
  
  traj_df_long <- reshape2::melt(sampled_trajectories, "max_values", variable.name = 'time_step', value.name = 'infected')
  
  traj_df_long$time_step <- as.numeric(gsub("V", "", traj_df_long$time_step))
  
  traj_df_long$sample_id <- rep(seq_len(n_draws), nrow(traj_df_long)/n_draws)
  
  color_palette <- scales::hue_pal()(n_draws) 
  
  ymax <- quantile(traj_df_long$infected, probs = ymax_prob)
  
  p <- ggplot(traj_df_long, aes(x = time_step, y = infected, group = sample_id)) +
    geom_line(alpha = 0.2, aes(color = factor(sample_id))) + 
    labs(title = " ",
         x = "Time Step",
         y = "Infected Individuals") +
    scale_colour_manual(values=color_palette) +
    geom_point(data = truth_df,
               aes(doy, count, group=NA),
               col="black", shape=1, size=2) +
    theme_minimal() + ylim(0, ymax) +
    theme(plot.margin = unit(c(0.5, 0.2, 0.5, 0.2), "cm"),
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
  
  return(p)
}
