plot_params_range <- function(stanmodel_1, stanmodel_2) {
  
  # Extract and process
  process_model_data <- function(stanmodel, model_name) {

    param_df <- as.data.frame(extract(stanmodel, pars = c("duration", "incub_h", "incub_v")))
    
    # New names and order
    new_names <- c(incub_v = "Incubation (V)", incub_h = "Incubation (H)", duration = "Recovery")
    
    ordered_names <- rev(c("Incubation (V)", "Incubation (H)", "Recovery"))
    
    # Calculate credible intervals
    cred_intervals_list <- lapply(names(new_names), function(parameter_name) {
      param_data <- param_df[[parameter_name]]
      quantiles <- quantile(param_data, probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99))
      tibble::tibble(
        parameter = new_names[parameter_name],
        lower_95 = quantiles['1%'],
        lower_90 = quantiles['5%'],
        lower_50 = quantiles['25%'],
        median = quantiles['50%'],
        upper_50 = quantiles['75%'],
        upper_90 = quantiles['95%'],
        upper_95 = quantiles['99%'],
        factor_parameter = factor(new_names[parameter_name], levels = ordered_names),
        model = factor(model_name, levels = c('2014', '2015')) 
      )
    })
    
    dplyr::bind_rows(cred_intervals_list)
  }
  
  # Process both models
  cred_intervals_df_1 <- process_model_data(stanmodel_1, '2014')
  cred_intervals_df_2 <- process_model_data(stanmodel_2, '2015')
  
  # Combine models
  cred_intervals_df <- rbind(cred_intervals_df_1, cred_intervals_df_2)
    
  # Plotting
  dodge_width <- 0.75
  
  cred_intervals_df$parameter <- ordered(factor(cred_intervals_df$parameter), ordered_names)
  cred_intervals_df$model <- ordered(factor(cred_intervals_df$model), c('2015', '2014'))
  
  p <- ggplot(data = cred_intervals_df, aes(x = factor_parameter, y = median, color = model)) +
    geom_linerange(aes(ymin = lower_95, ymax = upper_95), size = 2.5, alpha=0.5,
                   position = position_dodge(width = dodge_width)) +
    geom_linerange(aes(ymin = lower_90, ymax = upper_90), size = 3.5, alpha=0.5,
                   position = position_dodge(width = dodge_width)) +
    geom_linerange(aes(ymin = lower_50, ymax = upper_50), size = 4.5, alpha=0.5,
                   position = position_dodge(width = dodge_width)) +
    geom_point(size = 6, aes(color = model), 
               position = position_dodge(width = dodge_width)) + 
    # scale_y_continuous(breaks = seq(0, 20, by = 2), limits = c(0, 20)) +
    scale_color_manual(values = c('2014' = 'steelblue', '2015' = 'darkorange')) +
    labs(title = " ",
         color = "Year",
         x = "",
         y = "Duration (Days)") +
    theme_minimal() +
    coord_flip() +
    facet_wrap(~rev(factor_parameter), ncol = 1, scales="free") +
    theme(plot.margin = unit(c(1,0.75,1,0.75),"cm"),
          legend.direction = "horizontal",
          legend.position="bottom",
          strip.text = element_blank(), #element_text(size=18, face="bold"),
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size=16, face="bold"),
          legend.title = element_text(size=18, face="bold"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.title.y = element_text(size=22, face="bold"),
          axis.text.x = element_text(face="bold", size=15, vjust=0.5, 
                                     hjust=0.5, angle=0),
          axis.text.y = element_text(size=18, face="bold"),
          plot.title = element_text(size=22, face="bold"))
  
  return(p)
}