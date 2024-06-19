plot_pred_incidence <- function(my_model, my_truth = NULL, 
                                index_offset = 110,
                                xlab = "Time (Days)", ylab = "Predicted Incidence", 
                                pred_loc = "pred_infected") {
  
  model_summary <- summary(my_model, pars = pred_loc, probs = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975))$summary
  
  n_days <- dim(model_summary)[1]
  
  inc_pred_inf <- cbind(
    as.data.frame(model_summary), 
    Time = 1:n_days
  )
  
  colnames(inc_pred_inf) <- make.names(colnames(inc_pred_inf))
  
  if(!is.null(my_truth)){
    
    indx_day <- my_truth[which(my_truth$date == min(my_truth$date)),]$doy
    clear_day <- my_truth[which(my_truth$date == max(my_truth$date)),]$doy
    
    inc_pred_inf$Time <- inc_pred_inf$Time + indx_day - index_offset
  }
  
  p <- ggplot(inc_pred_inf, mapping = aes(x = Time)) +
    geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill = "95% CI"), alpha = 0.2) +
    geom_ribbon(aes(ymin = X5., ymax = X95., fill = "90% CI"), alpha = 0.3) +
    geom_ribbon(aes(ymin = X10., ymax = X90., fill = "80% CI"), alpha = 0.4) +
    geom_ribbon(aes(ymin = X25., ymax = X75., fill = "50% CI"), alpha = 0.5) +
    geom_line(aes(y = X50., color = "Median"), size = 1.5) + 
    scale_fill_manual(values = rep("steelblue", 4),
                      name = "Credible Intervals",
                      breaks = c("95% CI", "90% CI", "80% CI", "50% CI")) +
    scale_color_manual(values = "black", name = "", labels = "Median") +
    guides(fill = guide_legend(override.aes = list(alpha = c(0.2, 0.3, 0.4, 0.5)))) +
    xlim(0, 400) +
    xlab(xlab) + 
    ylab(ylab) +
    theme_bw()
  
  if(!is.null(my_truth)){
    p <- p + geom_point(data=my_truth,
                        aes(x = doy, y = count), shape=1, size=2, col="darkred")
  }
  
  p <- p + theme(plot.margin = unit(c(2, 0.5, 2, 0.5), "cm"),
                 legend.position = "none",
                 strip.text = element_text(size = 18, face = "bold"),
                 strip.background = element_blank(),
                 legend.key.size = unit(1, "line"),
                 legend.key.width = unit(4, "line"),
                 legend.text = element_text(size = 12, face = "bold"),
                 legend.title = element_text(size = 16, face = "bold"),
                 axis.title.x = element_text(size = 25, face = "bold"),
                 axis.title.y = element_text(size = 25, face = "bold"),
                 axis.text.x = element_text(face = "bold", size = 20, 
                                            vjust = 0.5, hjust = 0.5, 
                                            angle = 0),
                 axis.text.y = element_text(size = 22, face = "bold"),
                 plot.title = element_text(size = 22, face = "bold"))
  
  return(p)
}
