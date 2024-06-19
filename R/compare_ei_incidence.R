compare_ei_incidence <- function(out_df, gamma_h) {

  out_df$s_lag <- -1 * c(0, diff(as.numeric(out_df$Sh)))
  out_df$e_lag <- c(0, diff(as.numeric(out_df$Eh)))
  out_df$ii_from_Sh_Eh <- out_df$e_lag + out_df$s_lag
  
  out_df$ih_lag <- c(0, diff(as.numeric(out_df$Ih)))
  out_df$recovered <- gamma_h * as.numeric(out_df$Ih)
  out_df$ii_from_Ih <- -out_df$ih_lag + out_df$recovered
  
  total_ii_from_Sh_Eh <- sum(out_df$ii_from_Sh_Eh, na.rm = TRUE)
  total_ii_from_Ih <- sum(out_df$ii_from_Ih, na.rm = TRUE)
  
  ratio <- total_ii_from_Ih / total_ii_from_Sh_Eh
  rounded_ratio <- round(ratio, 3)  
  
  incidence_data <- data.frame(
    Time = rep(out_df$time, 2),
    Incidence = c(out_df$ii_from_Sh_Eh, out_df$ii_from_Ih),
    Type = rep(c("Infected", "Symptomatic"), each = nrow(out_df))
  )
  
  label_text <- paste0("Estimated \u03C1 = ", rounded_ratio)
  
  p <- ggplot(incidence_data, aes(x = Time, y = Incidence, color = Type)) +
    geom_line(linewidth = 1) +
    labs(title = " ", x = "Time Step", y = "Incidence") +
    scale_color_manual(values = c("blue", "red")) +
    annotate("text", x = 350, y = 100, label = label_text,
             hjust = 1.1, vjust = 1.1, size = 6, color = "black") +
    theme_bw() +
    theme(plot.margin = unit(c(1,0.5,1,0.5),"cm"),
          legend.direction = "horizontal",
          legend.position="bottom", 
          strip.text = element_blank(), #element_text(size=18, face="bold"),
          strip.background = element_blank(),
          legend.title = element_blank(), 
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size=16, face="bold"),
          axis.title.x = element_text(size=24, face="bold"),
          axis.title.y = element_text(size=24, face="bold"),
          axis.text.x = element_text(face="bold", size=18, vjust=1, 
                                     hjust=1, angle=45),
          axis.text.y = element_text(size=20, face="bold"),
          plot.title = element_text(size=22, face="bold"))
  
  return(p)
}