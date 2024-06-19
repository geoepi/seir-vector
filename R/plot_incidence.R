plot_incidence <- function(dataframe) {
  
  index_case <- min(dataframe$date)
  
  sum_cases <- sum(dataframe$count)
  
  year_cases <- unique(dataframe$year)
  
  ylims <- c(0, max(dataframe$count) + 5)
  
  p <- ggplot() + 
    geom_vline(xintercept = index_case,
               linetype = "longdash",
               linewidth=0.7,
               col="gray50") +
    annotate("text", x = index_case, y = ylims[2]/2, label = paste("Index Case"),  
             color = "gray20", angle = 90, vjust = 1.5) +
    geom_bar(data=dataframe, aes(date, count), stat="identity", alpha=1, 
             color="transparent", fill="lightsteelblue", width=1) +
    annotate("text", x = as_date(paste0(year_cases, "-10-20")), y = ylims[2]*0.85, label = paste("Year: ", year_cases),  
             color = "gray20", angle = 0, vjust = 1.5, size=9) +
    annotate("text", x = as_date(paste0(year_cases, "-10-20")), y = ylims[2]*0.85 - 4, label = paste("Total Cases: ", sum_cases),  
             color = "gray20", angle = 0, vjust = 1.5, size=9) +
    scale_x_date(date_breaks = "25 day", date_labels = "%b %d") + 
    xlab("Date") + 
    ylab("Observed Incidence") +
    theme_bw() +
    theme(plot.margin = unit(c(1.5,0.5,1.5,0.5),"cm"),
          legend.direction = "horizontal",
          legend.position="none", 
          strip.text = element_blank(), #element_text(size=18, face="bold"),
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size=16, face="bold"),
          legend.title = element_text(size=18, face="bold"),
          axis.title.x = element_text(size=24, face="bold"),
          axis.title.y = element_text(size=24, face="bold"),
          axis.text.x = element_text(face="bold", size=18, vjust=1, 
                                     hjust=1, angle=45),
          axis.text.y = element_text(size=20, face="bold"),
          plot.title = element_text(size=22, face="bold"))
  
  return(p)
}