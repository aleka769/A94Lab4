theme_liu <- function(data){
  
  # Need to change to aes_string()
  
  p <- ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Width, color = Species)) +
    annotation_custom(grid::rasterGrob(test)) +
    geom_point() +
    theme_bw() +
    theme(panel.grid.major = element_line(color = "#16C7D2", 
                                          linetype = 3),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(), 
          plot.background = element_rect(),
          panel.border = element_blank(),
          text = element_text(color = "#16C7D2"),
          axis.text = element_text(color = "#16C7D2"),
          axis.ticks = element_line(color = "#16C7D2"),
          axis.line.x.bottom = element_line(color = "#16C7D2", 
                                            arrow = arrow(length = unit(0.1, "inches"), type = "closed")),
          axis.line.y.left = element_line(color = "#16C7D2", 
                                          arrow = arrow(length = unit(0.1, "inches"), type = "closed")),
          axis.title.y = element_text(angle = 0),
          axis.title.x = element_text(hjust = 1),
          plot.margin=margin(5,5,30,5)) +
    labs(title = "Linköpings universitet")
  
}

# rgb(22, 199, 210, maxColorValue = 255)
# rgb(124, 236, 241, maxColorValue = 255)
