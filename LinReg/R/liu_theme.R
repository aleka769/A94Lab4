#' Linkoping University theme, with official LiU colors.
#' @importFrom ggplot2 theme_bw theme element_line element_rect element_blank element_text margin
#' 
#' @return A ggplot theme
#' @export
theme_liu <- function(){
  
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "#16C7D2", 
                                                          linetype = 3), 
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(), 
                 plot.background = ggplot2::element_rect(),
                 panel.border = ggplot2::element_blank(),
                 text = ggplot2::element_text(color = "#16C7D2"),
                 axis.text = ggplot2::element_text(color = "#16C7D2"),
                 axis.ticks = ggplot2::element_line(color = "#16C7D2"),
                 axis.line.x.bottom = ggplot2::element_line(color = "#16C7D2", 
                                                            arrow = grid::arrow(length = grid::unit(0.1, "inches"), type = "closed")),
                 axis.line.y.left = ggplot2::element_line(color = "#16C7D2", 
                                                          arrow = grid::arrow(length = grid::unit(0.1, "inches"), type = "closed")),
                 axis.title.y = ggplot2::element_text(angle = 0),
                 axis.title.x = ggplot2::element_text(hjust = 1),
                 plot.margin= ggplot2::margin(5,5,30,5))
  
}

# rgb(22, 199, 210, maxColorValue = 255)
# rgb(124, 236, 241, maxColorValue = 255)
