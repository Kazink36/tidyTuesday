
theme_36 <- function(color = "gray90") {
  theme_minimal() +
    theme(text = element_text(color = color,face = "bold"),
          title = element_text(color = color,face = "bold"),
          panel.grid = element_line(color = color),
          axis.text = element_text(color = color),
          legend.title = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = color),
          plot.title = element_text(hjust = 0.5,color = color,face = "bold"),
          legend.position = c(.9,.1))
}



finish_plot <- function(gg.plot, h = c(190, 210), c = 100, l = 65, deg = 45,n = 500) {
  

  make_gradient <- function(deg = 45, n = 100, cols = blues9) {
    cols <- colorRampPalette(cols)(n + 1)
    rad <- deg / (180 / pi)
    mat <- matrix(
      data = rep(seq(0, 1, length.out = n) * cos(rad), n),
      byrow = TRUE,
      ncol = n
    ) +
      matrix(
        data = rep(seq(0, 1, length.out = n) * sin(rad), n),
        byrow = FALSE,
        ncol = n
      )
    mat <- mat - min(mat)
    mat <- mat / max(mat)
    mat <- 1 + mat * n
    mat <- matrix(data = cols[round(mat)], ncol = n)
    grid::rasterGrob(
      image = mat,
      width = unit(1, "npc"),
      height = unit(1, "npc"), 
      interpolate = TRUE
    )
  }
  x <- scales::hue_pal(h = h, c = c, l = l)
  g <- make_gradient(
    deg = deg, n = n, cols = x(10)
  )
  
  
  grid.newpage()
  grid.draw(g)
  plot(gg.plot, newpage = FALSE)
  
  invisible(gg.plot)
}





