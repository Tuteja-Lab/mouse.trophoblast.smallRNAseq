theme_clean <- function() {
  font.1 <- 'sans'
  font.2 <- 'sans'
  font.3 <- 'sans'
  theme_classic() %+replace% 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        family = font.1,
        size = 12,
        face = 'bold',
        hjust = 0,
        vjust = 2
      ),
      plot.subtitle = element_text(
        family = font.1,
        size = 10),
      plot.caption = element_text(
        family = font.1,
        size = 10,
        hjust = 1
      ),
      plot.margin = margin(
        t = 1,
        r = 1,
        b = 1,
        l = 2,
        unit = "cm"
      ),
      axis.title = element_text(
        family = font.1,
        size = 10,
        face = 'bold'
      ),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      axis.text = element_text(family = font.1,
                               size = 12),
      axis.text.x = element_text(
        margin = margin(5, b = 10),
        size = 10, angle = 0),
      strip.text = element_text(
        family = font.1,
        face = "bold",
        color = "gray35",
        hjust = 0,
        size = 10
      ),
      strip.background = element_rect(fill = "white",
                                      linetype = "blank"),
      legend.title = element_text(
        family = font.1,
        size = 12,
        color = "black",
        face = "bold"
      ),
      legend.text = element_text(
        family = font.1,
        size = 12,
        color = "black",
        face = "plain"
      ),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      ),
      legend.key = element_blank(),
      legend.position="none",
    )
}