#___________________________________________________________________________####
#   Styles / Themes                                                         ####

colors <- list(
  grey = "#f0f0f0",
  green = list(
    low = "#f7fcb9",
    medium = "#addd8e",
    high = "#31a354"
  ),
  orange = list(
    low = "#fff7bc",
    medium = "#fec44f",
    high = "#d95f0e"
  ),
  blue = list(
    low = "#ece7f2",
    medium = "#a6bddb",
    high = "#0f4a74" # "#143c63" # "#1c5a88" # "#2b8cbe"
  ),
  red = list(
    low = "#fee0d2",
    medium = "#fc9272",
    high = "#990f11" # "#7f1011" # "#a31918" # "#de2d26"
  )
)


fig_save_args <- list(
  units = "px",
  height = 1920,
  width = 1080
)


theme_custom <- function(base_family = "Helvetica",
                         base_size = 12,
                         padding = base_size*.75,
                         title_size = base_size*1.25,
                         show_legend_title = T) {
  out <- theme_bw() %+replace%
    theme(
      # Add the code below to theme() in the theme_custom() function
      panel.grid = element_line(color = "#C8C8C8",
                                size = .5,
                                linetype = "dashed"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      strip.text = element_text(color = "#ffffff",
                                margin = margin(t = 0.5*padding, b = 0.5*padding)),
      strip.background = element_rect(fill = "#003560")
    )
  
  if(!show_legend_title) {
    out <- out %+replace%
      theme(
        legend.title = element_blank()
      )
  }
  
  out
}