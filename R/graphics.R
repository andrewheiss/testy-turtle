# https://carto.com/carto-colors/
clrs <- list(Prism = rcartocolor::carto_pal(n = 12, "Prism"),
             PurpOr = rcartocolor::carto_pal(7, "PurpOr"),
             Emrld = rcartocolor::carto_pal(7, "Emrld"),
             Teal = rcartocolor::carto_pal(7, "Teal"),
             Peach = rcartocolor::carto_pal(7, "Peach"),
             Sunset = rcartocolor::carto_pal(7, "Sunset"))

set_annotation_fonts <- function() {
  ggplot2::update_geom_defaults("label", list(family = "Inter", face = "plain"))
  ggplot2::update_geom_defaults("text", list(family = "Inter", face = "plain"))
}

theme_donors <- function(base_size = 11, base_family = "Inter", prior = FALSE) {
  ret <- theme_bw(base_size, base_family) +
    theme(panel.background = element_rect(fill = "#ffffff", colour = NA),
          plot.title = element_text(size = rel(1.1), vjust = 1.2,
                               family = "Inter", face = "bold"),
          plot.subtitle = element_text(size = rel(0.8),
                                       family = "Inter", face = "plain"),
          plot.caption = element_text(margin = margin(t = 10), size = rel(0.6),
                                      family = "Inter", face = "plain"),
          panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.15),
          panel.spacing = unit(1, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = rel(0.8),
                                    family = "Inter", face = "bold"),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          legend.position = "bottom",
          legend.title = element_text(size = rel(0.8)),
          legend.key.size = unit(0.7, "line"),
          legend.key = element_blank(),
          legend.spacing = unit(0.1, "lines"),
          legend.margin = margin(t = 0),
          strip.text = element_text(size = rel(0.9), hjust = 0,
                                    family = "Inter", face = "bold"),
          strip.background = element_rect(fill = "white", colour = NA))
  
  if (prior) {
    ret <- ret +
      theme(panel.grid.major = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            panel.border = element_blank())
  } else {
    ret
  }
}

theme_donors_map <- function(base_size = 9, base_family = "Inter") {
  ret <- theme_void(base_size, base_family) +
    theme(plot.background = element_rect(fill = "#ffffff", colour = NA),
          legend.position = "bottom")
  
  ret
}

#' Convert mms to pts
#' 
#' Convert units specified in millimeters to typographic points. This is especially helpful when working with ggplot geoms that use size parameters
#'
#' @param x a numeric value (in millimeters)
#'
#' @return A numeric value (in points)
#' @export
#'
#' @examples
#' library(ggplot2)
#' 
#' ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   annotate(geom = "text", x = 20, y = 4, 
#'            label = "Here's a label", size = pts(11))
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, "pt"), "mm"))
}

# Functions for the sparkchart bar chart things in the results tables
spark_bar <- function(df) {
  ggplot(df, aes(x = pd, y = "")) +
    geom_col(fill = "grey20") +
    geom_text(aes(label = pd_nice), hjust = 1.4, color = "white", 
              size = 6, fontface = "bold") +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme_void() +
    theme(panel.background = element_rect(fill = "grey90", linewidth = 0))
}

save_sparks <- function(gg, name) {
  width <- 4
  height <- 0.45
  
  ggsave(glue("{name}.pdf"), gg, 
         width = width, height = height,
         device = cairo_pdf)
  
  ggsave(glue("{name}.png"), gg, 
         width = width, height = height,
         res = 300, device = ragg::agg_png)
  
  return(c(pdf = glue("{name}.pdf"), png = glue("{name}.png")))
}
