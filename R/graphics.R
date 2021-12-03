update_geom_defaults("bar", list(fill = "grey30"))
update_geom_defaults("line", list(colour = "grey30"))
update_geom_defaults("label", list(family = "Noto Sans", face = "plain"))
update_geom_defaults("text", list(family = "Noto Sans", face = "plain"))
# update_geom_defaults("label_repel", list(family = "Work Sans", face = "plain))
# update_geom_defaults("text_repel", list(family = "Work Sans", face = "plain))

# model_colors <- c("#1f78b4",  # Dark blue
#                   "#a6cee3",  # Light blue
#                   "#33a02c",  # Dark green
#                   "#b2df8a",  # Light green
#                   "#ff7f00")  # Dark orange
# 
# channel_colors <- c("#fdbf6f",  # Light orange
#                     "#1f78b4",  # Dark blue
#                     "#e31a1c")  # Red
# 
# barrier_colors <- c("#1b9e77",  # Turquoise
#                     "#d95f02",  # Orange
#                     "#7570b3",  # Purple
#                     "#e7298a")  # Pink
# 
# burden_colors <- c("#ff7f00",  # Dark orange
#                    "#1f78b4")  # Dark blue
# 
# simulation_individual <- "#DB9E36"  # Light orange
# simulation_mean <- "#BD4932"  # Burnt orange

model_colors <- c("black",
                  "grey50",
                  "grey80")

channel_colors <- c("grey70",
                    "grey40",
                    "black")

barrier_colors <- c("black",
                    "grey60",
                    "black",
                    "grey60")

burden_colors <- c("black",
                   "grey60")

simulation_individual <- "grey30"
simulation_mean <- "black"


# Save Cairo PDF and PNG at the same time
fig_save_cairo <- function(fig, filepath = here("analysis", "output"), 
                           filename, width, height, units = "in", ...) {
  ggsave(fig, filename = file.path(filepath, paste0(filename, ".pdf")),
         width = width, height = height, units = units, device = cairo_pdf, ...)
  ggsave(fig, filename = file.path(filepath, paste0(filename, ".png")),
         width = width, height = height, units = units, type = "cairo", dpi = 300, ...)
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

theme_donors <- function(base_size = 11, base_family = "Noto Sans", prior = FALSE) {
  ret <- theme_bw(base_size, base_family) +
    theme(panel.background = element_rect(fill = "#ffffff", colour = NA),
          title = element_text(size = rel(1.1), vjust = 1.2, 
                               family = "Noto Sans", face = "bold"),
          plot.subtitle = element_text(size = rel(0.8), 
                                       family = "Noto Sans", face = "plain"),
          plot.caption = element_text(margin = margin(t = 10), size = rel(0.6),
                                      family = "Noto Sans", face = "plain"),
          panel.border = element_rect(color = "grey50", fill = NA, size = 0.15),
          panel.spacing = unit(1, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.25, colour = "grey90"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = rel(0.8), 
                                    family = "Noto Sans", face = "bold"),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          legend.position = "bottom",
          legend.title = element_text(size = rel(0.8)),
          legend.key.size = unit(0.7, "line"),
          legend.key = element_blank(),
          legend.spacing = unit(0.1, "lines"),
          strip.text = element_text(size = rel(0.9), hjust = 0,
                                    family = "Noto Sans", face = "bold"),
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

theme_donors_map <- function(base_size = 9, base_family = "Noto Sans") {
  ret <- theme_void(base_size, base_family) +
    theme(legend.position = "bottom")
  
  ret
}

labs_exp_logged <- function(brks) {
  latex2exp::TeX(paste0("e^{", as.character(log(brks)), "}"))
}

labs_exp <- function(brks) {
  latex2exp::TeX(paste0("e^{", as.character(brks), "}"))
}
