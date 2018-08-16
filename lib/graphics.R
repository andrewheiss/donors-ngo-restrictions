library(gridExtra)
library(ggstance)

# model.colors <- c("#1f78b4",  # Dark blue
#                   "#a6cee3",  # Light blue
#                   "#33a02c",  # Dark green
#                   "#b2df8a",  # Light green
#                   "#ff7f00")  # Dark orange
# 
# channel.colors <- c("#fdbf6f",  # Light orange
#                     "#1f78b4",  # Dark blue
#                     "#e31a1c")  # Red
# 
# barrier.colors <- c("#1b9e77",  # Turquoise
#                     "#d95f02",  # Orange
#                     "#7570b3",  # Purple
#                     "#e7298a")  # Pink
# 
# burden.colors <- c("#ff7f00",  # Dark orange
#                    "#1f78b4")  # Dark blue
# 
# simulation.individual <- "#DB9E36"  # Light orange
# simulation.mean <- "#BD4932"  # Burnt orange

model.colors <- c("black",
                  "grey50",
                  "grey80")

channel.colors <- c("grey70",
                    "grey40",
                    "black")

barrier.colors <- c("black",
                    "grey60",
                    "black",
                    "grey60")

burden.colors <- c("black",
                   "grey60")

simulation.individual <- "grey30"
simulation.mean <- "black"


# Save Cairo PDF and PNG at the same time
fig.save.cairo <- function(fig, filepath = here("Output"), 
                           filename, width, height, units = "in", ...) {
  ggsave(fig, filename = file.path(filepath, paste0(filename, ".pdf")),
         width = width, height = height, units = units, device = cairo_pdf, ...)
  ggsave(fig, filename = file.path(filepath, paste0(filename, ".png")),
         width = width, height = height, units = units, type = "cairo", dpi = 300, ...)
}

theme_donors <- function(base_size = 9, base_family = "Work Sans Light") {
  update_geom_defaults("bar", list(fill = "grey30"))
  update_geom_defaults("line", list(colour = "grey30"))
  update_geom_defaults("label", list(family = "Work Sans Light", face = "plain"))
  update_geom_defaults("text", list(family = "Work Sans Light", face = "plain"))
  # update_geom_defaults("label_repel", list(family = "Work Sans", face = "plain))
  # update_geom_defaults("text_repel", list(family = "Work Sans", face = "plain))
  
  ret <- theme_bw(base_size, base_family) +
    theme(panel.background = element_rect(fill = "#ffffff", colour = NA),
          title = element_text(size = rel(1.1), vjust = 1.2, 
                               family = "Work Sans Medium", face = "plain"),
          plot.subtitle = element_text(size = rel(0.8), 
                                       family = "Work Sans Light", face = "plain"),
          plot.caption = element_text(margin = margin(t = 10), size = rel(0.6),
                                      family = "Work Sans Light", face = "plain"),
          panel.border = element_rect(color = "grey50", fill = NA, size = 0.15),
          panel.spacing = unit(1, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.25, colour = "grey90"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = rel(0.8), 
                                    family = "Work Sans Medium", face = "plain"),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          legend.position = "bottom",
          legend.title = element_text(size = rel(0.8)),
          legend.key.size = unit(.7, "line"),
          legend.key = element_blank(),
          legend.spacing = unit(0.1, "lines"),
          strip.text = element_text(size = rel(0.9), hjust = 0,
                                    family = "Work Sans Medium", face = "plain"),
          strip.background = element_rect(fill = "#ffffff", colour = NA))
  ret
}

theme_donors_map <- function(base_size = 9, base_family = "Work Sans Light") {
  ret <- theme_void(base_size, base_family) +
    theme(legend.position = "bottom")
  
  ret
}

plot.percent.missing <- function(df, n.cols = 2) {
  percent.missing <- df %>%
    mutate_all(funs(is.na(.))) %>%
    gather(variable, value) %>%
    group_by(variable) %>%
    summarise(perc.missing = sum(value) / n()) %>%
    ungroup() %>%
    mutate(variable = factor(variable, levels = rev(colnames(df)),
                             ordered = TRUE)) %>%
    arrange(desc(variable)) %>%
    mutate(row.name = 1:n())
  
  perc.missing.rows <- 1:nrow(percent.missing)
  perc.missing.cols <- split(perc.missing.rows,
                             cut(perc.missing.rows,
                                 quantile(perc.missing.rows, (0:n.cols) / n.cols),
                                 include.lowest = TRUE, labels = FALSE)) %>%
    map(~ as_data_frame(.)) %>% bind_rows(.id = "column") %>%
    rename(row.name = value)
  
  percent.missing <- percent.missing %>%
    left_join(perc.missing.cols, by = "row.name")
  
  ggplot(percent.missing, aes(x = perc.missing, y = variable)) +
    geom_barh(stat = "identity") + 
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) + 
    labs(x = "Percent missing", y = NULL) + 
    facet_wrap(~ column, nrow = 1, scales = "free_y") +
    theme_donors() + theme(strip.text = element_blank())
}


# Specifying different scale_*_* for individual facets is tricky/impossible,
# but this integer_breaks function will force all scales to be integers, at
# least.
# (via http://stackoverflow.com/a/10559838/120898)
integer_breaks <- function(n = 5, ...) {
  breaker <- scales::pretty_breaks(n, ...)
  function(x) {
    breaks <- breaker(x)
    breaks[breaks == floor(breaks)]
  }
}
