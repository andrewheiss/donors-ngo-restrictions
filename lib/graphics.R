theme_donors <- function(base_size=9, base_family="Open Sans") {
  update_geom_defaults("bar", list(fill = "grey30"))
  update_geom_defaults("line", list(colour = "grey30"))
  update_geom_defaults("label", list(family="Open Sans"))
  update_geom_defaults("text", list(family="Open Sans"))
  # update_geom_defaults("label_repel", list(family="Open Sans"))
  # update_geom_defaults("text_repel", list(family="Open Sans"))
  
  ret <- theme_bw(base_size, base_family) +
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          title = element_text(size=rel(1.1), vjust=1.2, family="Open Sans Bold"),
          plot.subtitle = element_text(size=rel(0.8), family="Open Sans"),
          plot.caption = element_text(margin=margin(t=10), size=rel(0.6),
                                    family="Open Sans"),
          panel.border = element_blank(), 
          panel.spacing = unit(1, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size=0.25, colour="grey90"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size=rel(0.8), family="Open Sans Bold"),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          legend.position = "bottom",
          legend.title = element_text(size=rel(0.8)),
          legend.key.size = unit(.7, "line"),
          legend.key = element_blank(),
          legend.spacing = unit(0.1, "lines"),
          strip.text = element_text(size=rel(1), family="Open Sans Bold"),
          strip.background = element_rect(fill="#ffffff", colour=NA))
  ret
}
