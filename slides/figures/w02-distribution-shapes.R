# Week 2 — three distribution shapes, with mean and median marked.
#
# The point of the slide is that mean and median separate as a distribution
# gets less symmetric. So both are computed from the sample, never placed by
# eye: where the lines fall is the evidence for the claim.
#
# The deck reveals the panels one at a time: set `shapes_shown` (1-3) before
# sourcing to draw only the first panels. The later ones are blanked out of
# the finished layout rather than left out of the data, so every build has
# the same geometry and the builds stack exactly.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

# Sample and centres, shared with w02-bimodal.R.
source("figures/w02-shapes-data.R", local = TRUE)

shapes_plot <- ggplot(shapes, aes(x = value)) +
  geom_histogram(bins = 45, fill = course_fills[["sky"]],
                 colour = course_colours[["ink"]], linewidth = 0.2) +
  geom_vline(data = centres,
             aes(xintercept = value, colour = statistic, linetype = statistic),
             linewidth = 1) +
  facet_wrap(~shape, scales = "free", nrow = 1) +
  # Linetype repeats the colour distinction, so the two stay separable in
  # greyscale and for colour-vision deficiency.
  scale_colour_manual(values = c(mean = course_colours[["red"]],
                                 median = course_colours[["purple"]]),
                      name = NULL) +
  scale_linetype_manual(values = c(mean = "dashed", median = "solid"),
                        name = NULL) +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    strip.text = element_text(face = "bold", size = rel(1.05)),
    legend.position = "bottom"
  )

n_shown <- if (exists("shapes_shown")) shapes_shown else nlevels(shapes$shape)
grob <- ggplotGrob(shapes_plot)
# Each panel's grobs carry its facet column in their names: "panel-3-1",
# "strip-t-3-1" and "axis-b-3-1", but "axis-l-1-3" and "axis-r-1-3".
part <- strsplit(grob$layout$name, "-", fixed = TRUE)
column <- vapply(part, function(x) {
  if (x[1] %in% c("panel", "strip", "axis") && length(x) >= 3) {
    as.integer(if (x[2] %in% c("l", "r")) x[4] else x[length(x) - 1])
  } else {
    NA_integer_
  }
}, integer(1))
hidden <- !is.na(column) & column > n_shown
grob$grobs[hidden] <- list(zeroGrob())
grid::grid.newpage()
grid::grid.draw(grob)
invisible(NULL)
