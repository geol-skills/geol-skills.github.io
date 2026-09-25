# Week 7 — what small, medium and large actually look like.
#
# The slide gives a lookup table: 0.2 small, 0.5 medium, 0.8 large. The
# table cannot say that even a "large" effect leaves the two groups
# overlapping across most of their range, which is the thing students most
# need to see before they read d = 0.8 as "obviously different".
#
# Densities are evaluated analytically, so nothing is simulated and there
# is no seed: with unit SD, the separation between the curves *is* d.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

effects <- c(0.2, 0.5, 0.8)
labels <- c("Small", "Medium", "Large")
grid <- seq(-4, 5, length.out = 600)

curves <- do.call(rbind, lapply(seq_along(effects), function(i) {
  d <- effects[i]
  group_a <- dnorm(grid, 0, 1)
  group_b <- dnorm(grid, d, 1)
  data.frame(
    panel = sprintf("%s: d = %.1f", labels[i], d),
    value = rep(grid, 2),
    density = c(group_a, group_b),
    group = rep(c("Group A", "Group B"), each = length(grid))
  )
}))

overlap <- do.call(rbind, lapply(seq_along(effects), function(i) {
  d <- effects[i]
  data.frame(panel = sprintf("%s: d = %.1f", labels[i], d),
             value = grid,
             density = pmin(dnorm(grid, 0, 1), dnorm(grid, d, 1)))
}))

panel_levels <- sprintf("%s: d = %.1f", labels, effects)
curves$panel <- factor(curves$panel, levels = panel_levels)
overlap$panel <- factor(overlap$panel, levels = panel_levels)

# Proportion of the two distributions that coincides, by numeric
# integration of the pointwise minimum.
shared <- vapply(effects, function(d) {
  sum(pmin(dnorm(grid, 0, 1), dnorm(grid, d, 1))) * diff(grid[1:2])
}, numeric(1))

ggplot(curves, aes(x = value, y = density)) +
  geom_area(data = overlap, fill = course_fills[["stone"]]) +
  geom_line(aes(colour = group), linewidth = 1.1) +
  facet_wrap(~panel) +
  scale_colour_manual(values = c("Group A" = course_colours[["ink"]],
                                 "Group B" = course_colours[["cyan"]]),
                      name = NULL) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  labs(
    x = "Standard deviations",
    y = NULL,
    title = "Small, medium and large, drawn to scale",
    subtitle = sprintf(
      "Even at d = %.1f the two groups still share %.0f%% of their range",
      max(effects), 100 * shared[length(shared)]
    ),
    caption = "Normal densities with unit SD; separation is d by construction"
  ) +
  theme(legend.position = "bottom")
