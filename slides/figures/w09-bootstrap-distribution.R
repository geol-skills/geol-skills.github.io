# Week 9 — a bootstrap interval and the distribution it came from.
#
# Idea adapted from Andrew Heiss, Data Visualization with R, session 6
# "Uncertainty"
# (https://datavizsp26.classes.andrewheiss.com/content/06-content.html):
# show uncertainty as a distribution, not just an interval.
#
# Same data and statistic as exercise w9_1 (exercises/week9-app.qmd): the mean
# of UK wind generation, 2015 to the latest year, bootstrapped 1000 times
# with set.seed(2847). Top panel: the number as it gets reported, "estimate
# [lower, upper]". Bottom panel: the histogram of the 1000 replicate means
# that number was read off, with the same percentile interval shaded. Same
# information, two shapes.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
late <- elec$wind_twh[elec$year >= 2015]

set.seed(2847)
boot_means <- replicate(1000, mean(sample(late, replace = TRUE)))

estimate <- mean(late)
interval <- quantile(boot_means, c(0.025, 0.975))
lower <- unname(interval[1])
upper <- unname(interval[2])

panels <- c("Reported as a range", "1000 bootstrap replicates")

point_data <- data.frame(
  panel = factor(panels[1], levels = panels),
  y = 0, x = estimate, xmin = lower, xmax = upper
)
hist_data <- data.frame(
  panel = factor(panels[2], levels = panels),
  x = boot_means
)
shade_data <- data.frame(
  panel = factor(panels[2], levels = panels),
  xmin = lower, xmax = upper, ymin = 0, ymax = Inf
)

ggplot(mapping = aes(x = x)) +
  geom_rect(
    data = shade_data,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = course_fills[["sky"]], alpha = 0.6
  ) +
  geom_histogram(
    data = hist_data, aes(x = x), bins = 30,
    fill = course_colours[["cyan"]], colour = "white", linewidth = 0.2
  ) +
  geom_segment(
    data = point_data,
    aes(x = xmin, xend = xmax, y = y, yend = y),
    linewidth = 1.6, colour = course_colours[["ink"]]
  ) +
  geom_point(
    data = point_data, aes(x = x, y = y),
    size = 4.5, colour = course_colours[["ink"]]
  ) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  labs(
    x = sprintf("Mean wind generation, 2015-%d (TWh)", max(elec$year)),
    y = NULL,
    title = "One number, or the shape it came from",
    subtitle = sprintf(
      "%.1f\u00a0TWh [%.1f, %.1f]: the same 95%% interval, shaded below",
      estimate, lower, upper
    ),
    caption = "1000 bootstrap resamples, set.seed(2847); data: DUKES 2026"
  ) +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text   = element_text(face = "bold", size = rel(0.95))
  )
