# Week 6 — the p-value as an area.
#
# "How often would we see a t this extreme?" is a question about area, and
# the slide asks it in words. Everything here — t, df, and the shaded
# fraction — comes out of the same t.test() the live demo runs, so the
# figure and the demo output cannot disagree.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

boreholes <- read.csv("../data/borehole_temp.csv")
test <- t.test(temperature_c ~ formation, data = boreholes)

df <- unname(test$parameter)
t_obs <- unname(abs(test$statistic))

grid <- seq(-4.5, 4.5, length.out = 800)
curve <- data.frame(t = grid, density = dt(grid, df))

# One frame per tail: a single frame would draw a band straight across the
# middle of the distribution, shading the region the tails exclude.
tails <- rbind(
  cbind(curve[curve$t <= -t_obs, ], tail = "lower"),
  cbind(curve[curve$t >= t_obs, ], tail = "upper")
)

ggplot(curve, aes(x = t, y = density)) +
  geom_area(data = tails, aes(group = tail),
            fill = course_colours[["red"]], alpha = 0.6) +
  geom_line(linewidth = 1.1, colour = course_colours[["ink"]]) +
  geom_vline(xintercept = c(-t_obs, t_obs), linetype = "dashed",
             colour = course_colours[["red"]], linewidth = 0.7) +
  scale_x_continuous(breaks = c(-t_obs, 0, t_obs),
                     labels = sprintf("%.2f", c(-t_obs, 0, t_obs))) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  labs(
    x = "t",
    y = NULL,
    title = "The p-value is the shaded area",
    subtitle = sprintf(
      paste("If the two formations really were the same,",
            "t would land out there %.1f%% of the time"),
      100 * test$p.value
    ),
    caption = sprintf("Welch t-test on borehole_temp.csv: t = %.2f, df = %.1f",
                      t_obs, df)
  )
