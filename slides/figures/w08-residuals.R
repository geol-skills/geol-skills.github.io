# Week 8 — the seasonal cycle HolmesCo's model does not know about.
#
# The generator below is copied verbatim from exercises/week8-content.qmd,
# seed included, so the slide and the WebR exercise show the same 60
# months. If that exercise changes, change this with it.
#
# Residuals against month rather than against fitted value: the pattern is
# a cycle in time, and time is not an axis of the default diagnostic plot.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

set.seed(7834)
months <- 1:60
rainfall <- 55 + 30 * sin(2 * pi * months / 12) + rnorm(60, 0, 10)
rainfall <- pmax(round(rainfall, 1), 5)
gw_level <- 8 + 0.07 * rainfall +
  1.5 * sin(2 * pi * (months - 4) / 12) +
  -0.015 * months + rnorm(60, 0, 0.5)
groundwater <- data.frame(
  month = months,
  rainfall_mm = rainfall,
  gw_level_m = round(gw_level, 2)
)

model <- lm(gw_level_m ~ rainfall_mm, data = groundwater)
groundwater$residual <- resid(model)

ggplot(groundwater, aes(x = month, y = residual)) +
  geom_hline(yintercept = 0, colour = course_colours[["concrete"]],
             linewidth = 0.6) +
  geom_line(linewidth = 0.9, colour = course_colours[["cyan"]]) +
  geom_point(size = 2, colour = course_colours[["cyan"]]) +
  scale_x_continuous(breaks = seq(0, 60, by = 12)) +
  labs(
    x = "Month of monitoring",
    y = "Residual (m)",
    title = "What rainfall alone leaves behind",
    subtitle = paste("Residuals from gw_level ~ rainfall.",
                     "They should be noise. They are a calendar."),
    caption = sprintf("Simulated monitoring record, 60 months; R² = %.2f",
                      summary(model)$r.squared)
  )
