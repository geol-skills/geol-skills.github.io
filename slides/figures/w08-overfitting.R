# Week 8 — the degree-15 polynomial that fits better and predicts worse.
#
# The deck live-codes this; the figure is the persistent version students
# can revisit, and the fallback if the demo misbehaves.
#
# Both R-squared values are read off the fitted models, so "fits worse,
# predicts better" is arithmetic on the slide rather than assertion.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

set.seed(2801)
n <- 20
observations <- data.frame(x = sort(runif(n, 0, 10)))
observations$y <- 3 + 1.4 * observations$x + rnorm(n, 0, 2.5)

straight <- lm(y ~ x, data = observations)
wiggly <- lm(y ~ poly(x, 15), data = observations)

fits <- data.frame(x = seq(min(observations$x), max(observations$x),
                           length.out = 400))
fits$straight <- predict(straight, fits)
fits$wiggly <- predict(wiggly, fits)

lines <- rbind(
  data.frame(x = fits$x, y = fits$straight,
             model = sprintf("Straight line (R² = %.2f)",
                             summary(straight)$r.squared)),
  data.frame(x = fits$x, y = fits$wiggly,
             model = sprintf("Degree 15 (R² = %.2f)",
                             summary(wiggly)$r.squared))
)

ggplot(observations, aes(x = x, y = y)) +
  geom_line(data = lines, aes(colour = model), linewidth = 1.1) +
  geom_point(size = 2.6, colour = course_colours[["ink"]]) +
  # Wide enough that the oscillation between points reads as oscillation
  # rather than as vertical bars leaving the panel.
  coord_cartesian(ylim = range(observations$y) + c(-14, 14)) +
  scale_colour_manual(
    values = c(unname(course_colours[["red"]]),
               unname(course_colours[["cyan"]])),
    name = NULL
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Twenty points, two models",
    subtitle = paste("The wiggly one passes closer to every point",
                     "and is worthless a step outside them."),
    caption = "Simulated, n = 20"
  ) +
  theme(legend.position = "bottom")
