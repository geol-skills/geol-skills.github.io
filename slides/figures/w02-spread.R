# Week 2 — two boreholes with the same mean and very different spreads.
#
# The slide's own numbers (mean 10, SD 50) would put most of the broad
# sample below zero, which is fine for an abstract variable and wrong for
# a temperature. The speaker note's boreholes — both averaging 15 degC,
# one varying by half a degree and one by five — say the same thing and
# stay physical.
#
# A shared x-axis is what makes the point: the tight borehole is a spike
# inside the range the loose one wanders over.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

set.seed(2203)
n <- 600
mean_temp <- 15

readings <- rbind(
  data.frame(borehole = "Borehole A: SD 0.5 °C",
             temperature = rnorm(n, mean_temp, 0.5)),
  data.frame(borehole = "Borehole B: SD 5 °C",
             temperature = rnorm(n, mean_temp, 5))
)

means <- do.call(rbind, lapply(split(readings, readings$borehole), function(d) {
  data.frame(borehole = d$borehole[1], temperature = mean(d$temperature))
}))

ggplot(readings, aes(x = temperature)) +
  geom_histogram(bins = 45, fill = course_fills[["sky"]],
                 colour = course_colours[["ink"]], linewidth = 0.2) +
  geom_vline(data = means, aes(xintercept = temperature),
             colour = course_colours[["red"]], linetype = "dashed",
             linewidth = 1) +
  facet_wrap(~borehole, ncol = 1) +
  labs(
    x = "Temperature (°C)",
    y = "Number of readings",
    title = "Same mean, different stories",
    subtitle = sprintf(
      "Both average %.1f °C. Only one of them is telling you the temperature.",
      mean(readings$temperature)
    ),
    caption = "Simulated, n = 600 readings per borehole"
  )
