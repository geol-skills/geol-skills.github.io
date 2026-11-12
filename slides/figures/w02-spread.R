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

# Rescale each draw to its exact mean and SD, so the slide's claim holds
# to the decimal: both dashed lines land on the same 15 degC.
exact_normal <- function(n, mean, sd) {
  x <- rnorm(n)
  (x - mean(x)) / sd(x) * sd + mean
}

readings <- rbind(
  data.frame(borehole = "Borehole A: SD 0.5\u00a0°C",
             temperature = exact_normal(n, mean_temp, 0.5)),
  data.frame(borehole = "Borehole B: SD 5\u00a0°C",
             temperature = exact_normal(n, mean_temp, 5))
)

means <- aggregate(temperature ~ borehole, data = readings, FUN = mean)

# Each panel names itself in its top-left corner, where the eye lands,
# rather than in a strip above it that is easily lost.
ggplot(readings, aes(x = temperature)) +
  geom_histogram(bins = 45, fill = course_fills[["sky"]],
                 colour = course_colours[["ink"]], linewidth = 0.2) +
  geom_vline(data = means, aes(xintercept = temperature),
             colour = course_colours[["red"]], linetype = "dashed",
             linewidth = 1) +
  geom_label(data = means, aes(label = borehole),
             x = -Inf, y = Inf, hjust = 0, vjust = 1,
             size = 5.5, fontface = "bold", colour = course_colours[["ink"]],
             fill = "white", label.size = 0,
             label.padding = unit(0.4, "lines")) +
  facet_wrap(~borehole, ncol = 1) +
  scale_y_count(expand = expansion(mult = c(0, 0.25))) +
  labs(
    x = "Temperature (°C)",
    y = "Number of readings",
    title = sprintf("Same mean (%.0f\u00a0°C), different meaning", mean_temp),
    caption = quote("Simulated, " * italic(n) * " = 600 readings per borehole")
  ) +
  theme(strip.text = element_blank(), panel.spacing.y = slide_gutter)
