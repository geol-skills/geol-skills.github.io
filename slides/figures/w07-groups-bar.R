# Week 7 — "Same data, four ways": 1. bar of means.
#
# A "dynamite plot" — bars with nothing else. Deliberately the weakest
# of the four panels: it shows four numbers and hides everything else
# about the 100 sites behind them (spread, shape, sample size, overlap
# between regions). Region order is by group mean, held identical
# across all four scripts in this set so the sequence reads as one
# argument.
#
# Idea adapted from Andrew Heiss, Data Visualization with R, session 6
# ("Uncertainty"):
# https://datavizsp26.classes.andrewheiss.com/content/06-content.html
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

wind <- read.csv("../data/wind_output.csv")

region_order <- names(sort(tapply(wind$capacity_pct, wind$region, mean)))
wind$region <- factor(wind$region, levels = region_order)

means <- aggregate(capacity_pct ~ region, data = wind, FUN = mean)

ggplot(means, aes(x = region, y = capacity_pct)) +
  geom_col(fill = course_colours[["concrete"]], width = 0.6) +
  labs(
    x = NULL,
    y = "Capacity factor (%)",
    title = "1. Bar of means",
    subtitle = "Four numbers. Looks conclusive.",
    caption = "wind_output.csv, 25 sites per region"
  )
