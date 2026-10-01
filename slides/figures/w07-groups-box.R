# Week 7 — "Same data, four ways": 2. box plot.
#
# Same data as w07-groups-bar.R, same region order. The spread is now
# visible — but not the individual sites, the sample size per region, or
# whether a region's distribution is one lump or two.
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

ggplot(wind, aes(x = region, y = capacity_pct)) +
  geom_boxplot(fill = course_fills[["sky"]], colour = course_colours[["ink"]]) +
  labs(
    x = NULL,
    y = "Capacity factor (%)",
    title = "2. Box plot",
    subtitle = "Now you can see the spread.",
    caption = "wind_output.csv, 25 sites per region"
  )
