# Week 8 — "Plotting a comparison", coda: a raincloud.
#
# The previous three views each show one thing: the points, the interval,
# the shape. A raincloud puts all three on one axis: a half-violin
# ("cloud") for the shape of each region's 25 sites, the sites themselves
# beside it ("rain"), and the mean with its 95% CI, computed as in
# w08-groups-points.R. ggdist draws the cloud. Same data and region order
# as the other w08-groups-* figures.
#
# Idea adapted from Andrew Heiss, Data Visualization with R, session 6
# ("Uncertainty"):
# https://datavizsp26.classes.andrewheiss.com/content/06-content.html
#
# w08-groups-raincloud-pair.R sets `highlight` (regions drawn in purple)
# before sourcing this script; on its own, every region is cyan.
#
# set.seed() fixes the jitter, so the figure does not reshuffle between
# renders.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
library("ggdist")

source("figures/theme.R")

wind <- read.csv("../data/wind_output.csv")

region_order <- names(sort(tapply(wind$capacity_pct, wind$region, mean)))
wind$region <- factor(wind$region, levels = region_order)

purple <- if (exists("highlight")) highlight else character(0)
wind$tier <- ifelse(wind$region %in% purple, "high", "rest")
tier_line <- c(high = course_colours[["purple"]],
               rest = course_colours[["cyan"]])
tier_fill <- c(high = course_fills[["heather"]],
               rest = course_fills[["sky"]])

ci_of <- function(x) {
  n <- length(x)
  m <- mean(x)
  se <- sd(x) / sqrt(n)
  tcrit <- qt(0.975, n - 1)
  data.frame(capacity_pct = m, lower = m - tcrit * se, upper = m + tcrit * se)
}

summaries <- do.call(rbind, lapply(split(wind, wind$region), function(d) {
  cbind(region = d$region[1], ci_of(d$capacity_pct))
}))
summaries$region <- factor(summaries$region, levels = region_order)

set.seed(2707)

ggplot(wind, aes(x = region, y = capacity_pct)) +
  stat_slab(
    aes(fill = tier, colour = tier),
    side = "right", position = position_nudge(x = 0.06),
    scale = 0.55, density = "unbounded", trim = FALSE, linewidth = 0.4
  ) +
  geom_jitter(
    aes(x = as.numeric(region) - 0.15, colour = tier),
    width = 0.06, height = 0, alpha = 0.5
  ) +
  geom_pointrange(
    data = summaries, aes(ymin = lower, ymax = upper),
    colour = course_colours[["ink"]], linewidth = 1, size = 0.6
  ) +
  scale_colour_manual(values = tier_line, guide = "none") +
  scale_fill_manual(values = tier_fill, guide = "none") +
  labs(
    x = NULL,
    y = "Capacity factor (%)",
    title = "Sites, interval and distribution",
    subtitle = paste("Cloud: distribution. Rain: the individual sites.",
                     "Bar: mean +/- 95\u00a0% CI."),
    caption = "wind_output.csv, 25 sites per region"
  )
