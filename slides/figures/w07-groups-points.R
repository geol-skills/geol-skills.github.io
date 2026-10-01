# Week 7 — "Same data, four ways": 3. raw points + mean +/- 95% CI.
#
# Same data, same region order. This is the picture ANOVA actually
# reasons about: a mean and its uncertainty, with the 25 sites behind it
# so the reader can see how much scatter that mean summarises. The CI is
# a plain t-based interval computed by hand — mean_cl_normal() needs
# Hmisc, which is not on the deploy runner or in the webR package set.
#
# Idea adapted from Andrew Heiss, Data Visualization with R, session 6
# ("Uncertainty"):
# https://datavizsp26.classes.andrewheiss.com/content/06-content.html
#
# set.seed() fixes the jitter, so the figure does not reshuffle between
# renders.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

wind <- read.csv("../data/wind_output.csv")

region_order <- names(sort(tapply(wind$capacity_pct, wind$region, mean)))
wind$region <- factor(wind$region, levels = region_order)

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
  geom_jitter(width = 0.15, height = 0, alpha = 0.5,
              colour = course_colours[["cyan"]]) +
  geom_errorbar(data = summaries, aes(ymin = lower, ymax = upper),
                width = 0.2, colour = course_colours[["ink"]],
                linewidth = 1) +
  geom_point(data = summaries, size = 2.6, colour = course_colours[["ink"]]) +
  labs(
    x = NULL,
    y = "Capacity factor (%)",
    title = "3. Raw points + mean +/- 95% CI",
    subtitle = "The sites, and what ANOVA actually compares.",
    caption = "wind_output.csv, 25 sites per region"
  )
