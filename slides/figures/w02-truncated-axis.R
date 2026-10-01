# Week 2 — "Spot the lie", chart 1: the truncated axis.
#
# Deliberately misleading: bar heights encode ratios, so a baseline of
# 270 TWh turns a 13% fall into an apparent collapse.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
recent <- elec[elec$year >= 2019, ]

ggplot(recent, aes(x = factor(year), y = total_twh)) +
  geom_col(fill = course_colours[["cyan"]], width = 0.7) +
  coord_cartesian(ylim = c(270, 330)) +
  labs(
    x = "Year",
    y = "Total generation (TWh)",
    title = "UK electricity generation is collapsing!",
    caption = "Source: DUKES 2025"
  )
