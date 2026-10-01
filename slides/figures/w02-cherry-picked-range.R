# Week 2 — "Spot the lie", chart 2: the cherry-picked date range.
#
# Deliberately misleading: 2021-2023 is a genuine dip inside a series that
# grew roughly tenfold since 2000. The window hides the trend it contradicts.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
window <- elec[elec$year >= 2021 & elec$year <= 2023, ]

ggplot(window, aes(x = year, y = bioenergy_twh)) +
  geom_line(linewidth = 1.2, colour = course_colours[["purple"]]) +
  geom_point(size = 3, colour = course_colours[["purple"]]) +
  scale_x_continuous(breaks = window$year) +
  labs(
    x = "Year",
    y = "Bioenergy generation (TWh)",
    title = "Bioenergy is in decline",
    caption = "Source: DUKES 2025"
  )
