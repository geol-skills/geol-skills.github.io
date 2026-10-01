# Week 2 — the honest counterpart to w02-cherry-picked-range.R.
#
# The full series, with the 2021-2023 window shaded: the dip is real, and
# it is three years of a climb from 4 TWh to 40 TWh. Shading rather than
# redrawing the window keeps the lie visible inside the truth.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")

ggplot(elec, aes(x = year, y = bioenergy_twh)) +
  annotate("rect", xmin = 2021, xmax = 2023, ymin = 0, ymax = Inf,
           fill = course_fills[["cedar"]], alpha = 0.7) +
  annotate("text", x = 2022, y = 8, label = "the slice\nthey showed you",
           size = 4.2, colour = course_colours[["ink"]], lineheight = 0.95) +
  geom_line(linewidth = 1.2, colour = course_colours[["purple"]]) +
  geom_point(size = 2, colour = course_colours[["purple"]]) +
  scale_y_continuous(limits = c(0, 45), expand = expansion(c(0, 0.02))) +
  labs(
    x = "Year",
    y = "Bioenergy generation (TWh)",
    title = "UK bioenergy generation, 2000-2024",
    caption = "Source: DUKES 2025"
  )
