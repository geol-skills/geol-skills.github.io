# Week 10 — a figure that tells a story: the good half of a matched pair.
#
# Paired with w10-unlabelled-figure.R, which plots this same series with
# every label stripped out. Keep the two on the same data: the contrast is
# the teaching, so a change here needs the same change there.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
elec$bioenergy_pct <- 100 * elec$bioenergy_twh / elec$total_twh

first <- elec[which.min(elec$year), ]
last <- elec[which.max(elec$year), ]

ggplot(elec, aes(x = year, y = bioenergy_twh)) +
  geom_line(linewidth = 1.2, colour = course_colours[["cyan"]]) +
  geom_point(data = last, size = 3, colour = course_colours[["cyan"]]) +
  labs(
    x = "Year",
    y = "Bioenergy generation (TWh)",
    title = sprintf("UK bioenergy generation, %d–%d",
                    first$year, last$year),
    subtitle = sprintf(
      "From %.0f%% of all UK electricity in %d to %.0f%% in %d",
      first$bioenergy_pct, first$year, last$bioenergy_pct, last$year
    ),
    caption = "Source: DUKES 2025 Table 5.6B"
  )
