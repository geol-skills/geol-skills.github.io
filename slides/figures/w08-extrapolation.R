# Week 8 — extrapolating UK solar growth past the point of absurdity.
#
# The slide asserts that extrapolating solar to 2050 puts it above the
# whole UK grid. That is a numeric claim about a dataset the repo ships,
# so it is fitted here rather than stated: lm() on log(solar), projected
# forward, against total generation.
#
# The projection stops a few years past the crossing. Carried to 2050 on a
# linear axis it reaches tens of thousands of TWh and flattens the grid
# line to nothing; the 2050 figure goes in the subtitle instead, where it
# is still computed.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
observed <- elec[elec$solar_twh > 0, ]

growth <- lm(log(solar_twh) ~ year, data = observed)
grid_now <- elec$total_twh[which.max(elec$year)]

horizon <- 2050
future <- data.frame(year = seq(min(observed$year), horizon))
future$solar_twh <- exp(predict(growth, future))

crossing <- min(future$year[future$solar_twh > grid_now])
shown <- future[future$year <= crossing + 2, ]

ggplot(shown, aes(x = year, y = solar_twh)) +
  geom_hline(yintercept = grid_now, colour = course_colours[["red"]],
             linetype = "dashed", linewidth = 0.9) +
  annotate("text", x = min(shown$year), y = grid_now, hjust = 0, vjust = -0.6,
           size = 4.4, colour = course_colours[["red"]],
           label = sprintf("All UK generation, %d: %.0f TWh",
                           max(elec$year), grid_now)) +
  geom_line(linewidth = 1, linetype = "dotted",
            colour = course_colours[["concrete"]]) +
  geom_line(data = observed, aes(y = solar_twh), linewidth = 1.3,
            colour = course_colours[["cyan"]]) +
  geom_point(data = observed, aes(y = solar_twh), size = 1.8,
             colour = course_colours[["cyan"]]) +
  labs(
    x = "Year",
    y = "Solar generation (TWh)",
    title = sprintf("The fitted trend overtakes the entire grid in %d",
                    crossing),
    subtitle = sprintf(
      paste("Solid: measured. Dotted: the same model extrapolated.",
            "By %d it reads %.0f TWh, %.0f times the grid."),
      horizon, future$solar_twh[future$year == horizon],
      future$solar_twh[future$year == horizon] / grid_now
    ),
    caption = "Exponential fit to DUKES 2025 solar generation, 2011 onward"
  )
