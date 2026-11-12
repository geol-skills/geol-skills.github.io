# Week 1 — the crossover: coal collapsing as bioenergy grows.
#
# This is the plenary answer to the session's Exercise 3 ("Add coal to
# the same plot — what do you see?"), so it is deliberately the chart the
# students have just tried to build, in the same order and line types as
# the solution: plot(), lines(), legend(), no ggplot2.
#
# The crossover year is found from the data, not read off the chart.
#
# Run with the working directory set to slides/ (see figures/README.md).

source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
overtaken <- elec$year[elec$bioenergy_twh > elec$coal_twh]
crossover <- min(overtaken)

par_slide()

plot(
  elec$year, elec$bioenergy_twh,
  type = "l", lwd = 3, lty = 1, col = course_colours[["cyan"]],
  ylim = c(0, 150),
  xlab = "Year", ylab = "Generation (TWh)",
  main = sprintf("Bioenergy overtook coal in %d", crossover)
)
lines(elec$year, elec$coal_twh, lwd = 3, lty = 2, col = course_colours[["ink"]])

legend(
  "topright",
  legend = c("Bioenergy", "Coal"),
  col = c(course_colours[["cyan"]], course_colours[["ink"]]),
  lwd = 3, lty = c(1, 2), bty = "n", cex = 1.1
)
add_source("Source: DUKES 2026 Table 5.6.B")

invisible(NULL)
