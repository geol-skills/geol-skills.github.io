# Week 1 — the crossover: coal collapsing as bioenergy grows.
#
# This is the plenary answer to the session's own exercise ("Add coal to
# the same plot — what do you see?"), so it is deliberately the chart the
# students have just tried to build: plot(), lines(), legend(), no ggplot2.
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
  elec$year, elec$coal_twh,
  type = "l", lwd = 3, col = course_colours[["ink"]],
  ylim = c(0, max(elec$coal_twh)),
  xlab = "Year", ylab = "Generation (TWh)",
  main = sprintf("Bioenergy overtook coal in %d", crossover)
)
lines(elec$year, elec$bioenergy_twh, lwd = 3, col = course_colours[["cyan"]])

legend(
  "topright",
  legend = c("Coal", "Bioenergy"),
  col = c(course_colours[["ink"]], course_colours[["cyan"]]),
  lwd = 3, bty = "n", cex = 1.1
)
add_source("Source: DUKES 2025 Table 5.6B")

invisible(NULL)
