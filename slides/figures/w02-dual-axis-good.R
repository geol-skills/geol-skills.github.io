# Week 2 — the honest counterpart to w02-dual-axis-bad.R.
#
# One axis, both series indexed to 2015 = 100. Two quantities in different
# units can only share a panel if they share a scale, and indexing is the
# usual way to build one. The lines then sit almost on top of each other:
# imports and generation grew together, dipped together in 2022-23, and
# recovered together.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
imports <- read.csv("../data/pellet_imports.csv")

annual <- tapply(imports$import_kt, imports$year, sum)
years <- as.integer(names(annual))
bioenergy <- elec$bioenergy_twh[match(years, elec$year)]

index <- function(x) 100 * x / x[1]

long <- rbind(
  data.frame(year = years, series = "Generation (TWh)",
             value = index(bioenergy)),
  data.frame(year = years, series = "Pellet imports (kt)",
             value = index(as.numeric(annual)))
)

ggplot(long, aes(x = year, y = value, colour = series)) +
  geom_hline(yintercept = 100, linewidth = 0.4,
             colour = course_colours[["concrete"]]) +
  geom_line(linewidth = 1.4) +
  scale_y_continuous(limits = c(0, 160), expand = expansion(c(0, 0.02))) +
  scale_x_continuous(breaks = seq(2015, 2024, 3)) +
  scale_colour_manual(values = unname(course_colours[c("purple", "gold")])) +
  labs(
    x = "Year",
    y = "Index (2015 = 100)",
    colour = NULL,
    title = "Imports and generation, on one scale",
    subtitle = "Both up about 40% since 2015, and dipping together in 2023",
    caption = "Sources: DUKES 2025; Forest Research / HMRC"
  ) +
  theme(legend.position = "top")
