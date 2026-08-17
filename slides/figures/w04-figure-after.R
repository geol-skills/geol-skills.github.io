# Week 4 — the "after" half of the before/after pair.
#
# The same data as w04-figure-before.R, fixed in the four ways the slide
# lists: fewest series that still carry the story, legend ordered by value,
# axis named with its units, and a caption that says what to look at.
#
# Which four fuels, and the gap quoted in the subtitle, are computed from
# the latest year rather than chosen by hand.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
fuels <- setdiff(grep("_twh$", names(elec), value = TRUE), "total_twh")
latest <- elec[which.max(elec$year), ]

ranked <- fuels[order(unlist(latest[fuels]), decreasing = TRUE)]
top <- ranked[1:4]

readable <- function(fuel) {
  tools::toTitleCase(sub("_twh$", "", fuel))
}

long <- do.call(rbind, lapply(top, function(fuel) {
  data.frame(year = elec$year, fuel = readable(fuel), twh = elec[[fuel]])
}))
long$fuel <- factor(long$fuel, levels = vapply(top, readable, character(1)))

gap <- latest[[top[1]]] - latest[[top[2]]]

ggplot(long, aes(x = year, y = twh, colour = fuel)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = course_palette(4), name = NULL) +
  labs(
    x = "Year",
    y = "Generation (TWh)",
    title = sprintf("The UK's four largest power sources in %d",
                    latest$year),
    subtitle = sprintf(
      "%s has climbed from almost nothing to within %.0f TWh of %s",
      readable(top[2]), gap, readable(top[1])
    ),
    caption = "Source: DUKES 2025 Table 5.6B"
  )
