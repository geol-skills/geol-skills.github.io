# Week 4 — the "after" half of the before/after pair.
#
# The same data as w04-figure-before.R, fixed in the four ways the slide
# lists: fewest series that still carry the story, legend ordered by size,
# axis named with its units, and a subtitle that says what to look at.
#
# The four are the largest by MEAN generation over the whole record, not by
# the latest year. Ranking on the latest year quietly drops coal — for most
# of this period the single biggest source in the country — and leaves
# "wind has almost caught gas" as the headline, a 3 TWh gap. Ranking on the
# mean keeps coal in, and the louder and truer story appears: wind and coal
# have swapped places.
#
# So the choice of "top four" is itself an editorial act. The slide says so.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
fuels <- setdiff(grep("_twh$", names(elec), value = TRUE), "total_twh")

ranked <- fuels[order(vapply(elec[fuels], mean, numeric(1)), decreasing = TRUE)]
top <- ranked[1:4]

first <- elec[which.min(elec$year), ]
latest <- elec[which.max(elec$year), ]

readable <- function(fuel) {
  tools::toTitleCase(sub("_twh$", "", fuel))
}

# A fuel that has all but vanished needs its decimal; 120 TWh does not.
tidy_num <- function(x) {
  if (x < 10) sprintf("%.1f", x) else sprintf("%.0f", x)
}

long <- do.call(rbind, lapply(top, function(fuel) {
  data.frame(year = elec$year, fuel = readable(fuel), twh = elec[[fuel]])
}))
long$fuel <- factor(long$fuel, levels = vapply(top, readable, character(1)))

change <- unlist(latest[top]) - unlist(first[top])
risen <- top[which.max(change)]
fallen <- top[which.min(change)]

# Coal takes the darkest colour, whatever its rank, so the line looks like
# the fuel.
colours <- setNames(course_palette(4), levels(long$fuel))
dark <- names(colours)[1]
colours[c(dark, "Coal")] <- colours[c("Coal", dark)]

ggplot(long, aes(x = year, y = twh, colour = fuel)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = colours, name = NULL) +
  labs(
    x = "Year",
    y = "Generation (TWh)",
    title = sprintf("The UK's four biggest power sources, %d to %d",
                    first$year, latest$year),
    subtitle = sprintf(
      "%s has fallen from %s TWh to %s; %s has risen from %s to %s",
      readable(fallen), tidy_num(first[[fallen]]), tidy_num(latest[[fallen]]),
      tolower(readable(risen)), tidy_num(first[[risen]]),
      tidy_num(latest[[risen]])
    ),
    caption = "Source: DUKES 2025 Table 5.6B"
  )
