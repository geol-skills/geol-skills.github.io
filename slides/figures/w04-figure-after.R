# Week 4 — the "after" half of the before/after pair.
#
# The same data as w04-figure-before.R, fixed in the four ways the slide
# lists: fewest series that still carry the story, each line labelled at its
# end rather than in a legend, axis named with its units, and a subtitle
# that says what to look at.
#
# The four are the largest by MEAN generation over the whole record, not by
# the latest year. Ranking on the latest year quietly drops coal — for most
# of this period the single biggest source in the country — and leaves
# "wind has almost caught gas" as the headline, a 7 TWh gap. Ranking on the
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

# A fuel that has all but vanished needs its decimal, or the word "zero";
# 120 TWh does not.
tidy_num <- function(x) {
  if (x == 0) return("zero")
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

# Labels sit just past each line's end. Gas and wind finish 7 TWh apart, so
# end values closer than `gap` are pushed apart, keeping their order.
ends <- long[long$year == latest$year, ]
ends <- ends[order(ends$twh), ]
gap <- 0.09 * max(long$twh)
ends$label_y <- ends$twh
for (i in seq_len(nrow(ends))[-1]) {
  ends$label_y[i] <- max(ends$label_y[i], ends$label_y[i - 1] + gap)
}

ggplot(long, aes(x = year, y = twh, colour = fuel)) +
  geom_line(linewidth = 1.2) +
  geom_text(data = ends, aes(y = label_y, label = fuel), hjust = 0,
            nudge_x = 0.6, size = 5, show.legend = FALSE) +
  scale_colour_manual(values = colours, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.13))) +
  labs(
    x = "Year",
    y = "Generation (TWh)",
    title = sprintf("The UK's four biggest power sources, %d to %d",
                    first$year, latest$year),
    subtitle = sprintf(
      "%s has fallen from %s\u00a0TWh to %s; %s has risen from %s to %s",
      readable(fallen), tidy_num(first[[fallen]]), tidy_num(latest[[fallen]]),
      tolower(readable(risen)), tidy_num(first[[risen]]),
      tidy_num(latest[[risen]])
    ),
    caption = "Source: DUKES 2026 Table 5.6.B"
  )
