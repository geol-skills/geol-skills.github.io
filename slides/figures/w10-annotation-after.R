# Week 10 — label or legend? The "after" half of a matched pair.
#
# The same data and scales as w10-annotation-before.R, with three editorial
# decisions made:
#
#   1. Label the lines directly, so there is no legend to decode.
#   2. Highlight the two lines the story is about (coal and bioenergy) and
#      grey the rest. The grey lines stay: they are the context that answers
#      "compared to what?".
#   3. Annotate the one point that matters: the first year bioenergy made
#      more of the UK's electricity than coal. The year is found from the
#      data, not typed.
#
# ggrepel spreads the end-of-line labels vertically so they don't overlap,
# and draws a short leader from each label to its line.
#
# Idea adapted from Andrew Heiss, Data Visualization with R, session 9
# ("Annotations"),
# https://datavizsp26.classes.andrewheiss.com/content/09-content.html
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
library("ggrepel")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
fuels <- setdiff(grep("_twh$", names(elec), value = TRUE), "total_twh")
story <- c("Coal", "Bioenergy")

readable <- function(fuel) {
  tools::toTitleCase(sub("_twh$", "", fuel))
}

long <- do.call(rbind, lapply(fuels, function(fuel) {
  data.frame(year = elec$year, fuel = readable(fuel),
             share = 100 * elec[[fuel]] / elec$total_twh)
}))
long$loud <- long$fuel %in% story

first_year <- min(long$year)
last_year <- max(long$year)
ends <- long[long$year == last_year, ]

share_of <- function(fuel, year) {
  long$share[long$fuel == fuel & long$year == year]
}

# The point that matters: the first year bioenergy out-generated coal.
years <- sort(unique(long$year))
ahead <- vapply(years, function(y) {
  share_of("Bioenergy", y) > share_of("Coal", y)
}, logical(1))
cross_year <- years[which(ahead)[1]]
cross_share <- share_of("Bioenergy", cross_year)

colours <- c(Coal = course_colours[["ink"]],
             Bioenergy = course_colours[["cyan"]])
grey <- course_colours[["concrete"]]
ends$colour <- ifelse(ends$loud, colours[ends$fuel], grey)

label_x <- last_year + 0.6

ggplot(long, aes(x = year, y = share, group = fuel)) +
  geom_line(data = long[!long$loud, ], colour = grey, linewidth = 0.8) +
  geom_line(data = long[long$loud, ], aes(colour = fuel), linewidth = 1.6) +
  geom_text_repel(
    data = ends,
    aes(label = fuel),
    colour = ends$colour,
    fontface = ifelse(ends$loud, "bold", "plain"),
    size = 5, hjust = 0, direction = "y",
    xlim = c(label_x, Inf), segment.size = 0.4,
    min.segment.length = 0, seed = 1
  ) +
  annotate("point", x = cross_year, y = cross_share, size = 3.5,
           colour = colours[["Bioenergy"]]) +
  annotate(
    "curve",
    x = cross_year - 6, y = cross_share + 2.5,
    xend = cross_year - 0.4, yend = cross_share + 0.4,
    curvature = -0.3, colour = course_colours[["black"]], linewidth = 0.5,
    arrow = arrow(length = unit(0.12, "inches"), type = "closed")
  ) +
  annotate(
    "text", x = cross_year - 6.2, y = cross_share + 2.5,
    label = sprintf("%d: bioenergy overtakes coal", cross_year),
    hjust = 1, size = 5, colour = course_colours[["black"]]
  ) +
  scale_colour_manual(values = colours, guide = "none") +
  scale_x_continuous(breaks = seq(first_year, last_year, by = 5)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(xlim = c(first_year, last_year), clip = "off") +
  labs(
    x = "Year",
    y = "Share of UK electricity (%)",
    title = sprintf("Bioenergy overtook coal in %d, as coal all but vanished",
                    cross_year),
    subtitle = sprintf(
      paste0("Coal fell from %.0f\u00a0%% of UK electricity in %d ",
             "to %.1f\u00a0%% in %d; ",
             "bioenergy rose from %.0f\u00a0%% to %.0f\u00a0%%"),
      share_of("Coal", first_year), first_year,
      share_of("Coal", last_year), last_year,
      share_of("Bioenergy", first_year), share_of("Bioenergy", last_year)
    ),
    caption = "Source: DUKES 2026 Table 5.6.B"
  ) +
  theme(plot.margin = margin(5, 110, 5, 5))
