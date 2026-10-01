# Week 10 — label or legend? The "before" half of a matched pair.
#
# Not a bad figure. It is Week 4's "after" standard: axes named with units,
# a title, a source, a legend ordered by size. Every fuel gets its own
# colour and every line is drawn equally loud, so the reader has to shuttle
# between the legend and the lines to find the story, and is not told which
# story to look for.
#
# Paired with w10-annotation-after.R: same data, same scales, but the lines
# are labelled directly, two are highlighted and the rest greyed, and one
# point is annotated. Keep the two on the same data: the difference between
# them is the lesson, so change one and you must change the other.
#
# Idea adapted from Andrew Heiss, Data Visualization with R, session 9
# ("Annotations"),
# https://datavizsp26.classes.andrewheiss.com/content/09-content.html
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
fuels <- setdiff(grep("_twh$", names(elec), value = TRUE), "total_twh")

readable <- function(fuel) {
  tools::toTitleCase(sub("_twh$", "", fuel))
}

long <- do.call(rbind, lapply(fuels, function(fuel) {
  data.frame(year = elec$year, fuel = readable(fuel),
             share = 100 * elec[[fuel]] / elec$total_twh)
}))

# Legend ordered by mean share over the record, largest first: the Week 4
# fix, ranked as w04-figure-after.R ranks. Ranking on 2024 alone would put
# coal last and hand it concrete, the palette's de-emphasis grey, muting one
# of the two lines the story is about before the reader has started.
mean_share <- tapply(long$share, long$fuel, mean)
long$fuel <- factor(long$fuel,
                    levels = names(sort(mean_share, decreasing = TRUE)))

ggplot(long, aes(x = year, y = share, colour = fuel)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = course_palette(nlevels(long$fuel)),
                      name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Year",
    y = "Share of UK electricity (%)",
    title = sprintf("UK electricity generation by fuel, %d–%d",
                    min(long$year), max(long$year)),
    caption = "Source: DUKES 2025 Table 5.6B"
  )
