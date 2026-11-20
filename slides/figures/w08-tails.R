# Week 8 — one tail or two? The same null world, counted two ways.
#
# Week 6's 1000 shuffles of the borehole formation labels (same seed as
# w06-null-world.R, so the two-tailed count matches that figure). Left: "is
# there any difference?" counts shuffles at least as extreme as delta in
# either direction. Right: "is Stainmore warmer?" counts only those at least
# as large as delta. The one-tailed p-value is about half the two-tailed one,
# because only one tail is in play. Both p-values are computed here.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

borehole <- read.csv("../data/borehole_temp.csv")

group_diff <- function(temperature, formation) {
  mean(temperature[formation == "Stainmore"]) -
    mean(temperature[formation == "Whin Sill"])
}

delta <- group_diff(borehole$temperature_c, borehole$formation)

set.seed(2341)
null_deltas <- replicate(1000, {
  shuffled <- sample(borehole$formation)
  group_diff(borehole$temperature_c, shuffled)
})

two <- abs(null_deltas) >= abs(delta)
one <- null_deltas >= delta
# Strip labels are plotmath, so p is italic
strip <- function(question, tail) {
  sprintf('atop("%s", "%d of 1000:" ~ italic(p) == "%.3f")',
          question, sum(tail), mean(tail))
}
panel_two <- strip("Two-tailed: any difference?", two)
panel_one <- strip("One-tailed: is Stainmore warmer?", one)
panels <- c(panel_two, panel_one)
worlds <- data.frame(
  delta = rep(null_deltas, 2),
  tail = c(two, one),
  panel = factor(rep(panels, each = length(null_deltas)), levels = panels)
)
cutoffs <- data.frame(
  panel = factor(c(panel_two, panel_two, panel_one), levels = panels),
  at = c(-delta, delta, delta)
)

# Bin edges fall exactly on +/- delta, so every bar is wholly in or out of
# the tail.
width <- abs(delta) / 5
n_bins <- ceiling(max(abs(null_deltas)) / width)
breaks <- width * seq(-n_bins, n_bins)

ggplot(worlds, aes(x = delta, fill = tail)) +
  geom_histogram(breaks = breaks, colour = course_colours[["ink"]],
                 linewidth = 0.2, show.legend = FALSE) +
  geom_vline(data = cutoffs, aes(xintercept = at), linetype = "dashed",
             colour = course_colours[["red"]], linewidth = 0.8) +
  facet_wrap(~ panel, labeller = label_parsed) +
  scale_fill_manual(values = c(`FALSE` = course_fills[["sky"]],
                               `TRUE` = course_colours[["red"]])) +
  scale_y_count() +
  labs(x = "Shuffled δ: Stainmore − Whin Sill mean (°C)",
       y = "Shuffles",
       caption = paste("Dashed: observed δ (and −δ). Week 6's null world;",
                       "source: borehole_temp.csv"))
