# Week 6 — SD or SE? More data keeps the scatter but narrows the null world.
#
# Top row: 8 boreholes per formation, drawn at random from borehole_temp.csv.
# Bottom row: all 35 per formation. Left: the data, as on "A world where
# formation doesn't matter". Right: 1000 shuffled deltas for each.
#
# The point the slide makes in speech: with more points the scatter of the
# data (the SD) stays about the same, but a shuffle is less likely to put
# the means far apart by chance, so the null world (whose width is the SE)
# narrows. Both are computed here, never typed. Real data only: the small
# sample is a subset, not a simulation. Its seed (29) was picked so the
# subset's SDs sit close to the full data's (20 and 9 degrees C): any one
# small sample can stray, and a stray one would muddle the point.
#
# Faceted by sample size so each column shares its axis and the widths can
# be compared by eye; the two columns differ in geom, so side_by_side().
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

borehole <- read.csv("../data/borehole_temp.csv")

group_diff <- function(temperature, formation) {
  mean(temperature[formation == "Stainmore"]) -
    mean(temperature[formation == "Whin Sill"])
}

small_n <- 8
set.seed(29)
small_rows <- unlist(lapply(split(seq_len(nrow(borehole)),
                                  borehole$formation),
                            sample, size = small_n))

samples <- list(borehole[small_rows, ], borehole)
n_labels <- sprintf("%d per formation",
                    vapply(samples, nrow, integer(1)) / 2)

# Seed 6 for each row, as in the live demo, so the bottom row's SE is the
# sd(null_deltas) the room has just seen.
null_deltas <- lapply(samples, function(d) {
  set.seed(6)
  replicate(1000, group_diff(d$temperature_c, sample(d$formation)))
})

points <- do.call(rbind, Map(function(d, label) {
  d$n <- label
  d
}, samples, n_labels))
points$n <- factor(points$n, levels = n_labels)

null_world <- data.frame(
  delta = unlist(null_deltas),
  n = factor(rep(n_labels, each = 1000), levels = n_labels)
)

# One label per row: the within-formation SDs on the left, the width of the
# null world on the right.
sds <- vapply(samples, function(d) {
  s <- tapply(d$temperature_c, d$formation, sd)
  sprintf("SD: %.0f and %.0f °C", s[["Stainmore"]], s[["Whin Sill"]])
}, character(1))
ses <- sprintf("SE ≈ %.1f °C", vapply(null_deltas, sd, numeric(1)))
sd_labels <- data.frame(n = factor(n_labels, levels = n_labels), label = sds)
se_labels <- data.frame(n = factor(n_labels, levels = n_labels), label = ses)

# Left: the data -----------------------------------------------------------

set.seed(1)
observed <- ggplot(points, aes(x = formation, y = temperature_c)) +
  geom_jitter(aes(colour = formation), width = 0.12, height = 0,
              size = 1.8, alpha = 0.7, show.legend = FALSE) +
  geom_text(data = sd_labels, aes(label = label), x = 1.5, y = Inf,
            vjust = 1.3, size = 4.2, colour = course_colours[["ink"]],
            inherit.aes = FALSE) +
  facet_grid(rows = vars(n)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  scale_colour_manual(values = unname(course_colours[c("ink", "cyan")])) +
  labs(x = NULL, y = "Temperature (°C)",
       title = "SD: same scatter")

# Right: the null worlds ---------------------------------------------------

shuffled <- ggplot(null_world, aes(x = delta)) +
  geom_histogram(binwidth = 1, boundary = 0,
                 fill = course_fills[["sky"]],
                 colour = course_colours[["ink"]], linewidth = 0.2) +
  geom_text(data = se_labels, aes(label = label), x = -Inf, y = Inf,
            hjust = -0.1, vjust = 1.3, size = 4.2,
            colour = course_colours[["ink"]], inherit.aes = FALSE) +
  facet_grid(rows = vars(n)) +
  scale_y_count(expand = expansion(mult = c(0, 0.25))) +
  labs(x = "Shuffled δ: Stainmore − Whin Sill mean (°C)",
       y = "Shuffles",
       title = "SE: narrower null world",
       caption = "Source: borehole_temp.csv; 8 per formation drawn at random")

side_by_side(observed, shuffled, widths = c(0.4, 0.6))
