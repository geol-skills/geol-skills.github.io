# Week 6 — the p-value as a tail you can count.
#
# Idea adapted from Andrew Heiss, "Null worlds: statistical testing in null
# worlds", https://nullworlds.andrewheiss.com/ (CC BY-SA 4.0). Our data, our
# code.
#
# Left: the world we sampled, with the difference in mean temperature
# between the formations (delta) marked. Right: 1000 worlds in which
# formation does not matter, made by shuffling the formation labels and
# recomputing delta each time. The red bars are the shuffles at least as
# extreme as the real delta; their share is the p-value, seen rather than
# looked up.
#
# The shuffle code and seed are exactly what the live demo on the next
# slide types, so the demo reproduces this histogram and its p-value. The
# p-value in the subtitle is computed here, never typed. t.test() stays off
# this figure: the deck names the t-test only after the null world has sunk
# in (issue #165).
#
# Two panels with different axes, so they are composed with side_by_side()
# rather than faceted; it draws them itself and returns invisible(NULL).
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

set.seed(6)
null_deltas <- replicate(1000, {
  shuffled <- sample(borehole$formation)
  group_diff(borehole$temperature_c, shuffled)
})
p_shuffle <- mean(abs(null_deltas) >= abs(delta))

# Left panel: the observed data --------------------------------------------

means <- aggregate(temperature_c ~ formation, data = borehole, FUN = mean)

set.seed(1)
observed <- ggplot(borehole, aes(x = formation, y = temperature_c)) +
  geom_jitter(aes(colour = formation), width = 0.12, height = 0,
              size = 2, alpha = 0.7, show.legend = FALSE) +
  geom_errorbar(data = means,
                aes(ymin = temperature_c, ymax = temperature_c),
                width = 0.45, linewidth = 1.2,
                colour = course_colours[["ink"]]) +
  annotate("segment", x = 1.5, xend = 1.5,
           y = means$temperature_c[means$formation == "Whin Sill"],
           yend = means$temperature_c[means$formation == "Stainmore"],
           colour = course_colours[["red"]], linewidth = 1.1,
           arrow = arrow(ends = "both", length = unit(0.12, "inches"))) +
  scale_colour_manual(values = unname(course_colours[c("ink", "cyan")])) +
  labs(x = NULL, y = "Temperature (°C)",
       title = "The world we sampled",
       subtitle = sprintf("Bars: means. Arrow: \u03b4 = %.1f\u00a0°C", delta))

# Right panel: the null world ----------------------------------------------

# Bin edges fall exactly on +/- delta, so every bar is either wholly in the
# tail or wholly out of it, and the red bars are the p-value's numerator.
width <- abs(delta) / 5
n_bins <- ceiling(max(abs(null_deltas)) / width)
breaks <- width * seq(-n_bins, n_bins)
null_world <- data.frame(delta = null_deltas)
null_world$tail <- abs(null_world$delta) >= abs(delta)

shuffled <- ggplot(null_world, aes(x = delta, fill = tail)) +
  geom_histogram(breaks = breaks, colour = course_colours[["ink"]],
                 linewidth = 0.2, show.legend = FALSE) +
  geom_vline(xintercept = c(-delta, delta), linetype = "dashed",
             colour = course_colours[["red"]], linewidth = 0.8) +
  scale_fill_manual(values = c(`FALSE` = course_fills[["sky"]],
                               `TRUE` = course_colours[["red"]])) +
  scale_y_count() +
  labs(
    x = "Shuffled \u03b4: Stainmore − Whin Sill mean (°C)",
    y = "Shuffles",
    title = "1000 worlds where formation doesn't matter",
    subtitle = bquote(
      .(sprintf("%d of 1000 shuffles as extreme: ", sum(null_world$tail))) *
        italic(p) * .(sprintf(" = %.3f", p_shuffle))
    ),
    caption = "Source: borehole_temp.csv, 35 boreholes per formation"
  )

side_by_side(observed, shuffled, widths = c(0.38, 0.62))
