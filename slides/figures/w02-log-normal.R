# Week 2 — why you log-transform: the same permeabilities, raw and logged.
#
# Simulated rather than taken from borehole_temp.csv, which data/README.md
# reserves for Week 6: showing it already log-transformed here would
# pre-answer the exercise where students discover the skew themselves.
#
# Every number in the labels is computed from the sample. The mean's
# percentile is the point of the left panel — it is not a typical value,
# it is a value larger than most of the data.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

set.seed(2202)
permeability <- rlnorm(2000, meanlog = log(5), sdlog = 2)

centres <- data.frame(
  panel = rep(c("Raw (mD)", "log10(mD)"), each = 2),
  statistic = rep(c("median", "mean"), 2),
  value = c(
    median(permeability), mean(permeability),
    median(log10(permeability)), mean(log10(permeability))
  )
)
centres$panel <- factor(centres$panel, levels = c("Raw (mD)", "log10(mD)"))

# The raw panel is cut at the 99th percentile. Left whole, a handful of
# samples in the thousands squash the other 99% into a single bar against
# the axis — the tail would be the only thing you could see. The count cut
# is named in the caption rather than quietly dropped.
raw_limit <- quantile(permeability, 0.99)
raw_shown <- permeability[permeability <= raw_limit]

samples <- rbind(
  data.frame(panel = "Raw (mD)", value = raw_shown),
  data.frame(panel = "log10(mD)", value = log10(permeability))
)
samples$panel <- factor(samples$panel, levels = c("Raw (mD)", "log10(mD)"))

mean_percentile <- 100 * mean(permeability < mean(permeability))

ggplot(samples, aes(x = value)) +
  geom_histogram(bins = 40, fill = course_fills[["sky"]],
                 colour = course_colours[["ink"]], linewidth = 0.2) +
  geom_vline(
    data = centres,
    aes(xintercept = value, colour = statistic, linetype = statistic),
    linewidth = 1
  ) +
  facet_wrap(~panel, scales = "free") +
  scale_colour_manual(
    values = c(mean = course_colours[["red"]],
               median = course_colours[["purple"]]),
    name = NULL
  ) +
  scale_linetype_manual(values = c(mean = "dashed", median = "solid"),
                        name = NULL) +
  labs(
    x = NULL,
    y = "Number of samples",
    title = "Taking logs turns a skew into a shape you can summarise",
    subtitle = sprintf(
      "Mean %.0f mD is larger than %.0f%% of the samples; median is %.1f mD",
      mean(permeability), mean_percentile, median(permeability)
    ),
    caption = sprintf(
      paste("Simulated log-normal permeabilities, n = 2000.",
            "Left panel cut at %.0f mD; %d samples lie beyond it."),
      raw_limit, sum(permeability > raw_limit)
    )
  ) +
  theme(legend.position = "bottom")
