# Week 8 — "Plotting a comparison": 2b. notched box plot.
#
# w08-groups-box.R with notches. Each notch spans median ± 1.58 × IQR / √n,
# roughly a 95 % interval for the median: where two notches don't overlap,
# the medians probably differ. Here the two northern regions' notches clear
# the two southern regions'; within each pair they overlap.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

wind <- read.csv("../data/wind_output.csv")

region_order <- names(sort(tapply(wind$capacity_pct, wind$region, mean)))
wind$region <- factor(wind$region, levels = region_order)

ggplot(wind, aes(x = region, y = capacity_pct)) +
  geom_boxplot(
    notch = TRUE,
    fill = course_fills[["sky"]],
    colour = course_colours[["ink"]]
  ) +
  labs(
    x = NULL,
    y = "Capacity factor (%)",
    title = "2b. Notched box plot",
    subtitle = "Notches that don't overlap: medians probably differ",
    caption = "wind_output.csv, 25 sites per region"
  )
