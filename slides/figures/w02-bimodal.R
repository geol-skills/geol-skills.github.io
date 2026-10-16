# Week 2 — the bimodal panel from the Shape slide, on its own.
#
# Sits beside the camel on "The two-humped problem": mean and median both
# fall in the empty valley between the humps. It reuses the data and centres
# of w02-distribution-shapes.R (from w02-shapes-data.R), so the two slides
# show the same sample.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")
source("figures/w02-shapes-data.R", local = TRUE)

bimodal <- shapes[shapes$shape == "Bimodal", ]
bimodal_centres <- centres[centres$shape == "Bimodal", ]

ggplot(bimodal, aes(x = value)) +
  geom_histogram(bins = 45, fill = course_fills[["sky"]],
                 colour = course_colours[["ink"]], linewidth = 0.2) +
  geom_vline(data = bimodal_centres,
             aes(xintercept = value, colour = statistic, linetype = statistic),
             linewidth = 1) +
  scale_colour_manual(values = c(mean = course_colours[["red"]],
                                 median = course_colours[["purple"]]),
                      name = NULL) +
  scale_linetype_manual(values = c(mean = "dashed", median = "solid"),
                        name = NULL) +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom"
  )
