# Week 6 — the significant result that a log transform makes vanish.
#
# Real data: borehole_temp.csv is right-skewed, and the p-value moves from
# about 0.03 to about 0.22 when you take logs. It is the same dataset the
# live demo tests two slides earlier, which is the point — the test they
# just ran is the one that breaks.
#
# The HolmesCo box beside this slide is about soil permeability. Do not
# relabel these axes to match it; the numbers here are temperatures.
#
# Both p-values are computed, never typed.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

boreholes <- read.csv("../data/borehole_temp.csv")

raw_test <- t.test(temperature_c ~ formation, data = boreholes)
log_test <- t.test(log(temperature_c) ~ formation, data = boreholes)

panels <- rbind(
  data.frame(formation = boreholes$formation,
             scale = "Raw (°C)",
             value = boreholes$temperature_c),
  data.frame(formation = boreholes$formation,
             scale = "log(°C)",
             value = log(boreholes$temperature_c))
)
panels$scale <- factor(panels$scale, levels = c("Raw (°C)", "log(°C)"))

# Formation down the rows, scale across the columns, x freed per column:
# the two formations then share an axis within each scale, which is what
# makes them comparable. Freeing every panel separately would not.
ggplot(panels, aes(x = value)) +
  geom_histogram(bins = 14, fill = course_fills[["sky"]],
                 colour = course_colours[["ink"]], linewidth = 0.2) +
  facet_grid(formation ~ scale, scales = "free_x") +
  labs(
    x = NULL,
    y = "Boreholes",
    title = "Same data, same test, two different conclusions",
    subtitle = sprintf("Raw: p = %.3f, \"significant\". Logged: p = %.2f.",
                       raw_test$p.value, log_test$p.value),
    caption = "Source: borehole_temp.csv, 35 boreholes per formation"
  )
