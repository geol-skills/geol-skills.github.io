# Week 7 — between-group variation, relative to within.
#
# Left: the wind sites as measured. Right: the same four group means,
# with every site twice as far from its own group mean. Only the
# scatter within each region changes, and with it F. That is the whole
# content of "between relative to within".
#
# Twice, not more: at three times some sites would fall below zero.
#
# Same data and region order as the "Same data, four ways" sequence, so
# the reader is looking at sites they have already seen.
#
# Points rather than boxplots: the F-ratio is about how far points sit
# from their group mean, and a boxplot hides exactly that.
#
# Each F comes from aov() on the panel's own data.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

wind <- read.csv("../data/wind_output.csv")
region_order <- names(sort(tapply(wind$capacity_pct, wind$region, mean)))

group_mean <- ave(wind$capacity_pct, wind$region)
residual <- wind$capacity_pct - group_mean

f_of <- function(d) {
  summary(aov(capacity_pct ~ region, data = d))[[1]][["F value"]][1]
}

scatter <- c("Our sites" = 1, "Same means, twice the scatter" = 2)
panels <- do.call(rbind, lapply(names(scatter), function(name) {
  d <- data.frame(region = wind$region,
                  capacity_pct = group_mean + scatter[[name]] * residual)
  # A plotmath string, so the facet strip can set F in italics.
  d$panel <- sprintf("\"%s: \" * italic(F) == %.1f", name, f_of(d))
  d
}))
panels$region <- factor(panels$region, levels = region_order)
panels$panel <- factor(panels$panel, levels = unique(panels$panel))

centres <- unique(data.frame(panel = panels$panel, region = panels$region,
                             capacity_pct = ave(panels$capacity_pct,
                                                panels$panel, panels$region)))

set.seed(2701)
ggplot(panels, aes(x = region, y = capacity_pct)) +
  geom_point(position = position_jitter(width = 0.18, height = 0),
             colour = course_colours[["cyan"]], size = 2, alpha = 0.8) +
  geom_segment(data = centres,
               aes(x = as.integer(region) - 0.3,
                   xend = as.integer(region) + 0.3,
                   y = capacity_pct, yend = capacity_pct),
               colour = course_colours[["ink"]], linewidth = 1.1) +
  scale_x_discrete(labels = function(x) sub(" ", "\n", x)) +
  facet_wrap(~panel, labeller = label_parsed) +
  labs(
    x = NULL,
    y = "Capacity factor (%)",
    title = "The same four group means, twice",
    subtitle = paste("Between-group variation is identical.",
                     "Only the scatter within each region changes."),
    caption = "Right panel: each site moved twice as far from its region's mean"
  )
