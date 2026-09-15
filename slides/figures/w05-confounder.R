# Week 5 — a confounder, before and after stratifying.
#
# Same points in both panels. Pooled, temperature rises convincingly with
# depth; split by geothermal region, every within-region trend is flat.
# Depth never caused anything — the deep boreholes are simply the ones
# drilled in hot ground.
#
# Simulated: borehole_temp.csv has no depth column, and the whole point is
# a variable correlated with both depth and temperature.
#
# Both slopes are fitted by lm() through geom_smooth, so neither trend is
# drawn by hand.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

set.seed(2502)
per_region <- 30

regions <- data.frame(
  region = c("Cool basin", "Warm margin", "Geothermal field"),
  mean_depth = c(500, 1000, 1500),
  mean_temp = c(18, 34, 52)
)

boreholes <- do.call(rbind, lapply(seq_len(nrow(regions)), function(i) {
  data.frame(
    region = regions$region[i],
    depth = rnorm(per_region, regions$mean_depth[i], 90),
    temperature = rnorm(per_region, regions$mean_temp[i], 3)
  )
}))
boreholes$region <- factor(boreholes$region, levels = regions$region)

pooled <- boreholes
pooled$panel <- "Ignoring region"
split_out <- boreholes
split_out$panel <- "Split by region"
both <- rbind(pooled, split_out)
both$panel <- factor(both$panel,
                     levels = c("Ignoring region", "Split by region"))

ggplot(both, aes(x = depth, y = temperature)) +
  geom_point(aes(colour = ifelse(panel == "Split by region",
                                 as.character(region), "All boreholes")),
             size = 2) +
  geom_smooth(aes(group = ifelse(panel == "Split by region",
                                 as.character(region), "all")),
              method = "lm", formula = y ~ x, se = FALSE,
              colour = course_colours[["ink"]], linewidth = 1) +
  facet_wrap(~panel) +
  scale_colour_manual(
    values = c("All boreholes" = course_colours[["concrete"]],
               "Cool basin" = course_colours[["cyan"]],
               "Warm margin" = course_colours[["purple"]],
               "Geothermal field" = course_colours[["gold"]]),
    breaks = regions$region,
    name = NULL
  ) +
  labs(
    x = "Depth (m)",
    y = "Temperature (°C)",
    title = "The same boreholes, with and without the confounder",
    subtitle = paste("Depth predicts temperature only until you ask",
                     "where the hole was drilled"),
    caption = "Simulated boreholes, 30 per region"
  ) +
  theme(legend.position = "bottom")
