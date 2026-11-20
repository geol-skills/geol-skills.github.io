# Week 2 — bad figure: the misleading area encoding.
#
# Deliberately misleading. Each circle's DIAMETER is drawn in proportion to
# the tonnage, so the area - what the eye actually reads - goes up with
# the square of the ratio. Imports rose 42%, and the 2024 circle covers
# just over twice the ink of the 2015 one. The circles share a centre line,
# not a baseline, so they cannot be read as bars; the infographic style,
# with no axis to check it against, is part of the trick.
#
# Do not switch the diameters to sqrt(value): that is the fix, and the
# honest version lives in w02-area-encoding-good.R.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

imports <- read.csv("../data/pellet_imports.csv")
annual <- tapply(imports$import_kt, imports$year, sum)

shown <- c("2015", "2024")
kt <- as.numeric(annual[shown])

# Diameter proportional to value - the error being illustrated.
radius <- 1.3 * kt / kt[1]
centre <- c(1.6, 1.6 + radius[1] + radius[2] + 1.4)

theta <- seq(0, 2 * pi, length.out = 181)
circles <- do.call(rbind, lapply(seq_along(shown), function(i) {
  data.frame(year = shown[i],
             x = centre[i] + radius[i] * cos(theta),
             y = radius[i] * sin(theta))
}))
labels <- data.frame(centre = centre, year = shown, kt = kt,
                     y = -max(radius) - 0.45)

ggplot() +
  geom_polygon(data = circles, aes(x = x, y = y, group = year),
               fill = course_fills[["stone"]],
               colour = course_colours[["ink"]], linewidth = 0.5) +
  geom_text(data = labels,
            aes(x = centre, y = y,
                label = sprintf("%s\n%.1f\u00a0Mt", year, kt / 1000)),
            size = 5, lineheight = 0.95,
            colour = course_colours[["ink"]]) +
  coord_fixed(xlim = c(-1.7, 10), ylim = c(-2.5, 2), clip = "off") +
  labs(
    title = "UK wood pellet imports have doubled",
    caption = "Source: Forest Research / HMRC"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank()
  )
