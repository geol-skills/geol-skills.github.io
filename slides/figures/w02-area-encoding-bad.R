# Week 2 — bad figure: the misleading area encoding.
#
# Deliberately misleading. Each square's SIDE is drawn in proportion to
# the tonnage, so the area - what the eye actually reads - goes up with
# the square of the ratio. Imports rose 42%, and the 2024 square covers
# just over twice the ink of the 2015 one. The infographic style, with no
# axis to check it against, is part of the trick.
#
# Do not switch the sides to sqrt(value): that is the fix, and it lives in
# w02-area-encoding-good.R.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

imports <- read.csv("../data/pellet_imports.csv")
annual <- tapply(imports$import_kt, imports$year, sum)

shown <- c("2015", "2024")
kt <- as.numeric(annual[shown])

# Side proportional to value - the error being illustrated.
side <- 2.6 * kt / kt[1]
centre <- c(1.6, 1.6 + side[1] / 2 + side[2] / 2 + 1.4)

squares <- data.frame(
  year = shown,
  xmin = centre - side / 2,
  xmax = centre + side / 2,
  ymin = 0,
  ymax = side
)

ggplot(squares) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = course_fills[["stone"]],
            colour = course_colours[["ink"]], linewidth = 0.5) +
  geom_text(aes(x = centre, y = -0.45,
                label = sprintf("%s\n%.1f Mt", year, kt / 1000)),
            size = 5, lineheight = 0.95,
            colour = course_colours[["ink"]]) +
  coord_fixed(xlim = c(-1.7, 10), ylim = c(-1.3, 4.2), clip = "off") +
  labs(
    title = "UK wood pellet imports have doubled",
    caption = "Source: Forest Research / HMRC"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank()
  )
