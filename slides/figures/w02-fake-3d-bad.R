# Week 2 — bad figure: the 3D bar chart.
#
# Deliberately misleading, and the lie is arithmetic rather than artistic:
# each bar is drawn at 90% of the height of the one in front of it, which
# is what "perspective" means in a spreadsheet's 3D chart. Gas (86.7 TWh)
# sits one step behind wind (83.3), so it is drawn at 78.0 and reads as
# the shorter bar. Nuclear (40.6) and bioenergy (40.3) flip the same way.
# Both true gaps are under 1%; the depth cue is ten times larger.
#
# Do not flatten this figure or remove the shrink — w02-fake-3d-good.R is
# the same four numbers drawn honestly, and the pair is the teaching point.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
y2024 <- elec[elec$year == 2024, ]

bars <- data.frame(
  fuel = c("Wind", "Gas", "Bioenergy", "Nuclear"),
  twh = c(y2024$wind_twh, y2024$gas_twh, y2024$bioenergy_twh,
          y2024$nuclear_twh)
)
bars$depth <- seq_len(nrow(bars)) - 1

width <- 0.62
dx <- 0.2
dy <- 2.4
shrink <- 0.9

# Drawn height: foreshortened, then lifted onto a receding baseline.
bars$base <- bars$depth * dy
bars$drawn <- bars$twh * shrink^bars$depth

box <- function(i) {
  b <- bars[i, ]
  x0 <- i
  x1 <- x0 + width
  y0 <- b$base
  y1 <- b$base + b$drawn
  rbind(
    data.frame(id = paste0(i, "-front"), face = "front",
               x = c(x0, x1, x1, x0), y = c(y0, y0, y1, y1)),
    data.frame(id = paste0(i, "-top"), face = "top",
               x = c(x0, x1, x1 + dx, x0 + dx),
               y = c(y1, y1, y1 + dy, y1 + dy)),
    data.frame(id = paste0(i, "-side"), face = "side",
               x = c(x1, x1 + dx, x1 + dx, x1),
               y = c(y0, y0 + dy, y1 + dy, y1))
  )
}

poly <- do.call(rbind, lapply(seq_len(nrow(bars)), box))
poly$face <- factor(poly$face, levels = c("front", "top", "side"))

ggplot(poly, aes(x = x, y = y, group = id, alpha = face)) +
  geom_polygon(fill = course_fills[["sky"]],
               colour = course_colours[["ink"]], linewidth = 0.4) +
  scale_alpha_manual(values = c(front = 1, top = 0.7, side = 0.45),
                     guide = "none") +
  scale_x_continuous(breaks = seq_len(nrow(bars)) + width / 2,
                     labels = bars$fuel) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(c(0, 0.02))) +
  labs(
    x = NULL,
    y = "Generation (TWh)",
    title = "Which generated more in 2024 - gas or wind?",
    caption = "Source: DUKES 2025"
  ) +
  theme(panel.grid.major.x = element_blank())
