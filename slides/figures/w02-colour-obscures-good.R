# Week 2 — the honest counterpart to w02-colour-obscures-bad.R.
#
# Same six series. Colour is spent only on the two that carry the story -
# the USA, which supplies most of the pellets, and Russia, whose series
# ends after 2021 - and the other four stay grey as context.
# Labels sit on the lines, so no eye-travel to a legend.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

imports <- read.csv("../data/pellet_imports.csv")
imports$origin <- gsub("_", " ", imports$origin)

highlight <- c("USA", "Russia")
imports$role <- ifelse(imports$origin %in% highlight, imports$origin,
                       "Other origins")
imports$role <- factor(imports$role, levels = c(highlight, "Other origins"))

last_point <- do.call(rbind, lapply(highlight, function(o) {
  rows <- imports[imports$origin == o, ]
  rows[which.max(rows$year), ]
}))

ggplot(imports, aes(x = year, y = import_kt, group = origin, colour = role)) +
  geom_line(aes(linewidth = role)) +
  geom_text(data = last_point, aes(label = origin), hjust = 1.05,
            vjust = -1.1, size = 5, show.legend = FALSE) +
  scale_colour_manual(
    values = unname(course_colours[c("ink", "gold", "concrete")]),
    guide = "none"
  ) +
  scale_linewidth_manual(values = c(1.4, 1.4, 0.7), guide = "none") +
  scale_x_continuous(breaks = seq(2015, 2024, 3)) +
  scale_y_continuous(limits = c(0, 7800), expand = expansion(c(0, 0.02))) +
  labs(
    x = "Year",
    y = "Imports (kt)",
    title = "The USA supplies most of it; Russia stops after 2021",
    subtitle = "Four smaller origins in grey",
    caption = "Source: Forest Research / HMRC"
  )
