# Week 2 — the honest counterpart to w02-colour-obscures-bad.R.
#
# Same six series. Colour is spent only on the two that carry the story -
# the USA, which supplies most of the pellets, and Russia, whose series
# ends after 2021 - and the other four stay grey as context.
# Each highlighted series gets its own colour; grey is reserved for context.
# Gold sat too close to the grey, so Russia is cyan. Labels sit next to their
# lines, so no eye-travel to a legend; Russia's is lifted clear of the grey
# cluster, and a dot marks where its series stops.
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

context <- imports[imports$role == "Other origins", ]
story <- imports[imports$role != "Other origins", ]
russia_end <- last_point[last_point$origin == "Russia", ]
usa_end <- last_point[last_point$origin == "USA", ]

ggplot(imports, aes(x = year, y = import_kt, group = origin, colour = role)) +
  geom_line(data = context, linewidth = 0.7) +
  geom_line(data = story, linewidth = 1.4) +
  geom_point(data = russia_end, size = 3) +
  geom_text(data = usa_end, aes(label = origin), hjust = 1.05,
            vjust = -1.1, size = 5, show.legend = FALSE) +
  annotate("segment", x = russia_end$year, xend = russia_end$year,
           y = russia_end$import_kt + 250, yend = 1550,
           colour = course_colours[["cyan"]], linewidth = 0.5) +
  annotate("text", x = russia_end$year, y = 1700, label = "Russia",
           vjust = 0, size = 5, colour = course_colours[["cyan"]]) +
  scale_colour_manual(
    values = unname(course_colours[c("ink", "cyan", "concrete")]),
    drop = FALSE, guide = "none"
  ) +
  scale_x_continuous(breaks = seq(2015, 2024, 3)) +
  scale_y_continuous(limits = c(0, 7800), expand = expansion(c(0, 0.02))) +
  labs(
    x = "Year",
    y = "Imports (kt)",
    title = "The USA supplies most of it; Russia stops after 2021",
    caption = "Source: Forest Research / HMRC"
  )
