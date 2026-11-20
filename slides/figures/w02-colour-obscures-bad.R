# Week 2 — bad figure: colour that obscures.
#
# Deliberately bad. Six origins, six rainbow hues, one thin line each and
# a legend in alphabetical order, so reading the chart means bouncing
# between the key and the panel for every series. Rainbow hues are not
# ordered and not equally distinguishable - the yellow-green pair is the
# worst of it, and red-green defeats the commonest colour vision
# deficiency outright.
#
# rainbow() sidesteps the contrast guard in theme.R, which only checks
# course_colours. That is deliberate here and nowhere else.
#
# The fix is w02-colour-obscures-good.R: same six series, colour spent on
# the two that carry the story.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

imports <- read.csv("../data/pellet_imports.csv")
imports$origin <- gsub("_", " ", imports$origin)

ggplot(imports, aes(x = year, y = import_kt, colour = origin)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = rainbow(length(unique(imports$origin)))) +
  scale_x_continuous(breaks = seq(2015, 2024, 3)) +
  scale_y_continuous(limits = c(0, 7800), expand = expansion(c(0, 0.02))) +
  labs(
    x = "Year",
    y = "Imports (kt)",
    colour = "Origin",
    title = "UK wood pellet imports by country of origin",
    caption = "Source: Forest Research / HMRC"
  )
