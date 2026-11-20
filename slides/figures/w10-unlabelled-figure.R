# Week 10 — Pitfall 2: the figure a reader cannot interpret.
#
# Deliberately bad. No axis titles, no units, no title, no caption: the
# reader is shown a line that rises and told nothing about what it measures.
# Do not "fix" it — w10-labelled-figure.R is the same series drawn properly,
# and the pitfall only lands as a contrast with it.
#
# theme.R is not sourced, and that is the point: this is a plot pasted
# straight out of someone's console, default grey theme and all.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

elec <- read.csv("../data/uk_electricity.csv")

ggplot(elec, aes(x = year, y = bioenergy_twh)) +
  geom_line(linewidth = 1.2) +
  labs(x = NULL, y = NULL) +
  theme_grey(base_size = 16)
