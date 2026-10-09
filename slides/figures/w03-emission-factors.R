# Week 3 — the official emission factors.
#
# The chart the students built in Week 2, redrawn so the discussion has
# something on screen. Deliberately the *incomplete* picture: combustion
# only, no supply chain, no payback. Week 4 reuses it as the stimulus for
# "spot the omission", so do not add the other scenarios here.
#
# Week 3 circles the coal figure so the room knows which number the
# discussion is about: options(emission_highlight = "coal") before
# draw_figure(). Week 4 leaves it unset, because circling the answer would
# spoil "spot the omission".
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

factors <- read.csv("../data/emission_factors.csv")
headline <- factors[factors$scenario %in% c("official", "lifecycle"), ]
headline$fuel <- factor(headline$fuel,
                        levels = headline$fuel[order(headline$co2_kg_per_mwh)])

highlight <- getOption("emission_highlight", character(0))
ringed <- headline[headline$fuel %in% highlight, ]
# About one label character, in data units at this figure's width.
char_width <- 0.025 * max(headline$co2_kg_per_mwh)
ringed$label_chars <- nchar(ringed$co2_kg_per_mwh)

ggplot(headline, aes(x = fuel, y = co2_kg_per_mwh)) +
  geom_col(fill = course_colours[["cyan"]], width = 0.7) +
  geom_text(aes(label = co2_kg_per_mwh), hjust = -0.3, size = 5,
            colour = course_colours[["ink"]]) +
  # Centred on the label (which sits 0.3 characters past the bar end) and
  # sized to its width, so a zero and a 910 are both circled cleanly.
  geom_point(data = ringed,
             aes(y = co2_kg_per_mwh + (0.3 + label_chars / 2) * char_width,
                 size = 9 + 3 * label_chars),
             shape = 21, stroke = 1.8,
             fill = NA, colour = course_colours[["red"]]) +
  scale_size_identity() +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    x = NULL,
    y = expression(paste("C", O[2], " (kg per MWh)")),
    title = "Official emission factors for UK electricity",
    caption = "Sources: UNFCCC accounting rules; DESNZ grid carbon factors"
  )
