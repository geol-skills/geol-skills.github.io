# Week 3 — the official emission factors, biomass among them.
#
# The chart the students built in Week 2, redrawn so the discussion has
# something on screen. Deliberately the *incomplete* picture: combustion
# only, no supply chain, no payback. Week 4 reuses it as the stimulus for
# "spot the omission", so do not add the other scenarios here.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

factors <- read.csv("../data/emission_factors.csv")
headline <- factors[factors$scenario %in% c("official", "lifecycle"), ]
headline$fuel <- factor(headline$fuel,
                        levels = headline$fuel[order(headline$co2_kg_per_mwh)])

ggplot(headline, aes(x = fuel, y = co2_kg_per_mwh)) +
  geom_col(fill = course_colours[["cyan"]], width = 0.7) +
  geom_text(aes(label = co2_kg_per_mwh), hjust = -0.3, size = 5,
            colour = course_colours[["ink"]]) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    x = NULL,
    y = expression(paste("C", O[2], " (kg per MWh)")),
    title = "Official emission factors for UK electricity",
    caption = "Sources: UNFCCC accounting rules; DESNZ grid carbon factors"
  )
