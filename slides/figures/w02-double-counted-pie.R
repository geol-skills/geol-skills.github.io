# Week 2 — "Spot the lie", chart 3: the double-counting pie chart.
#
# Deliberately misleading: "Low carbon" repeats wind, nuclear, solar and
# bioenergy, so the slices sum to ~446 TWh against an actual 285 TWh (156%).
# A pie must show parts of a whole.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
y2024 <- elec[elec$year == 2024, ]

shares <- data.frame(
  source = c("Gas", "Wind", "Nuclear", "Solar", "Bioenergy", "Coal",
             "Low carbon"),
  twh = c(y2024$gas_twh, y2024$wind_twh, y2024$nuclear_twh,
          y2024$solar_twh, y2024$bioenergy_twh, y2024$coal_twh,
          y2024$wind_twh + y2024$nuclear_twh + y2024$solar_twh +
            y2024$bioenergy_twh)
)
shares$source <- factor(shares$source, levels = shares$source)

ggplot(shares, aes(x = "", y = twh, fill = source)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = course_palette(7)) +
  labs(
    title = "UK electricity by source, 2024",
    fill = NULL,
    caption = "Source: DUKES 2025"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    # theme_slide() sets panel.grid.major, which beats a plain panel.grid
    panel.grid.major = element_blank()
  )
