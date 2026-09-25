# Week 2 — the honest counterpart to w02-double-counted-pie.R.
#
# The categories in the pie overlapped, so the fix is not a tidier pie: it
# is a bar chart, where "low carbon" becomes a shading of exclusive
# categories rather than a slice competing with them. Bars now sum to the
# 285 TWh actually generated.
#
# "Other" is total generation minus the named fuels, computed rather than
# asserted, so the bars account for every TWh in the table.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
y2024 <- elec[elec$year == 2024, ]

named <- c(Gas = y2024$gas_twh, Wind = y2024$wind_twh,
           Nuclear = y2024$nuclear_twh, Bioenergy = y2024$bioenergy_twh,
           Solar = y2024$solar_twh, Coal = y2024$coal_twh)

shares <- data.frame(
  source = c(names(named), "Other"),
  twh = c(unname(named), y2024$total_twh - sum(named)),
  carbon = c("Fossil", "Low carbon", "Low carbon", "Low carbon",
             "Low carbon", "Fossil", "Other")
)
shares <- shares[order(shares$twh), ]
shares$source <- factor(shares$source, levels = shares$source)

ggplot(shares, aes(x = twh, y = source, fill = carbon)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", twh)), hjust = -0.2, size = 4.4,
            colour = course_colours[["ink"]]) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(c(0, 0.02))) +
  scale_fill_manual(values = course_palette(3)) +
  labs(
    x = "Generation (TWh)",
    y = NULL,
    fill = NULL,
    title = "UK electricity by source, 2024",
    subtitle = sprintf("Exclusive categories, summing to %.0f TWh",
                       y2024$total_twh),
    caption = "Source: DUKES 2025"
  )
