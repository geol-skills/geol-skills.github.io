# Week 2 — the honest counterpart to w02-fake-3d-bad.R.
#
# Same four fuels, same order, no depth. Gas edges wind and nuclear edges
# bioenergy — both rankings the reverse of what the 3D version showed.
# Values are printed because the gaps are under 1% and no bar chart can
# carry a 1% difference on its own.
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
bars$fuel <- factor(bars$fuel, levels = bars$fuel)

ggplot(bars, aes(x = fuel, y = twh)) +
  geom_col(fill = course_fills[["sky"]], colour = course_colours[["ink"]],
           linewidth = 0.4, width = 0.62) +
  geom_text(aes(label = sprintf("%.1f", twh)), vjust = -0.5, size = 4.6,
            colour = course_colours[["ink"]]) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(c(0, 0.02))) +
  labs(
    x = NULL,
    y = "Generation (TWh)",
    title = sprintf("Gas, by %.1f TWh",
                    y2024$gas_twh - y2024$wind_twh),
    caption = "Source: DUKES 2025"
  ) +
  theme(panel.grid.major.x = element_blank())
