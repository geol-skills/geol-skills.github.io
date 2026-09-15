# Week 2 — the honest counterpart to w02-truncated-axis.R.
#
# Same bars, same years, baseline at zero. The fall from 2019 to 2024 is
# about 13%, which is what a bar chart should look like when it is 13%.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
recent <- elec[elec$year >= 2019, ]

drop_pct <- 100 * (1 - recent$total_twh[nrow(recent)] / recent$total_twh[1])

ggplot(recent, aes(x = factor(year), y = total_twh)) +
  geom_col(fill = course_colours[["cyan"]], width = 0.7) +
  scale_y_continuous(limits = c(0, 340), expand = expansion(c(0, 0.02))) +
  labs(
    x = "Year",
    y = "Total generation (TWh)",
    title = "UK electricity generation, 2019-2024",
    subtitle = sprintf("Down %.0f%% since 2019", drop_pct),
    caption = "Source: DUKES 2025"
  )
