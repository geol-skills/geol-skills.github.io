# Week 2 — the honest counterpart to w02-area-encoding-bad.R.
#
# The same two tonnages encoded as length from a zero baseline, which is
# the one channel the eye reads accurately, with an axis to check them
# against. The rise is 42%, not the doubling the squares implied.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

imports <- read.csv("../data/pellet_imports.csv")
annual <- tapply(imports$import_kt, imports$year, sum)

shown <- c("2015", "2024")
bars <- data.frame(year = shown, kt = as.numeric(annual[shown]))
change_pct <- 100 * (bars$kt[2] / bars$kt[1] - 1)

ggplot(bars, aes(x = year, y = kt / 1000)) +
  geom_col(fill = course_fills[["stone"]],
           colour = course_colours[["ink"]], linewidth = 0.5, width = 0.5) +
  geom_text(aes(label = sprintf("%.1f Mt", kt / 1000)), vjust = -0.5,
            size = 5, colour = course_colours[["ink"]]) +
  scale_y_continuous(limits = c(0, 12), expand = expansion(c(0, 0.02))) +
  labs(
    x = NULL,
    y = "Pellet imports (Mt)",
    title = sprintf("UK wood pellet imports rose %.0f%%", change_pct),
    caption = "Source: Forest Research / HMRC"
  ) +
  theme(panel.grid.major.x = element_blank())
