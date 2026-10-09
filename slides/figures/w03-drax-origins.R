# Week 3 — where the pellets burned in the UK were shipped from.
#
# Context for w03-energy-input.R: the 95% energy-input bar is Pacific
# Canada, so how much of Drax's fuel is that? Tonnes discharged at UK ports,
# summed over Q2-Q4 2025, by the country of the loading port, as shown on
# the Drax Biomass Tracker (biomasstracker.drax.com, read 2026-09-28).
#
# The tracker publishes no licence or download, so only these five
# aggregates are quoted, typed in by hand with attribution. Drax describes
# the figures as non-authoritative. Canadian pellets come from British
# Columbia and Alberta mills: the report's "Pacific Canada". A further
# 10 kt (0.2%) loaded in the Netherlands and Portugal is left out; shares are
# of the four countries shown.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

origins <- data.frame(
  port = c("United States", "Latvia", "Canada", "Estonia"),
  kt = c(4371, 911, 648, 110)
)
origins$share <- origins$kt / sum(origins$kt)
origins$port <- factor(origins$port, levels = rev(origins$port))
origins$highlight <- origins$port == "Canada"

ggplot(origins, aes(x = kt, y = port, fill = highlight)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(100 * share), "\u00a0%")),
            hjust = -0.2, size = 5, colour = course_colours[["ink"]]) +
  scale_fill_manual(values = c(`FALSE` = course_colours[["concrete"]],
                               `TRUE` = course_colours[["red"]]),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Pellets unloaded at UK ports, Apr-Dec 2025 (thousand tonnes)",
    y = NULL,
    title = "Drax pellets unloaded in the UK, by loading country",
    caption = "Source: Drax Biomass Tracker, Q2-Q4 2025 (read 2026-09-28)"
  )
