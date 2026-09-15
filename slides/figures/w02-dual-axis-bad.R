# Week 2 — bad figure: two y-axes, independently chosen.
#
# Deliberately misleading. The left axis runs 0-120 TWh so generation
# hugs the floor; the right axis runs 6000-9500 kt, a truncated window
# that magnifies every wobble in imports. Nothing is falsified, yet the
# chart says "imports are running away from generation" when the two
# series in fact move together within a few percent (see the "good"
# counterpart, w02-dual-axis-good.R).
#
# Do not rescale the axes to agree: the arbitrary pairing is the lie.
#
# Imports are summed across origins; 2015-2023 country shares are
# estimates (see data/README.md), but annual totals are firm.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")
imports <- read.csv("../data/pellet_imports.csv")

annual <- tapply(imports$import_kt, imports$year, sum)
series <- data.frame(
  year = as.integer(names(annual)),
  imports_kt = as.numeric(annual)
)
series$bioenergy_twh <- elec$bioenergy_twh[match(series$year, elec$year)]

# Right axis spans 6000-9500 kt; left spans 0-120 TWh. This maps one onto
# the other, and sec_axis() inverts it for the right-hand labels.
twh_max <- 120
kt_min <- 6000
kt_span <- 3500
series$imports_on_left <- (series$imports_kt - kt_min) / kt_span * twh_max

ggplot(series, aes(x = year)) +
  geom_line(aes(y = bioenergy_twh, colour = "Generation (TWh)"),
            linewidth = 1.4) +
  geom_line(aes(y = imports_on_left, colour = "Pellet imports (kt)"),
            linewidth = 1.4, linetype = "longdash") +
  scale_y_continuous(
    name = "Bioenergy generation (TWh)",
    limits = c(0, twh_max),
    sec.axis = sec_axis(~ . / twh_max * kt_span + kt_min,
                        name = "Pellet imports (kt)")
  ) +
  scale_x_continuous(breaks = seq(2015, 2024, 3)) +
  scale_colour_manual(values = unname(course_colours[c("purple", "gold")])) +
  labs(
    x = "Year",
    colour = NULL,
    title = "Imports soar while generation stands still",
    caption = "Sources: DUKES 2025; Forest Research / HMRC"
  ) +
  theme(legend.position = "top")
