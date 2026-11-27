# Week 7 — UK electricity from gas: a straight line and a curve.
#
# Polished version of the first live-coded slides. Both models are lm() on
# the annual DUKES series; the bands are 95 % confidence intervals for the
# fitted line (where the line could be), not for next year's value.
#
# The curve is a degree-4 polynomial, poly(year, 4). Neither model explains
# the 2012–15 trough, when cheap coal pushed gas off the grid: the curve
# only bends towards it. Time is not the cause; the models have nothing
# else to go on.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

elec <- read.csv("../data/uk_electricity.csv")

line <- lm(gas_twh ~ year, data = elec)
curve <- lm(gas_twh ~ poly(year, 4), data = elec)

fits <- rbind(
  cbind(elec["year"], model = "Straight line",
        predict(line, elec, interval = "confidence")),
  cbind(elec["year"], model = "Curve",
        predict(curve, elec, interval = "confidence"))
)
fits$model <- factor(fits$model, c("Straight line", "Curve"))

ggplot(fits, aes(x = year)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = model), alpha = 0.25) +
  geom_line(aes(y = fit, colour = model), linewidth = 1.2) +
  geom_point(data = elec, aes(y = gas_twh), size = 2.2,
             colour = course_colours[["ink"]]) +
  scale_colour_manual(values = course_palette(3)[2:3], name = NULL) +
  scale_fill_manual(values = course_palette(3)[2:3], name = NULL) +
  labs(x = "Year", y = "Electricity from gas (TWh)",
       caption = paste("Lines: lm() fits; bands: 95 % CI.",
                       "Data: DUKES 2026 Table 5.6.B")) +
  theme(legend.position = "top")
