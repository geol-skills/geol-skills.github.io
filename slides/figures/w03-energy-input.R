# Week 3 — how much non-biomass energy it takes to make a MWh of electricity.
#
# Redrawn from Figure 4 of Stephenson & MacKay (2014), "Life Cycle Impacts
# of Biomass Electricity in 2020", DECC URN 14D/243. Ranges are digitized
# from the published bar chart; the bioenergy extremes (EC 0.13-0.81,
# PE 0.25-0.95) match the report's Table 8. Coal, gas and nuclear are
# drawn as thin bars because the report shows them as near-points.
#
# EIR = non-bioenergy energy input / electricity delivered, counted as
# primary energy (the report's red bars). The report's energy-carrier (blue)
# bars are omitted: its comparator technologies come from studies that count
# primary energy, so only the red bars compare like with like.
#
# Contains public sector information licensed under the Open Government
# Licence v3.0.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

eir <- data.frame(
  source = c("Biomass: South US residues", "Biomass: Pacific Canada residues",
             "Nuclear (PWR)", "UK hard coal", "Russian hard coal",
             "Natural gas", "Wind", "Solar PV, UK", "Solar PV, S. Spain"),
  low  = c(0.25, 0.25, 0.01, 0.04, 0.08, 0.03, 0.04, 0.15, 0.07),
  high = c(0.76, 0.95, 0.02, 0.05, 0.09, 0.04, 0.10, 0.31, 0.15)
)
eir$source <- factor(eir$source, levels = rev(eir$source))
eir$highlight <- eir$source == "Biomass: Pacific Canada residues"

ggplot(eir, aes(y = source)) +
  geom_linerange(aes(xmin = low, xmax = high, colour = highlight),
                 linewidth = 7) +
  scale_colour_manual(values = c(`FALSE` = course_colours[["concrete"]],
                                 `TRUE` = course_colours[["red"]]),
                      guide = "none") +
  geom_vline(xintercept = 1, linetype = "dashed",
             colour = course_colours[["ink"]]) +
  annotate("text", x = 1, y = 5, hjust = 1.05, size = 4.5,
           label = "1\u00a0MWh in per MWh out",
           colour = course_colours[["ink"]]) +
  scale_x_continuous(labels = function(x) paste0(x * 100, "\u00a0%"),
                     limits = c(0, 1.05), expand = expansion(0)) +
  labs(
    x = "Non-biomass energy input per unit of electricity delivered (EIR)",
    y = NULL,
    title = "Energy in, per unit of electricity out",
    caption = paste("Source: Stephenson & MacKay (2014), DECC, Fig. 4;",
                    "Open Government Licence v3.0")
  )
