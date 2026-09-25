# Week 3 app — the same megawatt-hour of biomass, six ways.
#
# Every bar is one accounting choice from emission_factors.csv; nothing is
# modelled here. The spread between the shortest and tallest bar is the
# session's whole question: how much of the answer comes from the data,
# and how much from what you decided to count.
#
# Note for anyone extending this: the coal line falls *inside* the biomass
# range, and that crossing is the point of the figure. Biomass at the chimney
# is 1000 kg/MWh against coal's 910, so the two rightmost bars clear the coal
# line while the regrowth scenarios sit far below it. Keep the coal reference
# line visible and do not clip the top bars — whether biomass beats coal is
# decided by the accounting choice, not by the fuel.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

factors <- read.csv("../data/emission_factors.csv")
biomass <- factors[factors$fuel == "biomass", ]

readable <- c(
  official = "Official rules\n(counted as zero)",
  payback_100yr = "Regrowth over\n100 years",
  payback_40yr = "Regrowth over\n40 years",
  payback_20yr = "Regrowth over\n20 years",
  combustion_only = "Chimney only,\nno regrowth",
  with_supply_chain = "Chimney plus\nsupply chain"
)
biomass$label <- factor(readable[biomass$scenario], levels = readable)

coal <- factors$co2_kg_per_mwh[factors$fuel == "coal" &
                                 factors$scenario == "official"]
gas <- factors$co2_kg_per_mwh[factors$fuel == "gas" &
                                factors$scenario == "official"]

ggplot(biomass, aes(x = label, y = co2_kg_per_mwh)) +
  geom_hline(yintercept = coal, colour = course_colours[["red"]],
             linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = gas, colour = course_colours[["concrete"]],
             linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = -Inf, y = coal, vjust = -0.6, hjust = -0.05,
           size = 4.2, colour = course_colours[["red"]],
           label = sprintf("Coal: %d", coal)) +
  annotate("text", x = -Inf, y = gas, vjust = -0.6, hjust = -0.05,
           size = 4.2, colour = course_colours[["black"]],
           label = sprintf("Gas: %d", gas)) +
  geom_col(fill = course_colours[["cyan"]], width = 0.7) +
  geom_text(aes(label = co2_kg_per_mwh), vjust = -0.5, size = 4.5,
            colour = course_colours[["ink"]]) +
  # The tallest bar exceeds coal, so the limit must track the bars rather
  # than the reference line. coord_cartesian clips the view; ylim() would
  # drop the out-of-range bars entirely.
  coord_cartesian(ylim = c(0, max(biomass$co2_kg_per_mwh) * 1.12)) +
  labs(
    x = NULL,
    y = expression(paste("Net C", O[2], " (kg per MWh)")),
    title = "One megawatt-hour of biomass, under six accounting choices",
    subtitle = sprintf(
      "The same fuel, the same chimney: %d kg apart on assumptions alone",
      max(biomass$co2_kg_per_mwh) - min(biomass$co2_kg_per_mwh)
    ),
    caption = "Source: emission_factors.csv"
  )
