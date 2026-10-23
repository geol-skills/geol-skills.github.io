# Week 3 app — the same megawatt-hour of biomass, six ways.
#
# Like the official rules, every bar but the last leaves out the supply
# chain (harvesting, pelletizing, shipping); the last bar adds it, to show
# what it is worth. The regrowth bars are the chimney's CO2 less what a
# loblolly pine plantation, felled at 40 and replanted, has taken back
# after 20, 40 and 90 years, from fraction_reabsorbed(). Felling at 40
# makes the 40-year bar the moment the new stand matches the old: zero.
#
# The reference lines are coal's and gas's official (combustion only)
# factors, on the same basis as the bars. Biomass at the chimney (1000)
# clears coal (910); the 20-year regrowth bar sits below gas. Keep both
# lines visible and do not clip the bars — whether biomass beats a fossil
# fuel is decided by the accounting choice, not by the fuel.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")
source("../data/fraction_reabsorbed.R")

factors <- read.csv("../data/emission_factors.csv")
factor_of <- function(fuel, scenario) {
  factors$co2_kg_per_mwh[factors$fuel == fuel & factors$scenario == scenario]
}

felled_at <- 40
supply_chain <- factor_of("biomass", "with_supply_chain")
chimney <- factor_of("biomass", "combustion_only")
# The function reads data/forest_growth.csv, a path from the repo's top
# folder, as students' code does.
regrown <- local({
  old <- setwd("..")
  on.exit(setwd(old))
  chimney *
    (1 - fraction_reabsorbed(c(90, 40, 20), "loblolly_plantation", felled_at))
})

biomass <- data.frame(
  label = c("Official rules\n(counted as zero)",
            "90 years'\nregrowth", "40 years'\nregrowth",
            "20 years'\nregrowth", "Chimney only,\nno regrowth",
            "Chimney plus\nsupply chain"),
  co2_kg_per_mwh = round(c(factor_of("biomass", "official"), regrown,
                           chimney, supply_chain))
)
biomass$label <- factor(biomass$label, levels = biomass$label)

coal <- factor_of("coal", "official")
gas <- factor_of("gas", "official")

ggplot(biomass, aes(x = label, y = co2_kg_per_mwh)) +
  geom_hline(yintercept = 0, colour = course_colours[["concrete"]]) +
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
  geom_text(aes(label = sub("-", "−", co2_kg_per_mwh),
                vjust = ifelse(co2_kg_per_mwh < 0, 1.5, -0.5)),
            size = 4.5, colour = course_colours[["ink"]]) +
  # The tallest bar exceeds coal, so the limit must track the bars rather
  # than the reference line. coord_cartesian clips the view; ylim() would
  # drop the out-of-range bars entirely.
  coord_cartesian(ylim = c(min(biomass$co2_kg_per_mwh) - 90,
                           max(biomass$co2_kg_per_mwh) * 1.12)) +
  labs(
    x = NULL,
    y = expression(paste("Net C", O[2], " / kg")),
    title = "Carbon cost of one biomass MWh under six accounting choices",
    subtitle = sprintf(
      "Different assumptions change the answer by %d kg",
      diff(range(biomass$co2_kg_per_mwh))
    ),
    caption = paste0(
      "Supply chain excluded unless stated.\nRegrowth: loblolly pine", 
      " plantation felled at ", felled_at,
      " (Smith et al. 2006). Source: emission_factors.csv,",
      " forest_growth.csv"
    )
  )
