# Week 4 — "Biomass plantations lock up carbon": what's missing?
#
# A pine plantation is established a slice a year, and each stand is felled
# for pellets at 25 and replanted. Once every age from 0 to 24 is present,
# the landscape holds a steady average stock: the carbon the plantation
# "locks up".
#
# Stage 1 starts on abandoned farmland and measures the plantation against
# bare ground, which is the omission.
#
# Stage 2 adds the counterfactual: what the field would have held without
# the plantation, as in Stephenson & MacKay (2014), DECC, Scenarios 28-29
# (Table 45). Left alone, it grows back to forest or stays scrub. The
# plantation beats scrub and loses to regrowth.
#
# Stage 3 asks what was there before. The plantation replaces a mature
# forest, at the carbon stock the regrowth curve ends on, felled a slice a
# year. The carbon "locked up" is the same; the forest's carbon above it
# has gone up the chimney. (The leader then asks: what if it was cotton?
# Where will the cotton be grown now?)
#
# Plantation: loblolly pine, high productivity, live tree carbon
# (data/forest_growth.csv; Smith et al. 2006, Table A40). Forest: DECC's
# IPCC (2006) parameters, with roots, without litter, to match live tree
# carbon. Soil carbon is left out, as in DECC's figure.
#
# Run with the working directory set to slides/ (see figures/README.md).
# The stage is read from options(w04_plantation_stage = 1, 2 or 3).

library("ggplot2")
source("figures/theme.R")

stage <- getOption("w04_plantation_stage", 3)
years <- if (stage == 3) -10:100 else 0:100
rotation <- 25

growth <- read.csv("../data/forest_growth.csv")
pine <- growth[growth$forest == "loblolly_plantation", ]
stand_carbon <- approx(pine$age, pine$carbon_t_per_ha, xout = 0:100)$y

# One slice of 1/rotation of the land is planted each year; a stand is
# felled and replanted at the rotation age.
planted <- pmax(pmin(years + 1, rotation), 0)
plantation <- vapply(planted, function(n) {
  sum(stand_carbon[seq_len(n)]) / rotation
}, numeric(1))
locked_up <- plantation[length(plantation)]

# DECC Table 45: ten years of scrub; then forest above-ground dry matter
# grows 7 t/ha/yr for 20 years, then 2 t/ha/yr, up to 220 t/ha
# (103.4 t C/ha); roots add 24.8 t C/ha at maturity.
carbon_per_dm <- 103.4 / 220
root_share <- 24.8 / 103.4
scrub_years <- 10
scrub_ramp <- function(top) pmin(years / scrub_years, 1) * top
forest_dm <- pmin(
  220,
  7 * pmin(pmax(years - scrub_years, 0), 20) +
    2 * pmax(years - scrub_years - 20, 0)
)
regrowth <- pmax(scrub_ramp(7.18),
                 forest_dm * carbon_per_dm * (1 + root_share))
scrub <- scrub_ramp(7.4)
mature_forest <- 220 * carbon_per_dm * (1 + root_share)

nbsp <- "\u00a0"
unit <- paste0(nbsp, "t", nbsp, "C/ha")
label_size <- 4.6

if (stage == 3) {
  standing <- plantation + (1 - planted / rotation) * mature_forest
  released <- mature_forest - locked_up
  p <- ggplot(data.frame(year = years, carbon = standing)) +
    geom_ribbon(aes(x = year, ymin = carbon, ymax = mature_forest),
                fill = course_fills[["heather"]]) +
    geom_area(aes(x = year, y = carbon), fill = course_fills[["sky"]],
              colour = course_colours[["cyan"]], linewidth = 1.3) +
    geom_hline(yintercept = mature_forest, colour = course_colours[["gold"]],
               linewidth = 1.3) +
    annotate("text", x = max(years), y = mature_forest, hjust = 1,
             vjust = -0.6, size = label_size,
             colour = course_colours[["gold"]],
             label = "Left alone: stays forest") +
    annotate("text", x = 62, y = (mature_forest + locked_up) / 2,
             size = 5.5, colour = course_colours[["ink"]],
             label = sprintf("%d%s released", round(released), unit))
} else {
  curves <- data.frame(
    year = rep(years, 2),
    carbon = c(regrowth, scrub),
    land = rep(c("Left alone: grows back to forest",
                 "Left alone: stays scrub"),
               each = length(years))
  )
  end_labels <- curves[curves$year == max(years), ]
  p <- ggplot() +
    geom_area(aes(x = years, y = plantation), fill = course_fills[["sky"]],
              colour = course_colours[["cyan"]], linewidth = 1.3)
  if (stage == 2) {
    p <- p +
      geom_line(data = curves, aes(x = year, y = carbon, colour = land),
                linewidth = 1.3) +
      geom_text(data = end_labels,
                aes(x = year, y = carbon, label = land, colour = land),
                hjust = 1, vjust = -0.6, size = label_size) +
      scale_colour_manual(
        values = c(
          "Left alone: grows back to forest" = course_colours[["gold"]],
          "Left alone: stays scrub" = course_colours[["purple"]]
        ),
        guide = "none"
      )
  }
}

p +
  geom_hline(yintercept = 0, colour = course_colours[["concrete"]]) +
  annotate("text", x = 45, y = locked_up * 0.6, size = 5.5,
           colour = course_colours[["ink"]],
           label = sprintf("%s:\n%d%s locked up",
                           if (stage == 3) "Pine" else "Pine plantation",
                           round(locked_up), unit)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  coord_cartesian(ylim = c(0, 140)) +
  labs(
    x = "Years since planting began",
    y = paste0("Carbon in vegetation (t", nbsp, "C/ha)"),
    title = c(
      "Biomass plantations lock up carbon",
      "Biomass plantations lock up carbon… compared to what?",
      "Biomass plantations lock up carbon… and what was there before?"
    )[stage],
    caption = paste(
      "Plantation: Smith et al. (2006), loblolly pine, 25-year rotation.",
      c("", "Forest and scrub:", "Forest:")[stage],
      if (stage >= 2) "Stephenson & MacKay (2014), DECC, Table 45."
    )
  )
