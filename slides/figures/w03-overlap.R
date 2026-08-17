# Week 3 — what "overlap" looks like, at two levels of spread.
#
# The two centres are read from emission_factors.csv rather than typed, so
# the figure cannot drift away from the case study. Only the spread is
# invented: the slide's question is what happens to a fixed difference as
# variability changes.
#
# The shaded band is the pointwise minimum of the two curves, which is the
# region a measurement could have come from either population.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

factors <- read.csv("../data/emission_factors.csv")
factor_of <- function(fuel, scenario) {
  factors$co2_kg_per_mwh[factors$fuel == fuel & factors$scenario == scenario]
}

biomass_mean <- factor_of("biomass", "with_supply_chain")
coal_mean <- factor_of("coal", "official")

grid <- seq(0, 1400, length.out = 400)
spreads <- c("Narrow spread: easy to tell apart" = 80,
             "Wide spread: could be either" = 260)

densities <- lapply(spreads, function(sd_i) {
  list(biomass = dnorm(grid, biomass_mean, sd_i),
       coal = dnorm(grid, coal_mean, sd_i))
})

curves <- do.call(rbind, lapply(names(spreads), function(panel) {
  d <- densities[[panel]]
  data.frame(
    panel = panel,
    co2 = rep(grid, 2),
    density = c(d$biomass, d$coal),
    plant = rep(c("Biomass", "Coal"), each = length(grid))
  )
}))
curves$panel <- factor(curves$panel, levels = names(spreads))

# One row per grid point: geom_area over the doubled frame above would draw
# the band twice and comb it with vertical seams.
overlap <- do.call(rbind, lapply(names(spreads), function(panel) {
  d <- densities[[panel]]
  data.frame(panel = panel, co2 = grid, density = pmin(d$biomass, d$coal))
}))
overlap$panel <- factor(overlap$panel, levels = names(spreads))

ggplot(curves, aes(x = co2, y = density)) +
  geom_area(data = overlap, fill = course_fills[["stone"]]) +
  geom_line(aes(colour = plant), linewidth = 1.2) +
  facet_wrap(~panel) +
  scale_colour_manual(values = c(Biomass = course_colours[["cyan"]],
                                 Coal = course_colours[["ink"]]),
                      name = NULL) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  labs(
    x = expression(paste("C", O[2], " (kg per MWh)")),
    y = NULL,
    title = sprintf("The same %.0f kg gap, twice", coal_mean - biomass_mean),
    subtitle = "Shaded: values that could have come from either kind of plant",
    caption = "Centres from emission_factors.csv; spreads illustrative"
  ) +
  theme(legend.position = "bottom")
