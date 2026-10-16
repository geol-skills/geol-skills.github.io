# Week 3 — what "overlap" looks like, at two levels of spread.
#
# Both centres are read from emission_factors.csv rather than typed, so the
# figure cannot drift away from the case study, and both are at-chimney
# values: biomass "combustion_only" against coal "official", which the CSV
# notes is combustion only. Pairing biomass's supply-chain figure against
# coal's chimney figure — as this script once did — is the asymmetric system
# boundary the module spends Week 3 teaching students to catch.
#
# Only the spread is invented: the slide's question is what happens to a
# fixed difference as variability changes.
#
# The shaded band is the pointwise minimum of the two curves, which is the
# region a measurement could have come from either population.
#
# Title, gloss and caveat live on the slide rather than in the raster, so
# they reflow, carry a real minus sign, and reach a screen reader.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

factors <- read.csv("../data/emission_factors.csv")
factor_of <- function(fuel, scenario) {
  factors$co2_kg_per_mwh[factors$fuel == fuel & factors$scenario == scenario]
}

biomass_mean <- factor_of("biomass", "combustion_only")
coal_mean <- factor_of("coal", "official")

# Spreads are set as multiples of the gap rather than as absolute kilograms,
# so the contrast the slide is drawing survives a revision to the factors:
# a third of the gap separates the curves cleanly, half again buries them.
gap <- abs(coal_mean - biomass_mean)
spreads <- c("Narrow spread" = gap / 3, "Wide spread" = gap * 1.4)

# Three and a half standard deviations past the outer centre at the larger
# spread; a tighter range clips the wide panel's tails and makes the overlap
# look smaller than it is.
pad <- 3.5 * max(spreads)
grid <- seq(min(biomass_mean, coal_mean) - pad,
            max(biomass_mean, coal_mean) + pad,
            length.out = 500)

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
  labs(x = expression(paste("C", O[2], " (kg per MWh)")), y = NULL) +
  theme(legend.position = "bottom")
