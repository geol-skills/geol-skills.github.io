# Week 5 — do wind farms harm birds? Step 2 of 3: allow for recording effort.
#
# The same squares as w05-birds-raw.R. Bird records are volunteer sightings,
# and far more come from towns and gardens than from windy hills, so a
# square's species count partly measures how hard people looked. Number of
# records stands in for number of birdwatchers.
#
# The adjustment is stratification, the method on the following slide: sort
# squares into ten equal-sized bands by number of records, then compare
# each square with the average of its own band. Wind farm squares now come
# out *ahead*. Controlling for one confounder has reversed the answer — and
# the next figure shows that this is not the end of it either.
#
# Same choices as w05-birds-raw.R: 8+ turbines, two-thirds land, mean and
# 95% CI.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

squares <- read.csv("../data/bird_squares.csv")
squares <- squares[squares$land_fraction >= 2 / 3, ]
squares$group <- ifelse(squares$largest_farm_by_2015 >= 8,
                        "Wind farm", "No wind farm")

effort_band <- cut(squares$records, quantile(squares$records, 0:10 / 10),
                   include.lowest = TRUE)
squares$vs_band <- squares$species - ave(squares$species, effort_band)

mean_ci <- function(y) {
  half <- qt(0.975, length(y) - 1) * sd(y) / sqrt(length(y))
  c(mean = mean(y), lower = mean(y) - half, upper = mean(y) + half,
    n = length(y))
}
means <- as.data.frame(do.call(rbind, tapply(squares$vs_band, squares$group,
                                             mean_ci)))
means$group <- rownames(means)
means$label <- paste0(means$group, "\n(",
                      format(means$n, big.mark = ",", trim = TRUE),
                      " squares)")

gap <- means["Wind farm", "mean"] - means["No wind farm", "mean"]
gap_label <- paste(sub("-", "−", sprintf("%+.0f", gap)), "species")

ggplot(means, aes(x = label, y = mean, colour = group)) +
  geom_hline(yintercept = 0, colour = course_colours[["concrete"]]) +
  geom_pointrange(aes(ymin = lower, ymax = upper),
                  size = 1.2, linewidth = 1.4) +
  annotate("text", x = 1.5, y = mean(means$mean), label = gap_label,
           size = 7, colour = course_colours[["ink"]]) +
  scale_colour_manual(
    values = c("No wind farm" = course_colours[["concrete"]],
               "Wind farm" = course_colours[["cyan"]]),
    guide = "none"
  ) +
  scale_y_continuous(labels = function(x) sub("-", "−", x)) +
  labs(x = NULL,
       y = "Species, relative to squares\nwith similar recording effort",
       caption = paste("Mean and 95% CI; 10-km squares, 2006–15.",
                       "Birds: BTO via NBN Atlas.",
                       "Wind farms (8+ turbines): REPD, DESNZ."))
