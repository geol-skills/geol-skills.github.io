# Week 5 — do wind farms harm birds? Step 3 of 3: before the turbines.
#
# Only squares with no wind turbines at all during 2006–15, so turbines
# cannot have affected any of these bird records. Split them by whether a
# wind farm was later built there or applied for (whatever the planning
# outcome: a refused application still shows where developers chose to go).
#
# Measured as in w05-birds-effort.R — species relative to squares with
# similar recording effort — the squares developers later picked were
# already richer. The "benefit" in the previous figure predates the
# turbines, so something else is making both happen: the sites were chosen
# for reasons that also suit birds. Finding out what the turbines do needs
# a before-and-after comparison, which is the next slide.
#
# Same choices as w05-birds-raw.R: 8+ turbines, two-thirds land, mean and
# 95% CI. Effort bands are set across all squares, as in w05-birds-effort.R,
# so the measure is the same one.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

squares <- read.csv("../data/bird_squares.csv")
squares <- squares[squares$land_fraction >= 2 / 3, ]

effort_band <- cut(squares$records, quantile(squares$records, 0:10 / 10),
                   include.lowest = TRUE)
squares$vs_band <- squares$species - ave(squares$species, effort_band)

untouched <- squares[!squares$any_wind_by_2015, ]
untouched$group <- ifelse(untouched$largest_farm_later >= 8,
                          "Wind farm later", "No wind farm, ever")

mean_ci <- function(y) {
  half <- qt(0.975, length(y) - 1) * sd(y) / sqrt(length(y))
  c(mean = mean(y), lower = mean(y) - half, upper = mean(y) + half,
    n = length(y))
}
means <- as.data.frame(do.call(rbind, tapply(untouched$vs_band,
                                             untouched$group, mean_ci)))
means$group <- rownames(means)
means$label <- paste0(means$group, "\n(",
                      format(means$n, big.mark = ",", trim = TRUE),
                      " squares)")

gap <- means["Wind farm later", "mean"] - means["No wind farm, ever", "mean"]
gap_label <- paste(sub("-", "−", sprintf("%+.0f", gap)), "species")

ggplot(means, aes(x = label, y = mean, colour = group)) +
  geom_hline(yintercept = 0, colour = course_colours[["concrete"]]) +
  geom_pointrange(aes(ymin = lower, ymax = upper),
                  size = 1.2, linewidth = 1.4) +
  annotate("text", x = 1.5, y = mean(means$mean), label = gap_label,
           size = 7, colour = course_colours[["ink"]]) +
  scale_colour_manual(
    values = c("No wind farm, ever" = course_colours[["concrete"]],
               "Wind farm later" = course_colours[["purple"]]),
    guide = "none"
  ) +
  scale_y_continuous(labels = function(x) sub("-", "−", x)) +
  labs(x = "Squares with no turbines in 2006–15",
       y = "Species, relative to squares\nwith similar recording effort",
       caption = paste("Mean and 95% CI; 10-km squares, 2006–15.",
                       "Birds: BTO via NBN Atlas.",
                       "Wind farms (8+ turbines): REPD, DESNZ."))
