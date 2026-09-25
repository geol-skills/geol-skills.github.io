# Week 5 — do wind farms harm birds? Step 1 of 3: the raw comparison.
#
# Real data: every mostly-land 10-km square in Great Britain, the bird
# species recorded there in 2006–15, and whether a wind farm was operating
# by 2015. Squares with a wind farm have fewer species recorded. The next
# two figures (w05-birds-effort.R, w05-birds-before.R) take this apart.
#
# Choices, identical in all three figures:
# - "Wind farm" means a project of 8 or more turbines.
# - Squares under two-thirds land are dropped: coastal squares mix seabirds
#   with sparse records, and are not what the question is about.
# - Means with 95% confidence intervals, not the full distribution: the
#   slide is about the direction of the difference. How big it is against
#   the spread is for the students to ask ("Is that a big number?"), not
#   for the figure to answer for them.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

squares <- read.csv("../data/bird_squares.csv")
squares <- squares[squares$land_fraction >= 2 / 3, ]
squares$group <- ifelse(squares$largest_farm_by_2015 >= 8,
                        "Wind farm", "No wind farm")

mean_ci <- function(y) {
  half <- qt(0.975, length(y) - 1) * sd(y) / sqrt(length(y))
  c(mean = mean(y), lower = mean(y) - half, upper = mean(y) + half,
    n = length(y))
}
means <- as.data.frame(do.call(rbind, tapply(squares$species, squares$group,
                                             mean_ci)))
means$group <- rownames(means)
means$label <- paste0(means$group, "\n(",
                      format(means$n, big.mark = ",", trim = TRUE),
                      " squares)")

gap <- means["Wind farm", "mean"] - means["No wind farm", "mean"]
gap_label <- paste(sub("-", "−", sprintf("%+.0f", gap)), "species")

ggplot(means, aes(x = label, y = mean, colour = group)) +
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
  labs(x = NULL, y = "Bird species recorded\n(mean per square)",
       caption = paste("Mean and 95% CI; 10-km squares, 2006–15.",
                       "Birds: BTO via NBN Atlas.",
                       "Wind farms (8+ turbines): REPD, DESNZ."))
