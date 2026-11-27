# Week 8 — "Compared to what?": Wales vs Southern England.
#
# Two columns of the "Plotting a comparison" points figure, same
# encoding (cyan sites, ink mean +/- 95 % CI), so students can see the
# difference δ that the slide divides by SD and by SE. δ is marked as a
# bar between the two means.
#
# set.seed() fixes the jitter. Run with the working directory set to
# slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

wind <- read.csv("../data/wind_output.csv")
two <- wind[wind$region %in% c("Southern England", "Wales"), ]
two$region <- factor(two$region, levels = c("Southern England", "Wales"))

ci_of <- function(x) {
  m <- mean(x)
  half <- qt(0.975, length(x) - 1) * sd(x) / sqrt(length(x))
  data.frame(capacity_pct = m, lower = m - half, upper = m + half)
}
summaries <- do.call(rbind, lapply(split(two, two$region), function(d) {
  cbind(region = d$region[1], ci_of(d$capacity_pct))
}))

means <- summaries$capacity_pct
delta <- diff(means)
mid_x <- 1.5

set.seed(2707)

ggplot(two, aes(x = region, y = capacity_pct)) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.5,
              colour = course_colours[["cyan"]]) +
  geom_errorbar(data = summaries, aes(ymin = lower, ymax = upper),
                width = 0.2, colour = course_colours[["ink"]],
                linewidth = 1) +
  geom_point(data = summaries, size = 2.6, colour = course_colours[["ink"]]) +
  annotate("segment", x = c(1.12, 1.88), xend = mid_x,
           y = means, yend = means,
           linetype = "dashed", colour = course_colours[["ink"]]) +
  annotate("segment", x = mid_x, xend = mid_x, y = means[1], yend = means[2],
           colour = course_colours[["red"]], linewidth = 1.2) +
  annotate("text", x = mid_x + 0.04, y = mean(means), hjust = 0,
           label = sprintf('italic("δ") == "%.1f"', delta), parse = TRUE,
           colour = course_colours[["red"]], size = 5.5) +
  labs(
    x = NULL,
    y = "Capacity factor (%)",
    caption = "wind_output.csv, 25 sites per region"
  )
