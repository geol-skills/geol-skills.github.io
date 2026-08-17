# Week 7 — between-group variation, relative to within.
#
# The group means are identical in both panels. Only the scatter around
# them changes, and with it F. That is the whole content of "between
# relative to within", and it is not sayable.
#
# Points rather than boxplots: the F-ratio is about how far points sit
# from their group mean, and a boxplot hides exactly that.
#
# Each F comes from aov() on the panel's own data.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

set.seed(2701)
per_group <- 20
group_means <- c(Scotland = 34, Wales = 29, England = 25)

# One set of residuals, centred exactly, reused at two scales. Drawing
# each panel independently would leave the sample means slightly
# different, and the subtitle's claim that only the spread changes would
# be false.
residuals <- lapply(names(group_means), function(region) {
  r <- rnorm(per_group)
  r - mean(r)
})
names(residuals) <- names(group_means)

sample_panel <- function(within_sd) {
  do.call(rbind, lapply(names(group_means), function(region) {
    data.frame(region = region,
               capacity = group_means[[region]] +
                 within_sd * residuals[[region]])
  }))
}

f_of <- function(d) {
  summary(aov(capacity ~ region, data = d))[[1]][["F value"]][1]
}

spreads <- c(Tight = 2.5, Loose = 9)
panels <- do.call(rbind, lapply(names(spreads), function(name) {
  d <- sample_panel(spreads[[name]])
  d$panel <- sprintf("%s spread: F = %.1f", name, f_of(d))
  d
}))
panels$region <- factor(panels$region, levels = names(group_means))
panels$panel <- factor(panels$panel, levels = unique(panels$panel))

centres <- do.call(rbind, lapply(split(panels, ~ panel + region), function(d) {
  data.frame(panel = d$panel[1], region = d$region[1],
             capacity = mean(d$capacity))
}))

ggplot(panels, aes(x = region, y = capacity)) +
  geom_point(position = position_jitter(width = 0.18, height = 0),
             colour = course_colours[["cyan"]], size = 2, alpha = 0.8) +
  geom_segment(data = centres,
               aes(x = as.integer(region) - 0.3,
                   xend = as.integer(region) + 0.3,
                   y = capacity, yend = capacity),
               colour = course_colours[["ink"]], linewidth = 1.1) +
  facet_wrap(~panel) +
  labs(
    x = NULL,
    y = "Capacity factor (%)",
    title = "The same three group means, twice",
    subtitle = paste("Between-group variation is identical.",
                     "Only the scatter within each group changes."),
    caption = "Simulated, 20 sites per region"
  )
