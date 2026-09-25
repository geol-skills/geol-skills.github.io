# Week 5 — four sampling strategies over the same site.
#
# The table on the slide names these four; what it cannot show is that
# they are spatial patterns. Random and systematic are indistinguishable
# in words and obvious side by side.
#
# The convenience panel is HolmesCo's three boreholes generalised: every
# sample within reach of one valley road.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

set.seed(2501)
n <- 24
site <- 100

random <- data.frame(x = runif(n, 0, site), y = runif(n, 0, site))

# Six per quadrant, so every part of the site is represented whatever the
# sample happens to do.
corners <- expand.grid(x0 = c(0, site / 2), y0 = c(0, site / 2))
stratified <- do.call(rbind, lapply(seq_len(nrow(corners)), function(i) {
  data.frame(
    x = runif(n / 4, corners$x0[i], corners$x0[i] + site / 2),
    y = runif(n / 4, corners$y0[i], corners$y0[i] + site / 2)
  )
}))

# 6 by 4 rather than a square grid, so the count comes out at exactly n
# and the panel is not one hole short of a lattice.
systematic <- expand.grid(
  x = seq(site / 12, site, length.out = 6),
  y = seq(site / 8, site, length.out = 4)
)

# A road running up the valley, with samples scattered a short walk from it.
road <- runif(n, 10, 90)
convenience <- data.frame(x = road + rnorm(n, 0, 3),
                          y = road * 0.8 + 10 + rnorm(n, 0, 3))

samples <- rbind(
  cbind(strategy = "Random", random),
  cbind(strategy = "Stratified", stratified),
  cbind(strategy = "Systematic", systematic),
  cbind(strategy = "Convenience", convenience)
)
samples$strategy <- factor(
  samples$strategy,
  levels = c("Random", "Stratified", "Systematic", "Convenience")
)

strata <- data.frame(strategy = factor("Stratified", levels(samples$strategy)),
                     mid = site / 2)

ggplot(samples, aes(x = x, y = y)) +
  geom_vline(data = strata, aes(xintercept = mid),
             colour = course_colours[["concrete"]], linewidth = 0.4) +
  geom_hline(data = strata, aes(yintercept = mid),
             colour = course_colours[["concrete"]], linewidth = 0.4) +
  geom_point(colour = course_colours[["cyan"]], size = 2.4) +
  facet_wrap(~strategy, nrow = 1) +
  coord_equal(xlim = c(-5, site + 5), ylim = c(-5, site + 5)) +
  labs(
    x = NULL,
    y = NULL,
    title = "Twenty-four samples, four ways of choosing where",
    subtitle = paste("The first three are about the site.",
                     "The last is about the road."),
    caption = "Simulated site, 24 samples per strategy"
  ) +
  # panel.grid.major, not panel.grid: theme_slide() sets the more specific
  # element and would win.
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid.major = element_blank())
