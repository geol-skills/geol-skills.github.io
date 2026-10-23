# Week 2 — the sample behind the distribution-shape figures.
#
# Sourced by w02-distribution-shapes.R and w02-bimodal.R, so the Shape and
# two-humped slides show the same data. Defines `shapes` and `centres`.
#
# Run with the working directory set to slides/ (see figures/README.md).

set.seed(2201)
n <- 4000

shapes <- rbind(
  data.frame(shape = "Symmetric", value = rnorm(n, mean = 50, sd = 10)),
  data.frame(shape = "Skewed", value = rlnorm(n, meanlog = 3.4, sdlog = 0.55)),
  data.frame(shape = "Bimodal", value = c(rnorm(n / 2, mean = 32, sd = 6),
                                          rnorm(n / 2, mean = 68, sd = 6)))
)
shapes$shape <- factor(shapes$shape,
                       levels = c("Symmetric", "Skewed", "Bimodal"))

# One mean and one median per panel. Median first so the dashed mean draws
# over it: on the symmetric panel the two coincide, and whichever is drawn
# second is the only one visible.
centres <- do.call(rbind, lapply(split(shapes, shapes$shape), function(d) {
  data.frame(
    shape = d$shape[1],
    statistic = c("median", "mean"),
    value = c(median(d$value), mean(d$value))
  )
}))
