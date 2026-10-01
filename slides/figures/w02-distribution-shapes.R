# Week 2 — three distribution shapes, with mean and median marked.
#
# The point of the slide is that mean and median separate as a distribution
# gets less symmetric. So both are computed from the sample, never placed by
# eye: where the lines fall is the evidence for the claim.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

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

ggplot(shapes, aes(x = value)) +
  geom_histogram(bins = 45, fill = course_fills[["sky"]],
                 colour = course_colours[["ink"]], linewidth = 0.2) +
  geom_vline(data = centres,
             aes(xintercept = value, colour = statistic, linetype = statistic),
             linewidth = 1) +
  facet_wrap(~shape, scales = "free", nrow = 1) +
  # Linetype repeats the colour distinction, so the two stay separable in
  # greyscale and for colour-vision deficiency.
  scale_colour_manual(values = c(mean = course_colours[["red"]],
                                 median = course_colours[["purple"]]),
                      name = NULL) +
  scale_linetype_manual(values = c(mean = "dashed", median = "solid"),
                        name = NULL) +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    strip.text = element_text(face = "bold", size = rel(1.05)),
    legend.position = "bottom"
  )
