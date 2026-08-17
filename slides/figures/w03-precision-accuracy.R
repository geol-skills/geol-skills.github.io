# Week 3 — precision and accuracy, all four combinations.
#
# The slide names only two of the four quadrants; the distinction only
# clicks once you can see the pair it leaves out.
#
# Repeated measurements against a known truth, rather than the usual
# dartboard: this is the shape a student's own replicate data takes, and
# they can draw it with what they were taught in Week 2.
#
# The true value is the mean of one real formation, so the reference line
# is a number from the data rather than a round one chosen to look tidy.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

boreholes <- read.csv("../data/borehole_temp.csv")
truth <- mean(boreholes$temperature_c[boreholes$formation == "Whin Sill"])

set.seed(2301)
n <- 25
bias <- 6
cases <- expand.grid(
  precise = c(TRUE, FALSE),
  accurate = c(TRUE, FALSE)
)

readings <- do.call(rbind, lapply(seq_len(nrow(cases)), function(i) {
  precise <- cases$precise[i]
  accurate <- cases$accurate[i]
  data.frame(
    panel = sprintf("%s, %s",
                    if (precise) "Precise" else "Imprecise",
                    if (accurate) "accurate" else "inaccurate"),
    replicate = seq_len(n),
    temperature = rnorm(n,
                        mean = truth + if (accurate) 0 else bias,
                        sd = if (precise) 0.4 else 3)
  )
}))

ggplot(readings, aes(x = replicate, y = temperature)) +
  geom_hline(yintercept = truth, linetype = "dashed",
             colour = course_colours[["red"]], linewidth = 0.8) +
  geom_point(colour = course_colours[["cyan"]], size = 2) +
  facet_wrap(~panel) +
  labs(
    x = "Replicate measurement",
    y = "Temperature (°C)",
    title = "Precision is not accuracy",
    subtitle = sprintf(
      paste("Dashed line: the true value, %.1f °C.",
            "Precision is scatter; accuracy is where the scatter sits."),
      truth
    ),
    caption = "Simulated replicates around a real formation mean"
  )
