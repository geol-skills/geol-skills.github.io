# Week 9 — the gold assay: 50 positives is below the noise floor.
#
# The same HolmesCo scenario Week 6 works through by hand
# (week06-content.qmd, "HolmesCo strikes gold"). A 95%-specific assay
# calls 5% of barren samples positive, so 10,000 samples yield about 500
# positives with no gold anywhere. The press release reports 50.
#
# Keep the numbers in step with Week 6. They are the same press release,
# and the figure is only a callback if they match.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

n_samples <- 10000
sensitivity <- 0.95
specificity <- 0.95
reported <- 50

expected_positives <- function(prevalence) {
  n_samples * (sensitivity * prevalence +
                 (1 - specificity) * (1 - prevalence))
}

prevalence <- seq(0, 0.05, length.out = 400)
curve <- data.frame(prevalence = prevalence,
                    positives = expected_positives(prevalence))

noise_floor <- round(expected_positives(0))

ggplot(curve, aes(x = 100 * prevalence, y = positives)) +
  geom_ribbon(aes(ymin = 0, ymax = positives),
              fill = course_fills[["stone"]], alpha = 0.5) +
  geom_line(linewidth = 1.2, colour = course_colours[["cyan"]]) +
  geom_hline(yintercept = reported, linetype = "dashed",
             colour = course_colours[["red"]], linewidth = 0.9) +
  annotate("text", x = 0.1, y = reported, hjust = 0, vjust = -0.7,
           size = 4.6, colour = course_colours[["red"]],
           label = sprintf("HolmesCo reports %d", reported)) +
  annotate("text", x = 0.1, y = noise_floor, hjust = 0, vjust = 1.9,
           size = 4.6, colour = course_colours[["ink"]],
           label = sprintf("Expected from noise alone: %d", noise_floor)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x = "Samples that really are over gold (%)",
    y = sprintf("Positive tests (of %s)", format(n_samples, big.mark = ",")),
    title = "The discovery is smaller than the error rate",
    subtitle = sprintf(
      paste("With no gold anywhere, a %.0f%%-accurate assay",
            "still calls %d of %s positive"),
      100 * specificity, noise_floor, format(n_samples, big.mark = ",")
    ),
    caption = paste("Sensitivity and specificity both 95%;",
                    "shaded = expected positives")
  )
