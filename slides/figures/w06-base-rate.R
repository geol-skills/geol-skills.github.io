# Week 6 — what the 2x2 table cannot show: the answer moves with the base
# rate.
#
# The table works one case exactly. This is the same arithmetic swept
# across every prevalence, so the 9% on the previous slide is a point on a
# curve rather than a number to take on trust.
#
# Test accuracy is held at the slide's 99% for both sensitivity and
# specificity, so nothing here is a different test — only a different
# population.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

sensitivity <- 0.99
specificity <- 0.99

positive_predictive_value <- function(prevalence) {
  true_positives <- sensitivity * prevalence
  false_positives <- (1 - specificity) * (1 - prevalence)
  true_positives / (true_positives + false_positives)
}

prevalence <- 10^seq(-4, log10(0.5), length.out = 400)
curve <- data.frame(prevalence = prevalence,
                    ppv = positive_predictive_value(prevalence))

slide_case <- data.frame(prevalence = 1 / 1000)
slide_case$ppv <- positive_predictive_value(slide_case$prevalence)

ggplot(curve, aes(x = prevalence, y = ppv)) +
  geom_line(linewidth = 1.2, colour = course_colours[["cyan"]]) +
  geom_point(data = slide_case, size = 3.5,
             colour = course_colours[["red"]]) +
  annotate("text", x = slide_case$prevalence * 1.4, y = slide_case$ppv,
           hjust = 0, vjust = -0.6, size = 4.6,
           colour = course_colours[["red"]],
           label = sprintf("1 in 1,000: %.0f%%", 100 * slide_case$ppv)) +
  scale_x_log10(
    breaks = 10^(-4:-1),
    labels = function(p) paste0("1 in ", format(round(1 / p), big.mark = ","))
  ) +
  scale_y_continuous(labels = function(v) paste0(round(100 * v), "%"),
                     limits = c(0, 1)) +
  labs(
    x = "How common the condition is",
    y = "Chance you have it, given a positive test",
    title = "The same test, on different populations",
    subtitle = sprintf(
      "Sensitivity and specificity fixed at %.0f%%. Only the base rate moves.",
      100 * sensitivity
    ),
    caption = "Computed from Bayes' rule"
  )
