# Week 8 — the class coin toss: what a fair coin does in 6 tosses.
#
# The null world for one student's test, computed exactly with dbinom().
# All heads or all tails has probability 2 / 64 = 0.031, so the two-tailed
# test rejects "my coin is fair" at p < 0.05. Five of one side is not
# enough: with it, the tails hold 0.22. The coins are fair, so every
# rejection in the room is a false positive.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

n_tosses <- 6
heads <- 0:n_tosses
coin <- data.frame(
  heads = heads,
  prob = dbinom(heads, n_tosses, 0.5),
  reject = heads %in% c(0, n_tosses)
)
alpha <- sum(coin$prob[coin$reject])

ggplot(coin, aes(x = heads, y = prob, fill = reject)) +
  geom_col(width = 0.85, colour = course_colours[["ink"]], linewidth = 0.2,
           show.legend = FALSE) +
  annotate("text", x = c(0, n_tosses), y = 0.07, size = 6,
           colour = course_colours[["red"]],
           label = c("All tails", "All heads")) +
  scale_fill_manual(values = c(`FALSE` = course_fills[["sky"]],
                               `TRUE` = course_colours[["red"]])) +
  scale_x_continuous(breaks = heads) +
  labs(x = sprintf("Heads in %d tosses of a fair coin", n_tosses),
       y = "Probability",
       title = sprintf("Red: \"not a fair coin\". Chance if fair: %.3f",
                       alpha),
       caption = sprintf("Exact binomial probabilities: dbinom(0:%d, %d, 0.5)",
                         n_tosses, n_tosses))
