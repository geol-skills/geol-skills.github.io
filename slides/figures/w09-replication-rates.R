# Week 9 — putting a number on "many published findings don't hold up".
#
# Published summary statistics, hard-coded with their sources, because
# there is no honest way to simulate a replication crisis. Each was read
# from the paper's own abstract:
#
#  Open Science Collaboration (2015) Science 349:aac4716 — "Ninety-seven
#    percent of original studies had significant results. Thirty-six
#    percent of replications had significant results." No count given for
#    the 36%, so none is shown.
#  Camerer et al. (2016) Science 351:1433 — "a significant effect in the
#    same direction as the original study for 11 replications (61%)".
#  Errington et al. (2021) eLife 10:e71601 — "combining positive and null
#    effects, the success rate was 46% (51/112)".
#
# The three use different success criteria, which is why the criterion is
# on the axis label rather than hidden in a footnote: "replicated" is not
# one thing, and this deck is about being precise on exactly that point.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

source("figures/theme.R")

replications <- data.frame(
  field = c("Psychology\n(100 studies, 2015)",
            "Experimental economics\n(18 studies, 2016)",
            "Cancer biology\n(112 effects, 2021)"),
  criterion = c("significant replication",
                "significant, same direction",
                "3 of 5 success criteria"),
  percent = c(36, 61, 46),
  counted = c(NA, "11/18", "51/112")
)
replications$field <- factor(replications$field,
                             levels = rev(replications$field))
replications$label <- ifelse(
  is.na(replications$counted),
  sprintf("%d%%", replications$percent),
  sprintf("%d%% (%s)", replications$percent, replications$counted)
)

ggplot(replications, aes(x = field, y = percent)) +
  geom_col(fill = course_colours[["cyan"]], width = 0.6) +
  geom_text(aes(label = label), hjust = -0.15, size = 5,
            colour = course_colours[["ink"]]) +
  # Nudged in category space, not by vjust: the criterion has to clear the
  # bar edge, and vjust is measured in font sizes.
  geom_text(aes(y = 2, label = criterion), hjust = 0, size = 3.8,
            colour = course_colours[["black"]],
            position = position_nudge(x = -0.42)) +
  coord_flip(clip = "off") +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(v) paste0(v, "%")) +
  labs(
    x = NULL,
    y = "Replications that succeeded",
    title = "\"Many published findings don't hold up\"",
    subtitle = paste("Three replication projects, three fields,",
                     "three different definitions of success"),
    caption = paste("Open Science Collaboration 2015 Science 349:aac4716;",
                    "Camerer et al. 2016 Science 351:1433;",
                    "\nErrington et al. 2021 eLife 10:e71601")
  )
