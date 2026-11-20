# Weeks 1–10 — the PPDAC ring, with this week's stage(s) lit.
#
# PPDAC (Problem, Plan, Data, Analysis, Conclusion) is the module's spine
# (Decisions.md, issue #2; introduced on the Week 1 "The cycle" slide).
# Week 1 introduces it with nothing lit; every later recap shows it again
# so students see where the week sits in the cycle. It is a diagram, not
# data: nothing is measured, and students never reproduce it, so it uses
# ggplot2 even in the base-R week.
#
# Before draw_figure(), set options(ppdac_stage = ..., ppdac_phase = ...):
# one or more stage names to light, and the centre text. Options rather
# than variables, because draw_figure() sources into the global
# environment, which need not be where knitr evaluates the chunk.
# Without them it lights Problem; character(0) lights nothing.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

stages <- c("Problem", "Plan", "Data", "Analysis", "Conclusion")
lit <- getOption("ppdac_stage", "Problem")
centre_label <- getOption("ppdac_phase", "")
if (!all(lit %in% stages)) {
  stop("Unknown PPDAC stage: ", paste(setdiff(lit, stages), collapse = ", "))
}

# Stages clockwise from the top.
step <- 2 * pi / length(stages)
angle <- pi / 2 - (seq_along(stages) - 1) * step
nodes <- data.frame(
  stage = stages,
  x = cos(angle),
  y = sin(angle),
  lit = stages %in% lit
)

# Each arrow follows the circle from one stage towards the next, stopping
# short of both labels.
gap <- 0.42
arcs <- do.call(rbind, lapply(seq_along(stages), function(i) {
  t <- seq(angle[i] - gap, angle[i] - step + gap, length.out = 30)
  data.frame(arc = i, x = cos(t), y = sin(t))
}))

ggplot() +
  geom_path(
    data = arcs, aes(x, y, group = arc),
    colour = course_colours[["concrete"]], linewidth = 1,
    arrow = arrow(length = unit(0.12, "inches"), type = "closed")
  ) +
  geom_label(
    data = nodes, aes(x, y, label = stage, fill = lit, colour = lit),
    size = 5, fontface = "bold", label.padding = unit(0.35, "lines")
  ) +
  annotate(
    "text", x = 0, y = 0, label = centre_label, size = 5,
    colour = course_colours[["black"]]
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = course_colours[["cyan"]], "FALSE" = course_fills[["stone"]]
    ),
    guide = "none"
  ) +
  # White text on the lit stage: cyan passes 4.5:1 against white (theme.R).
  scale_colour_manual(
    values = c("TRUE" = "white", "FALSE" = course_colours[["ink"]]),
    guide = "none"
  ) +
  coord_equal(xlim = c(-1.5, 1.5), ylim = c(-1.3, 1.3), clip = "off") +
  theme_void()
