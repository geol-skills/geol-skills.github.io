# Week 8 — the w08-groups-raincloud.R figure with Scotland and Northern
# England in purple. Sets up the question for "Is that a big
# difference?": the two pairs look different, but are they?
#
# Draws w08-groups-raincloud.R with `highlight` set. local() keeps
# `highlight` out of the deck's environment, so later figures that source
# the raincloud are unaffected.
#
# Run with the working directory set to slides/ (see figures/README.md).

local({
  highlight <- c("Scotland", "Northern England")
  source("figures/w08-groups-raincloud.R", local = TRUE)$value +
    labs(subtitle = paste("Cloud: distribution. Rain: sites.",
                          "Bar: mean +/- 95\u00a0% CI.",
                          "Colour: Windiness."))
})
