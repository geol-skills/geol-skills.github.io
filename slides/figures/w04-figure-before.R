# Week 4 — the "before" half of the before/after pair.
#
# Deliberately bad, in exactly the four ways the slide lists: every fuel
# plotted, legend in whatever order the columns happened to be in, an axis
# labelled with a raw column name, no caption, default grey panel.
#
# Do not tidy it. w04-figure-after.R is the same data done properly, and
# the slide is the contrast between them.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")

elec <- read.csv("../data/uk_electricity.csv")
fuels <- grep("_twh$", names(elec), value = TRUE)
fuels <- setdiff(fuels, "total_twh")

long <- do.call(rbind, lapply(fuels, function(fuel) {
  data.frame(year = elec$year, fuel = fuel, twh = elec[[fuel]])
}))

ggplot(long, aes(x = year, y = twh, colour = fuel)) +
  geom_line() +
  labs(x = "year", y = "twh") +
  theme_grey(base_size = 14)
