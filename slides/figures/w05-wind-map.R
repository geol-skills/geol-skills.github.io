# Week 5 — the squares behind the wind farm and bird figures.
#
# Shown before w05-birds-raw.R, so students can see these are real places.
# Every mostly-land 10-km square in Great Britain, coloured by wind farm
# history. The map hints at the confounder: wind farms are not scattered at
# random, but gather in the Scottish uplands, Wales, the Pennines and the
# flat, exposed east of England, and the later ones follow the earlier.
#
# Groups match the bar figures: a wind farm is a project of 8+ turbines;
# "later" means built after 2015 or applied for, on a square that had no
# turbines at all in 2006–15.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

squares <- read.csv("../data/bird_squares.csv")
squares <- squares[squares$land_fraction >= 2 / 3, ]
squares$group <- "Neither"
squares$group[!squares$any_wind_by_2015 &
                squares$largest_farm_later >= 8] <- "Wind farm later"
squares$group[squares$largest_farm_by_2015 >= 8] <- "Wind farm by 2015"
squares$group <- factor(squares$group, levels = c("Wind farm by 2015",
                                                  "Wind farm later",
                                                  "Neither"))

ggplot(squares, aes(x = easting + 5000, y = northing + 5000, fill = group)) +
  geom_tile(width = 10000, height = 10000) +
  scale_fill_manual(values = c("Wind farm by 2015" = course_colours[["cyan"]],
                               "Wind farm later" = course_colours[["purple"]],
                               "Neither" = course_fills[["stone"]]),
                    name = NULL) +
  coord_equal() +
  theme_void(base_size = 16) +
  theme(legend.position = "right",
        text = element_text(colour = course_colours[["ink"]]),
        plot.caption = element_text(size = rel(0.7))) +
  labs(caption = "10-km squares, at least two-thirds land. REPD, DESNZ.")
