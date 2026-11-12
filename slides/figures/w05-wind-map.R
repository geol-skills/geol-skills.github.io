# Week 5 — the squares behind the wind farm and bird figures.
#
# Shown before w05-birds-raw.R, so students can see these are real places.
# Every mostly-land 10-km square in Great Britain, twice: coloured by wind
# farm history on the left, by bird species recorded on the right. The pair
# hints at the confounder: wind farms are not scattered at random, but
# gather in the Scottish uplands, Wales, the Pennines and the flat, exposed
# east of England, and the later ones follow the earlier. Species counts are
# highest where people live and visit: the south, the coasts, round cities.
#
# The species scale is clipped at 75 and 225 (4 % and 2 % of squares lie
# beyond), and the legend says so: on the full 24–309 range the middle
# half of squares share one shade and the pattern disappears.
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

map_theme <- theme_void(base_size = 16) +
  theme(legend.position = "right",
        text = element_text(colour = course_colours[["ink"]]),
        plot.caption = element_text(size = rel(0.7)),
        plot.caption.position = "plot")

base <- ggplot(squares, aes(x = easting + 5000, y = northing + 5000)) +
  coord_equal() +
  map_theme

farms <- base +
  geom_tile(aes(fill = group), width = 10000, height = 10000) +
  scale_fill_manual(values = c("Wind farm by 2015" = course_colours[["purple"]],
                               "Wind farm later" = course_colours[["cyan"]],
                               "Neither" = course_fills[["stone"]]),
                    name = NULL) +
  labs(caption = "10-km squares, at least two-thirds land. REPD, DESNZ.")

birds <- base +
  geom_tile(aes(fill = species), width = 10000, height = 10000) +
  scale_fill_gradientn(colours = c(course_fills[["sky"]],
                                   course_colours[["cyan"]],
                                   course_colours[["ink"]]),
                       limits = c(75, 225), oob = scales::squish,
                       breaks = c(75, 150, 225),
                       labels = c("75 or fewer", "150", "225 or more"),
                       name = "Bird species\nrecorded,\n2006–15") +
  labs(caption = "BTO volunteer records, via NBN Atlas.")

side_by_side(farms, birds)
