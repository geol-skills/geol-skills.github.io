# Week 1 — where the UK's imported wood pellets come from.
#
# Base graphics, because Week 1 students have not met ggplot2.
#
# Latest year only: data/README.md records that 2024's origin shares come
# from HMRC, while the earlier years are estimated from trade reports.
#
# Run with the working directory set to slides/ (see figures/README.md).

source("figures/theme.R")

pellets <- read.csv("../data/pellet_imports.csv")
latest_year <- max(pellets$year)
latest <- pellets[pellets$year == latest_year, ]
latest <- latest[order(latest$import_kt), ]

par_slide()
par(mar = c(5.4, 6.5, 2.6, 1))

barplot(
  latest$import_kt,
  names.arg = gsub("_", " ", latest$origin),
  horiz = TRUE,
  xlim = c(0, max(latest$import_kt) * 1.05),
  col = course_colours[["cyan"]],
  border = NA,
  xlab = "Imports (thousand tonnes)",
  main = sprintf("UK wood pellet imports by origin, %d (%.1f Mt in total)",
                 latest_year, sum(latest$import_kt) / 1000)
)
add_source("Source: Forest Research, Forestry Statistics 2025, Table 3.8")

invisible(NULL)
