# Render one figure script to a PNG, without rendering a whole deck.
#
#   Rscript slides/figures/preview.R w02-distribution-shapes.R [out.png]
#
# Sets the working directory the figure scripts expect, so this works from
# the repo root.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript slides/figures/preview.R <script.R> [output.png]")
}

script <- args[1]
out <- if (length(args) >= 2) args[2] else sub("\\.R$", ".png", script)
out <- normalizePath(out, mustWork = FALSE)

# Locate slides/ whether we were called from the repo root or from slides/
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
here <- dirname(sub("^--file=", "", file_arg))
setwd(normalizePath(file.path(here, "..")))

# 1280x720 slide, figure occupying most of the width
png(out, width = 1100, height = 480, res = 110)
on.exit(dev.off())

# Draw by the same route a deck does, so a figure cannot look right here
# and wrong on a slide.
source("figures/theme.R")
draw_figure(file.path("figures", basename(script)))

message("Wrote ", out)
