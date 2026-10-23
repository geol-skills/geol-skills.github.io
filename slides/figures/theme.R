# Shared palette and ggplot2 theme for slide figures.
#
# Sourced by every figure script; edit here and every figure follows.

library("ggplot2")

# Palette --------------------------------------------------------------

# Read the Durham palette from the .gpl rather than hard-coding hexes, so
# the colours have one definition. durham.gpl is derived from the master at
# GitHub/preferences/Inkscape/durham.gpl — vendored because the deploy
# runner cannot see a local path, and extended with the accessible variants
# the file itself documents.
read_gpl <- function(path) {
  lines <- readLines(path, warn = FALSE)
  entries <- grep("^\\s*\\d+\\s+\\d+\\s+\\d+\\s+\\S", lines, value = TRUE)
  parts <- regmatches(
    entries,
    regexec("^\\s*(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(.*?)\\s*$", entries)
  )
  hex <- vapply(parts, function(p) {
    grDevices::rgb(as.integer(p[2]), as.integer(p[3]), as.integer(p[4]),
                   maxColorValue = 255)
  }, character(1))
  names(hex) <- vapply(parts, function(p) {
    gsub(" ", "_", tolower(sub("\\s*#[0-9A-Fa-f]{6}\\s*$", "", p[5])))
  }, character(1))
  hex
}

durham <- read_gpl("figures/durham.gpl")

# WCAG relative luminance, and the contrast ratio between two colours.
relative_luminance <- function(hex) {
  v <- grDevices::col2rgb(hex)[, 1] / 255
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  sum(v * c(0.2126, 0.7152, 0.0722))
}

contrast_ratio <- function(a, b = "#FFFFFF") {
  l <- c(relative_luminance(a), relative_luminance(b))
  (max(l) + 0.05) / (min(l) + 0.05)
}

# Series colours, most distinct first. Durham Cyan, Gold and Concrete are
# too light to carry a line on white, so the accessible variants stand in
# under the plain names — a figure script asks for "cyan" and gets a cyan
# that can actually be seen.
#
# Red is last because it means "wrong" across these decks — the misleading
# charts, the mean that misrepresents, the null being rejected. A palette
# that handed it to the second series of an ordinary chart would spend that
# meaning by accident, so course_palette() reaches it only at n = 7.
course_colours <- c(
  ink      = unname(durham[["ink"]]),         # default for a single series
  cyan     = unname(durham[["cyan_aa"]]),
  purple   = unname(durham[["purple"]]),
  gold     = unname(durham[["gold_aa"]]),
  black    = unname(durham[["black"]]),
  concrete = unname(durham[["concrete_aa"]]),  # de-emphasized series
  red      = unname(durham[["red"]])
)

# Pastels, for filled areas. These are the unmodified Durham tints and none
# of them reaches 3:1 on white, so a shape filled with one needs a darker
# outline to define its edge — see the histogram in w02-distribution-shapes.
course_fills <- c(
  sky     = unname(durham[["sky"]]),
  stone   = unname(durham[["stone"]]),
  heather = unname(durham[["heather"]]),
  cedar   = unname(durham[["cedar"]])
)

# Guard against a future edit quietly reintroducing an unreadable series
# colour. Fills are exempt: they are outlined, not read on their own.
local({
  weak <- Filter(function(n) contrast_ratio(course_colours[[n]]) < 4.5,
                 names(course_colours))
  if (length(weak)) {
    stop("Series colours below 4.5:1 on white: ", paste(weak, collapse = ", "))
  }
})

# Use for discrete scales: scale_colour_manual(values = course_palette(3))
# Seven is the hard ceiling; four or five is the practical one, past which a
# slide stops being readable whatever the colours.
course_palette <- function(n = length(course_colours)) {
  if (n > length(course_colours)) {
    stop("course_palette() has ", length(course_colours), " colours; ",
         "asked for ", n, ". Group the data instead.")
  }
  unname(course_colours[seq_len(n)])
}

# Theme ----------------------------------------------------------------

# Gutter between side-by-side panels. At ggplot2's default (5.5 pt) two
# columns of facets read as one wide chart (issue #162; Week 6, "When
# assumptions are violated"), so every multi-column figure gets this much
# white space between columns: facets through theme_slide(), composed
# plots through side_by_side(). Base-R panels: see figures/README.md.
slide_gutter <- unit(3, "lines")

# base_size 16 suits a figure rendered at fig-width 9 on a 1280x720 slide.
# base_family is left at the default: "Source Sans Pro" is not installed on
# the CI runner and a missing family raises warnings on every render.
#
# Subtitle and caption are de-emphasized by size, not by a paler grey:
# Durham's light neutrals (Concrete, Cedar) fall below WCAG contrast on
# white. Grid lines are neutral chrome rather than a palette colour, so
# they never read as a data series.
theme_slide <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      text             = element_text(colour = course_colours[["ink"]]),
      axis.text        = element_text(colour = course_colours[["ink"]]),
      plot.title       = element_text(face = "bold", size = rel(1.1)),
      plot.subtitle    = element_text(colour = course_colours[["black"]],
                                      size = rel(0.85)),
      plot.caption     = element_text(colour = course_colours[["black"]],
                                      size = rel(0.7)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#e5e8ea"),
      panel.spacing.x  = slide_gutter,
      plot.margin      = margin(5, 10, 5, 5)
    )
}

theme_set(theme_slide())

# Counts are whole numbers, so their axis is labelled with whole numbers:
# ggplot2's default breaks put 2.5 and 7.5 on a "Boreholes" axis. Add
# scale_y_count() to any histogram or bar chart whose axis counts things.
# It keeps ggplot2's default breaks and drops the fractional ones, so tick
# density matches every other figure.
count_breaks <- function(n = 5) {
  function(limits) {
    breaks <- scales::extended_breaks(n)(limits)
    breaks <- breaks[breaks == round(breaks)]
    if (length(breaks) < 2) {
      breaks <- seq(ceiling(limits[1]), floor(limits[2]))
    }
    breaks
  }
}

scale_y_count <- function(...) {
  scale_y_continuous(breaks = count_breaks(), ...)
}

# Base-R analogue of theme_slide(), for the Week 1 figures: students have
# not met ggplot2 yet, so those scripts use plot() and barplot(). Call it
# immediately before plotting — par() applies to the open device only.
par_slide <- function() {
  par(
    mar      = c(5.4, 4.6, 2.6, 1),
    cex.axis = 1.1,
    cex.lab  = 1.2,
    cex.main = 1.3,
    col.axis = course_colours[["ink"]],
    col.lab  = course_colours[["ink"]],
    col.main = course_colours[["ink"]],
    fg       = course_colours[["ink"]],
    bty      = "n",
    las      = 1
  )
}

# The caption slot theme_slide() gives a ggplot; base R has none.
add_source <- function(text) {
  mtext(text, side = 1, line = 4.2, adj = 1, cex = 0.75,
        col = course_colours[["black"]])
}

# Numbers ---------------------------------------------------------------

# Numbers follow ISO 80000-1: no comma separators. Four-digit numbers stay
# whole (1000); five or more digits are grouped in threes with a narrow
# no-break space, U+202F (10 000), so the groups never wrap apart. Use for
# any number pasted into a figure's text, and as labels = iso_number on an
# axis that reaches 10 000. Years are four digits, so they are unaffected.
iso_number <- function(x, ...) {
  out <- format(x, trim = TRUE, scientific = FALSE, ...)
  big <- !is.na(x) & abs(x) >= 1e4
  out[big] <- format(x[big], big.mark = "\u202f", trim = TRUE,
                     scientific = FALSE, ...)
  out
}

# Drawing from a deck ---------------------------------------------------

# Runs a figure script and prints the plot it ends with. source() returns
# the last expression's value, which ggplot() would otherwise not print
# from inside a knitr chunk.
#
# Week 1 scripts use base graphics, which draw as a side effect and return
# NULL; printing that would put a literal "NULL" under the figure.
draw_figure <- function(script) {
  value <- source(script)$value
  if (!is.null(value)) {
    print(value)
  }
}

# Two ggplots side by side, for panels that cannot share a facet (different
# geoms or axes). It draws the pair and returns invisible(NULL), so make it
# the script's last line and draw_figure() prints nothing over it. widths
# are the shares of the device each panel takes.
side_by_side <- function(left, right, widths = c(1, 1)) {
  # Half the gutter on each inner edge, so the columns sit slide_gutter apart.
  inner_edge <- function(p, side) {
    m <- theme_get()$plot.margin
    m[side] <- slide_gutter * 0.5
    p + theme(plot.margin = m)
  }
  grid::grid.newpage()
  layout <- grid::grid.layout(1, 2, widths = grid::unit(widths, "null"))
  grid::pushViewport(grid::viewport(layout = layout))
  print(inner_edge(left, 2),
        vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(inner_edge(right, 4),
        vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  invisible(NULL)
}

# Source link ----------------------------------------------------------

# Emits the "source" footnote printed under a figure, so a student can open
# the exact script that drew it. Use inline: `r fig_source("x.R")`
#
# The link is deck-relative because the source repository is private; the
# script resolves because _quarto.yml publishes figures/*.R beside the deck.
fig_source <- function(script) {
  paste0("[", script, "](figures/", script, ")")
}
