# Week 4 reveal: one figure per briefing submitted, Faithful on the left
# and Traitors on the right, one row per option its reviewers say it
# recommends ("Not yet reviewed" until they have). Arms raised: they say
# it added carbon capture. With `silhouette_filled` TRUE, each figure
# fills red from the feet up, by the share of its reviewers who voted
# SUSPECT; the reveal page fades from the empty version to the filled one.
#
# Drawn live by `allocate-traitors.R harvest`, which supplies `verdicts`
# (one row per review: author, truth, verdict, argues, ccs) and, once the
# deadline has passed, `briefing_list` (one row per briefing submitted:
# author, truth), so every briefing is shown before anyone reviews it.
# Run on its own, it draws an invented class, so preview.R works.

argued <- c("BIOMASS", "COAL", "CLOSE")
if (!exists("verdicts")) {
  set.seed(4)
  authors <- data.frame(author = sprintf("s%02d", 1:93),
                        truth = rep(c("faithful", "traitor"), c(62, 31)),
                        argues = sample(argued, 93, TRUE, c(0.35, 0.15, 0.5)),
                        ccs = runif(93) < 0.25)
  verdicts <- authors[rep(seq_len(93), 2), ]
  p <- ifelse(verdicts$truth == "traitor", 0.6, 0.3)
  verdicts$verdict <- ifelse(runif(nrow(verdicts)) < p, "SUSPECT", "FAITHFUL")
}
if (!exists("silhouette_filled")) silhouette_filled <- TRUE
if (is.null(verdicts$ccs)) verdicts$ccs <- FALSE
harvest_time <- if (exists("harvested_at")) harvested_at else "demo data"

if (!nrow(verdicts)) {
  verdicts <- data.frame(author = character(0), truth = character(0),
                         argues = character(0), ccs = logical(0),
                         verdict = character(0))
}

# One row per briefing. Reviewers can disagree about what it argued: take
# the commonest answer.
modal <- function(x) names(sort(table(x), decreasing = TRUE))[[1]]
one_briefing <- function(d) {
  data.frame(author = d$author[[1]], truth = d$truth[[1]],
             argues = modal(d$argues), ccs = mean(d$ccs) > 0.5,
             n = nrow(d), k = sum(d$verdict == "SUSPECT"))
}
briefings <- do.call(rbind, lapply(split(verdicts, verdicts$author),
                                   one_briefing))
if (is.null(briefings)) {
  briefings <- data.frame(author = character(0), truth = character(0),
                          argues = character(0), ccs = logical(0),
                          n = integer(0), k = integer(0))
}
# Every briefing submitted, reviewed or not.
if (exists("briefing_list") && NROW(briefing_list)) {
  given <- briefing_list
  from_reviews <- briefings[match(tolower(given$author),
                                  tolower(briefings$author)), ]
  given$n <- ifelse(is.na(from_reviews$n), 0L, from_reviews$n)
  given$k <- ifelse(is.na(from_reviews$k), 0L, from_reviews$k)
  given$argues <- from_reviews$argues
  given$ccs <- from_reviews$ccs %in% TRUE
  briefings <- given[, c("author", "truth", "argues", "ccs", "n", "k")]
}
briefings$argues[is.na(briefings$argues)] <- "UNSTATED"
if (any(briefings$argues == "UNSTATED")) argued <- c(argued, "UNSTATED")
briefings <- briefings[order(match(briefings$argues, argued), briefings$truth,
                             -briefings$k / pmax(briefings$n, 1)), ]

# A pictogram person, as on a toilet door, feet at 0 and crown at 1: head,
# torso, and limbs drawn as rounded bars. Arms down, or raised in a V.
capsule <- function(x0, y0, x1, y1, r, n = 10) {
  a <- atan2(y1 - y0, x1 - x0)
  end <- function(x, y, from) {
    t <- seq(from, from + pi, length.out = n)
    data.frame(x = x + r * cos(t), y = y + r * sin(t))
  }
  rbind(end(x1, y1, a - pi / 2), end(x0, y0, a + pi / 2))
}
torso_xy <- function() {
  t <- seq(0, pi / 2, length.out = 8)
  r <- 0.05
  data.frame(x = c(-0.11, 0.11, 0.11 - r + r * cos(t),
                   -0.11 + r - r * rev(cos(t))),
             y = c(0.44, 0.44, 0.76 - r + r * sin(t),
                   0.76 - r + r * rev(sin(t))))
}
person <- function(arms_up) {
  a <- seq(0, 2 * pi, length.out = 40)
  arms <- if (arms_up) {
    list(capsule(-0.15, 0.72, -0.26, 0.93, 0.035),
         capsule(0.15, 0.72, 0.26, 0.93, 0.035))
  } else {
    list(capsule(-0.16, 0.72, -0.16, 0.47, 0.035),
         capsule(0.16, 0.72, 0.16, 0.47, 0.035))
  }
  c(list(data.frame(x = 0.085 * cos(a), y = 0.905 + 0.085 * sin(a)),
         torso_xy(),
         capsule(-0.06, 0.43, -0.06, 0.045, 0.045),
         capsule(0.06, 0.43, 0.06, 0.045, 0.045)),
    arms)
}
crown <- 1

# The part of a polygon below height h (Sutherland–Hodgman, one edge).
below <- function(p, h) {
  out <- list()
  for (i in seq_len(nrow(p))) {
    j <- i %% nrow(p) + 1L
    a_in <- p$y[[i]] <= h
    b_in <- p$y[[j]] <= h
    if (a_in) out[[length(out) + 1L]] <- p[i, ]
    if (a_in != b_in) {
      t <- (h - p$y[[i]]) / (p$y[[j]] - p$y[[i]])
      x <- p$x[[i]] + t * (p$x[[j]] - p$x[[i]])
      out[[length(out) + 1L]] <- data.frame(x = x, y = h)
    }
  }
  if (length(out) < 3L) return(NULL)
  do.call(rbind, out)
}

# Layout: each group wraps onto as many lines as its row needs.
count <- function(a, t) sum(briefings$argues == a & briefings$truth == t)
nf <- vapply(argued, count, 0, t = "faithful")
nt <- vapply(argued, count, 0, t = "traitor")
lines <- max(1, ceiling(max(nf + nt) / 26))
cols <- c(faithful = max(1, ceiling(max(nf) / lines)),
          traitor = max(1, ceiling(max(nt) / lines)))
step <- 0.64
start <- c(faithful = 0, traitor = cols[["faithful"]] * step + 1.4)
row_lines <- pmax(1, ceiling(nf / cols[["faithful"]]),
                  ceiling(nt / cols[["traitor"]]))
row_top <- -cumsum(c(0, head(row_lines, -1) * 1.08 + 0.5))
names(row_top) <- argued

shapes <- list()
fills <- list()
for (r in argued) {
  for (g in c("faithful", "traitor")) {
    b <- briefings[briefings$argues == r & briefings$truth == g, ]
    for (i in seq_len(nrow(b))) {
      x0 <- start[[g]] + ((i - 1) %% cols[[g]]) * step
      y0 <- row_top[[r]] - ((i - 1) %/% cols[[g]]) * 1.08
      id <- paste(r, g, i)
      parts <- person(b$ccs[[i]])
      for (k in seq_along(parts)) {
        p <- parts[[k]]
        shapes[[length(shapes) + 1L]] <- data.frame(
          x = p$x + x0, y = p$y + y0, id = paste(id, k), reviewed = b$n[[i]] > 0
        )
        if (silhouette_filled && b$n[[i]] > 0 && b$k[[i]] > 0) {
          part <- below(p, crown * b$k[[i]] / b$n[[i]])
          if (!is.null(part)) {
            fills[[length(fills) + 1L]] <- data.frame(
              x = part$x + x0, y = part$y + y0, id = paste(id, k)
            )
          }
        }
      }
    }
  }
}
if (!length(shapes)) {
  shapes <- list(data.frame(x = numeric(0), y = numeric(0),
                            id = character(0), reviewed = logical(0)))
}
shapes <- do.call(rbind, shapes)
fills <- if (length(fills)) do.call(rbind, fills) else NULL

headers <- data.frame(
  x = start + (cols - 1) * step / 2, y = 1.35,
  label = c(paste0("Faithful (", sum(briefings$truth == "faithful"), ")"),
            paste0("Traitors (", sum(briefings$truth == "traitor"), ")")),
  colour = c(course_colours[["cyan"]], course_colours[["purple"]])
)
row_names <- data.frame(
  x = -0.7, y = row_top - (row_lines - 1) * 0.54 + 0.45,
  label = c(BIOMASS = "Keep burning\nbiomass", COAL = "Return to\ncoal",
            CLOSE = "Let Drax\nclose", UNSTATED = "Not yet\nreviewed")[argued]
)

plot <- ggplot() +
  geom_polygon(data = shapes, aes(x, y, group = id),
               fill = course_colours[["concrete"]], colour = NA)
if (!is.null(fills)) {
  plot <- plot + geom_polygon(data = fills, aes(x, y, group = id),
                              fill = course_colours[["red"]], colour = NA)
}
plot +
  geom_text(data = headers, aes(x, y, label = label), colour = headers$colour,
            size = 7, fontface = "bold") +
  geom_text(data = row_names, aes(x, y, label = label), hjust = 1, size = 5.5,
            lineheight = 0.9, colour = course_colours[["ink"]]) +
  coord_fixed(clip = "off") +
  scale_x_continuous(expand = expansion(add = c(3.4, 0.4))) +
  scale_y_continuous(expand = expansion(add = c(0.3, 0.5))) +
  labs(x = NULL, y = NULL, caption = paste0(
    if (silhouette_filled) {
      "Red: share of reviewers voting SUSPECT. "
    } else {
      ""
    },
    "Arms raised: added carbon capture. ",
    iso_number(nrow(verdicts)), " reviews · ", harvest_time
  )) +
  theme_void(base_size = 16) +
  theme(plot.caption = element_text(colour = course_colours[["black"]]))
