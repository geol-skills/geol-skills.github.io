# Week 4 reveal: every harvested verdict against the truth.
#
# Drawn live by `allocate-traitors.R harvest`, which supplies `verdicts`
# (one row per review: truth, verdict, argues). Run on its own, it draws
# an invented class, so preview.R works.

if (!exists("verdicts")) {
  verdicts <- data.frame(
    truth = rep(c("faithful", "traitor"), c(120, 60)),
    verdict = c(rep(c("FAITHFUL", "SUSPECT"), c(86, 34)),
                rep(c("FAITHFUL", "SUSPECT"), c(27, 33)))
  )
}
harvest_time <- if (exists("harvested_at")) harvested_at else "demo data"

truths <- c("faithful", "traitor")
votes <- c("FAITHFUL", "SUSPECT")
cells <- as.data.frame(table(truth = factor(verdicts$truth, truths),
                             verdict = factor(verdicts$verdict, votes)))
# table() varies truth fastest: believed, fooled, accused, caught.
cells$name <- c("Correct", "Fooled · Type II", "Wrongly accused · Type I",
                "Caught")
cells$wrong <- seq_len(nrow(cells)) %in% c(2, 3)
cells$of <- ave(cells$Freq, cells$truth, FUN = sum)
cells$share <- ifelse(
  cells$of > 0,
  paste0(cells$Freq, " of ", cells$of, " · ",
         round(100 * cells$Freq / pmax(cells$of, 1)), "\u00a0%"),
  "none reviewed"
)

ggplot(cells, aes(truth, verdict)) +
  geom_tile(aes(fill = wrong), colour = "white", linewidth = 3) +
  geom_text(aes(label = iso_number(Freq)), size = 16, fontface = "bold",
            colour = course_colours[["ink"]], nudge_y = 0.12) +
  geom_text(aes(label = name, colour = wrong), size = 7, fontface = "bold",
            nudge_y = -0.2) +
  geom_text(aes(label = share), size = 5, nudge_y = -0.36,
            colour = course_colours[["black"]]) +
  scale_fill_manual(values = c(`FALSE` = course_fills[["sky"]],
                               `TRUE` = course_fills[["stone"]]),
                    guide = "none") +
  scale_colour_manual(values = c(`FALSE` = course_colours[["ink"]],
                                 `TRUE` = course_colours[["red"]]),
                      guide = "none") +
  scale_x_discrete(position = "top",
                   labels = c(faithful = "Actually Faithful",
                              traitor = "Actually a Traitor")) +
  # SUSPECT on top, as in the Week 3 table: Type I top left.
  scale_y_discrete(limits = votes,
                   labels = c(FAITHFUL = "Voted FAITHFUL",
                              SUSPECT = "Voted SUSPECT")) +
  labs(x = NULL, y = NULL,
       caption = paste0(iso_number(nrow(verdicts)), " reviews · ",
                        harvest_time)) +
  theme(panel.grid.major = element_blank(),
        axis.text = element_text(size = rel(1.2), face = "bold"))
