# Week 7 — the model that fits better and predicts worse.
#
# Polished version of the "test" slide. Three models of daily GB gas
# generation, each fitted to 2025 and then asked about 2026 so far, data it
# never saw. The measure is the typical miss: mean absolute error, GWh/day.
#
#   Season         gas ~ winter                         2 coefficients
#   Sensible       gas ~ working + wind * winter        5
#   Every detail   gas ~ month * weekday * wind       168
#
# "Every detail" learns 2025 best and predicts 2026 worse than "Sensible":
# it memorised 2025. winter = cos(2 pi day / 365), 1 on 1 January.
#
# Run with the working directory set to slides/ (see figures/README.md).

library("ggplot2")
source("figures/theme.R")

daily <- read.csv("../data/gb_daily_generation.csv")
daily$date <- as.Date(daily$date)
daily <- daily[daily$date >= "2025-01-01", ]
daily$year <- as.numeric(format(daily$date, "%Y"))
daily$winter <- cos(2 * pi * as.numeric(format(daily$date, "%j")) / 365)
daily$working <- !weekdays(daily$date) %in% c("Saturday", "Sunday") &
  !daily$bank_holiday
daily$month <- months(daily$date)
daily$weekday <- weekdays(daily$date)

models <- list(
  "Season" = lm(gas_gwh ~ winter, data = daily, subset = year == 2025),
  "Sensible" = lm(gas_gwh ~ working + wind_gwh * winter,
                  data = daily, subset = year == 2025),
  "Every detail" = lm(gas_gwh ~ month * weekday * wind_gwh,
                      data = daily, subset = year == 2025)
)

miss <- function(m, yr) {
  rows <- daily$year == yr
  mean(abs(daily$gas_gwh[rows] - predict(m, daily[rows, ])))
}
scores <- expand.grid(model = names(models), year = c(2025, 2026),
                      stringsAsFactors = FALSE)
scores$miss <- mapply(function(m, y) miss(models[[m]], y),
                      scores$model, scores$year)
scores$data <- ifelse(scores$year == 2025, "2025\n(learnt from)",
                      "2026 so far\n(never seen)")
n_coef <- vapply(models, function(m) length(coef(m)), numeric(1))
scores$label <- sprintf("%s (%d)", scores$model, n_coef[scores$model])
scores$label <- factor(scores$label, unique(scores$label))

ggplot(scores, aes(x = data, y = miss, colour = label, group = label)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3.5) +
  geom_text(data = scores[scores$year == 2026, ],
            aes(label = label), hjust = -0.12, size = 5.2,
            show.legend = FALSE) +
  scale_colour_manual(values = course_palette(3), guide = "none") +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.9))) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = NULL, y = "Typical miss (GWh a day)",
       caption = paste("Mean absolute error; (n) = coefficients estimated.",
                       "Data: NESO generation mix, Met Office HadCET, gov.uk"))
