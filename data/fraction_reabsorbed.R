# How much of a felled stand's carbon has the replanted stand taken back?
#
# years:     years since felling and replanting (a vector is fine)
# forest:    one of the `forest` values in data/forest_growth.csv
# felled_at: age of the stand when it was felled, in years
#
# Returns carbon in the new stand at `years`, divided by carbon in the old
# stand at `felled_at`. This rises from 0 (at felling), to 1 when the new stand
# holds as much as the old one did, and higher, if the new stand outgrows the
# old. Ages between the table's 5-year steps are interpolated along a
# straight line; ages beyond the table give NA.
fraction_reabsorbed <- function(years, forest, felled_at) {
  growth <- read.csv("data/forest_growth.csv")
  stand <- growth[growth$forest == forest, ]
  if (nrow(stand) == 0) {
    stop("No forest called \"", forest, "\". Choose from: ",
         paste(unique(growth$forest), collapse = ", "))
  }
  carbon_at <- function(age) approx(stand$age, stand$carbon_t_per_ha, age)$y
  carbon_at(years) / carbon_at(felled_at)
}
