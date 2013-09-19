dev_subset <- function(data, patientsplit) {
  dev.patients <- unique(subset(patientsplit, dev == 1)$PtID)
  subset(data, patient.id %in% dev.patients)
}

add_date_since <- function(data, date.var, since.var, patient.data, baseline.var) {
  patient.baseline <- structure(as.Date(patient.data[[baseline.var]]),
                                names = patient.data$patient.id)

  data[[since.var]] <- with(data, as.Date(date.var) - patient.baseline[patient.id])
  data[[since.var]] <- as.numeric(data[[since.var]])
}
