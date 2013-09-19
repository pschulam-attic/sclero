dev_subset <- function(data, patientsplit) {
  dev.patients <- unique(subset(patientsplit, dev == 1)$PtID)
  data <- subset(data, patient.id %in% dev.patients)

  return(data)
}

add_date_since <- function(data, date.var, since.var, patient.data,
                           baseline.var, as.year = TRUE) {
  patient.baseline <- structure(as.Date(patient.data[[baseline.var]]),
                                names = patient.data$patient.id)

  dates <- as.Date(data[[date.var]])
  data[[since.var]] <- dates - patient.baseline[data$patient.id]

  div <- if (as.year) 365 else 1
  data[[since.var]] <- as.numeric(data[[since.var]]) / div

  return(data)
}
