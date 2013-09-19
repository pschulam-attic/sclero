# Demo visualizing the interactions between the lab and clinical
# variables recorded for the sclero project.

options(warn = -1)

require(ggplot2)
require(GGally)
require(plyr)
require(sclero)

data(list = c("clinic", "pft", "patientsplit"))

dev.patients <- unique(subset(patientsplit, dev == 1)$patient.id)
clinic <- subset(clinic, patient.id %in% dev.patients)
pft <- subset(pft, patient.id %in% dev.patients)

clinic_worst <- function(measurements) {
  w <- lifetime_worst(measurements, low = FALSE)
  ceiling(w)
}

lab_worst <- function(...) lifetime_worst(...)

clinic.worst.data <- ddply(clinic, ~ patient.id + score.type, summarize,
                           lifetime.worst = clinic_worst(score.value))

clinic.worst.data <- dcast(clinic.worst.data,
                           patient.id ~ score.type,
                           value.var = "lifetime.worst")

lab.worst.data <- ddply(pft, ~ patient.id + test.type, summarize,
                        lifetime.worst = lab_worst(perc.of.predicted))

lab.worst.data <- dcast(lab.worst.data,
                        patient.id ~ test.type,
                        value.var = "lifetime.worst")

worst.data <- merge(clinic.worst.data, lab.worst.data, by = "patient.id")

# Plot a full cross plot of the severity scores and perc. predicted

full.cross.vars <- c("skin.severity", "rp.severity", "gi.severity", "fvc", "dlco")
full.cross.data <- worst.data[, full.cross.vars]

for (var.name in full.cross.vars[1:3]) {
  full.cross.data[[var.name]] <- as.factor(full.cross.data[[var.name]])
}

ggpairs(
    full.cross.data,
    title = "Clinical Var. Interaction with FVC/DLCO",
    axisLabels = "show",
    diag = list(continuous = "bar", discrete = "bar"),
    upper = list(discrete = "ratio", combo = "facethist", continuous = "points"),
    lower = list(discrete = "blank", combo = "blank", continuous = "cor")
    )

# Plot lab var interactions

lab.vars <- c("fvc", "dlco")
lab.cross.data <- worst.data[, lab.vars]

p <- ggpairs(
    lab.cross.data,
    title = "Lab Variable Interactions",
    axisLabels = "show",
    diag = list(continuous = "bar"),
    upper = list(continuous = "points"),
    lower = list(continuous = "density")
    )

p.1.2 <- qplot(x = dlco, y = fvc, data = lab.cross.data, alpha = 0.2)
putPlot(p, p.1.2, 1, 2)
