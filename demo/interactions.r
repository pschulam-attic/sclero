# Demo visualizing the interactions between the lab and clinical
# variables recorded for the sclero project.

options(warn = -1)

require(ggplot2)
require(GGally)
library(grid)
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
    na.omit(full.cross.data),
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

# Plot lab var coef interactions

get_var_slope <- function(pft.data, patient.data, test.type.name) {
  pft.data <- subset(pft, test.type == test.type.name)
  pft.data <- add_date_since(pft.data, "date", "year", patient.data, "date.diagnosed")
  pft.data <- na.omit(pft.data)

  lme.fit <- fit_mixed_effects(perc.of.predicted ~ 1 + year, ~ 1 + year | patient.id, data = pft.data)
  cf.data <- patient_coef(lme.fit)
  cf.data <- cf.data[, c("patient.id", "year")]
  names(cf.data) <- c("patient.id", paste0(test.type.name, ".slope"))

  return(cf.data)
}

fvc.slopes <- get_var_slope(pft, patient, "fvc")
dlco.slopes <- get_var_slope(pft, patient, "dlco")
all.slopes <- merge(fvc.slopes, dlco.slopes, by = "patient.id")

p <- ggpairs(
    all.slopes, columns = c(2, 3),
    title = "Lab Variable Slope Interactions",
    axisLabels = "internal",
    diag = list(continuous = "bar"),
    upper = list(continuous = "points"),
    lower = list(continuous = "density")
    )

p.1.2 <- ggplot(all.slopes, aes(x = dlco.slope, y = fvc.slope))
p.1.2 <- p.1.2 + geom_point(alpha = 0.2) +
    geom_abline(aes(slope = 1), alpha = 0.5, color = "blue", linetype = "dashed") +
    geom_hline(aes(yintercept = 0), alpha = 0.5, color = "red") +
    geom_vline(aes(xintercept = 0), alpha = 0.5, color = "red")


putPlot(p, p.1.2, 1, 2)

# Plot clinical variable interactions

clinic.cross.vars <- c("skin.severity", "rp.severity", "gi.severity")
clinic.cross.data <- worst.data[, clinic.cross.vars]

p1 <- ggplot(clinic.cross.data, aes(x = skin.severity, y = rp.severity)) + geom_jitter(alpha = 0.2)
p2 <- ggplot(clinic.cross.data, aes(x = skin.severity, y = gi.severity)) + geom_jitter(alpha = 0.2)
p3 <- ggplot(clinic.cross.data, aes(x = rp.severity, y = gi.severity)) + geom_jitter(alpha = 0.2)

grid.arrange(p1, p2, p3, nrow = 2, ncol = 2, main = "Clinical Var. Interactions")

# Plot total skin score against severity scores

total.skin.vars <- c("total.skin", "rp.severity", "gi.severity")
total.skin.data <- worst.data[, total.skin.vars]
total.skin.data <- melt(total.skin.data, measure.vars = c("rp.severity", "gi.severity"))

p <- ggplot(total.skin.data, aes(x = total.skin, y = value))
p <- p + geom_jitter(alpha = 0.2) + facet_wrap(~ variable, ncol = 1)
p + labs(title = "Total Skin Score Against Severity Scores")

# Plot total skin score against lab variables

total.skin.vars <- c("total.skin", "fvc", "dlco")
total.skin.data <- worst.data[, total.skin.vars]
total.skin.data <- melt(total.skin.data, measure.vars = c("fvc", "dlco"))

p <- ggplot(total.skin.data, aes(x = total.skin, y = value))
p <- p + geom_point(alpha = 0.2) + geom_smooth() + facet_wrap(~ variable, ncol = 1)
p + labs(title = "Total Skin Score Against PFTs")

# FVC against clinic histograms

clinic.hist.data <- melt(worst.data, measure.vars = c("skin.severity", "rp.severity", "gi.severity"))
fvc.median.data <- ddply(clinic.hist.data, ~ variable + value, summarize, fvc.med = median(fvc, na.rm = TRUE))

p <- ggplot(clinic.hist.data, aes(x = fvc))
p <- p + geom_vline(aes(xintercept = fvc.med), data = fvc.median.data, color = "red", linetype = "dashed")
p <- p + facet_grid(value ~ variable) + labs(title = "FVC Against Clinical Measurements (Red = Median)")
p + geom_histogram(binwidth = 2, alpha = 0.5) + xlim(50, 100)

# DLCO against clinic histograms

dlco.median.data <- ddply(clinic.hist.data, ~ variable + value, summarize, dlco.med = median(dlco, na.rm = TRUE))

p <- ggplot(clinic.hist.data, aes(x = dlco))
p <- p + geom_vline(aes(xintercept = dlco.med), data = dlco.median.data, color = "red", linetype = "dashed")
p <- p + facet_grid(value ~ variable) + labs(title = "DLCO Against Clinical Measurements (Red = Median)")
p + geom_histogram(binwidth = 2, alpha = 0.5) + xlim(50, 100)
