options(stringsAsFactors = FALSE)
library(plyr)
library(reshape2)

source("inst/R/sclerodata-path.r")
clinic.csv <- file.path(sclerodata.path, "tVisit.csv")
clinic.rdata <- file.path("data", "clinic.rdata")

clinic.raw <- read.csv(clinic.csv)

keep.columns <- c(
    "PtID", "Visit.Date", "Total.Skin.Score",
    "Skin.Sev.Score", "RP.Sev.Score", "GI.Sev.Score"
    )

clinic.raw <- clinic.raw[, keep.columns]

new.names <- c(
    "patient.id", "date", "total.skin",
    "skin.severity", "rp.severity", "gi.severity"
    )

names(clinic.raw) <- new.names

clinic <- melt(clinic.raw, measure.vars = c("total.skin", "skin.severity", "rp.severity", "gi.severity"),
               variable.name = "score.type", value.name = "score.value")

clinic <- arrange(clinic, patient.id, date, score.type)

save(clinic, file = clinic.rdata)
