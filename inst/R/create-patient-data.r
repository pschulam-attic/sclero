options(stringsAsFactors = FALSE)
library(plyr)
library(reshape2)

source("inst/R/sclerodata-path.r")
patient.csv <- file.path(sclerodata.path, "tPtData.csv")
patient.rdata <- file.path("data", "patient.rdata")

patient.raw <- read.csv(patient.csv)

keep.columns <- c(
    "PtID", "DOB", "Sex", "Race1", "Race2", "ethnic",
    "DateFirstSeen", "DateDiagnosis", "Date1stSymptom"
    )

patient.raw <- patient.raw[, keep.columns]

new.names <- c(
    "patient.id", "date.of.birth", "sex", "race1", "race2", "ethnicity",
    "date.first.seen", "date.diagnosed", "date.of.first.symptom"
    )

names(patient.raw) <- new.names

patient <- arrange(patient.raw, patient.id)

save(patient, file = patient.rdata)
