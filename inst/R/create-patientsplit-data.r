options(stringsAsFactors = FALSE)
library(plyr)
library(reshape2)

source("inst/R/sclerodata-path.r")
patient.csv <- file.path(sclerodata.path, "tPtData.csv")
patientsplit.rdata <- file.path("data", "patientsplit.rdata")

patient.raw <- read.csv(patient.csv)
patient.raw <- arrange(patient.raw, PtID)

set.seed(1)
patient.ids <- unique(patient.raw$PtID)
patientsplit <- data.frame(patient.id = patient.ids,
                           dev = rbinom(length(patient.ids), 1, 0.5))

save(patientsplit, file = patientsplit.rdata)
