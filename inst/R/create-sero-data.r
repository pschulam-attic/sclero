options(stringsAsFactors = FALSE)
library(plyr)
library(reshape2)

source("inst/R/sclerodata-path.r")
sero.csv <- file.path(sclerodata.path, "tSero.csv")
sero.rdata <- file.path("data", "sero.rdata")

sero.raw <- read.csv(sero.csv)

keep.columns <- c(
    "PtID", "Sample.Date",
    "ANA", "ACA", "Scl.70", "Ro", "La", "RNP",
    "Sm", "DNA", "RNA.Polymerase.I.III"
    )

sero.raw <- sero.raw[, keep.columns]

new.names <- c(
    "patient.id", "date",
    "ana", "aca", "scl.70", "ro", "la", "rnp",
    "sm", "dna", "rna.poly"
    )

names(sero.raw) <- new.names

sero <- melt(sero.raw, measure.vars = c("aca", "scl.70", "ro", "la", "rnp", "sm", "dna", "rna.poly"),
             variable.name = "antibody.type", value.name = "antibody.result")

sero <- arrange(sero, patient.id, date, antibody.type)

save(sero, file = sero.rdata)
