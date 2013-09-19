options(stringsAsFactors = FALSE)
library(plyr)
library(reshape2)

source("inst/R/sclerodata-path.r")
pft.csv <- file.path(sclerodata.path, "tPFT.csv")
pft.rdata <- file.path("data", "pft.rdata")

pft.raw <- read.csv(pft.csv)

keep.columns <- c(
    "PtID", "Date",
    "Height", "Weight", "age",
    "FVC.Pre", "perc.FVC.of.predicted",
    "DLCO", "perc.DLCO.of.predicted"
    )

pft.raw <- pft.raw[, keep.columns]

new.names <- c(
    "patient.id", "date",
    "height", "weight", "age",
    "fvc", "perc.fvc",
    "dlco", "perc.dlco"
    )

names(pft.raw) <- new.names

pft <- melt(pft.raw, measure.vars = c("fvc", "dlco"),
            variable.name = "test.type", value.name = "test.result")

pft <- transform(pft, perc.of.predicted = ifelse(test.type == "fvc", perc.fvc, perc.dlco))
pft <- subset(pft, select = -c(perc.fvc, perc.dlco))

pft <- arrange(pft, patient.id, date, test.type)

save(pft, file = pft.rdata)
