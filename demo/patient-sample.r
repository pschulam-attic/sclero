require(sclero)
require(ggplot2)
require(plyr)

data(pft)
data(patient)

set.seed(1)

df <- add_date_since(pft, "date", "year", patient, "date.of.first.symptom")
n.train.visits <- ddply(subset(df, year >= 0 & year < 2), ~ patient.id + test.type, summarize, visits = length(na.omit(test.result)))
n.test.visits <- ddply(subset(df, year >= 3 & year < 7), ~ patient.id + test.type, summarize, visits = length(na.omit(test.result)))

n.train.fvc <- subset(n.train.visits, test.type == "fvc")
n.test.fvc <- subset(n.test.visits, test.type == "fvc")

patient.ids <- intersect(subset(n.train.fvc, visits >= 3)$patient.id,
                         subset(n.test.fvc, visits >= 1)$patient.id)

some.patients <- sample(patient.ids, 16)

df <- subset(df, patient.id %in% some.patients & test.type == "fvc")

p <- ggplot(df, aes(x = year, y = perc.of.predicted))
p <- p + geom_point() + facet_wrap(~ patient.id)
p <- p + geom_hline(yintercept = 70, color = "red", linetype = "dashed")
p + labs(title = "Sample Patient Trajectories")
