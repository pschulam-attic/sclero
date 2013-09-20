require(sclero)
require(ggplot2)
require(plyr)

data(pft)
data(patient)

set.seed(2)

n.visits <- ddply(pft, ~ patient.id + test.type, summarize, visits = length(na.omit(test.result)))
n.fvc.visits <- subset(n.visits, test.type == "fvc")

patient.ids <- subset(n.fvc.visits, visits > 5)$patient.id

some.patients <- sample(patient.ids, 16)

plot_since <- function(data, patient.data, since.var) {
  df <- add_date_since(data, "date", "year", patient.data, since.var)
  p <- ggplot(df, aes(x = year, y = perc.of.predicted))
  p + geom_point() + facet_wrap(~ patient.id) +
      labs(title = paste("Visits since", since.var))
}

df <- subset(pft, patient.id %in% some.patients & test.type == "fvc")

plot_since(df, patient, "date.diagnosed")
plot_since(df, patient, "date.of.first.symptom")
plot_since(df, patient, "date.first.seen")
