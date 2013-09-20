# Predict the worst FVC measurements that a person will have base on
# demographic data.

library(sclero)
library(ggplot2)
library(reshape2)
library(plyr)

data(patient)
data(pft)

patient.worst <- ddply(pft, ~ patient.id + test.type, summarize, lifetime.worst = lifetime_worst(perc.of.predicted))
patient.worst <- dcast(patient.worst, patient.id ~ test.type, value.var = "lifetime.worst")

data <- merge(patient, patient.worst, by = "patient.id")

years_bw <- function(d1, d2) as.numeric(as.Date(d2) - as.Date(d1)) / 365

data <- transform(data, age = years_bw(date.of.birth, date.of.first.symptom))

sex.str <- c("male", "female")
aa.str <- c("no", "yes")

data <- transform(data, african.american = ifelse(race1 == 2, 1, 0))
data <- data[, c("patient.id", "age", "sex", "african.american", "fvc", "dlco")]
data <- na.omit(data)
data <- transform(data,
                  sex = as.factor(sex.str[sex + 1]),
                  african.american = as.factor(aa.str[african.american + 1]))

lm.fit <- lm(fvc ~ 1 + age + sex + african.american, data = data)
