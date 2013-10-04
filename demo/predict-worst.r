# Predict the worst FVC measurements that a person will have base on
# demographic data.

library(ggplot2)
library(reshape2)
library(plyr)

data(patient)
data(pft)
data(sero)

patient.worst <- ddply(pft, ~ patient.id + test.type, summarize, lifetime.worst = lifetime_worst(perc.of.predicted))
patient.worst <- dcast(patient.worst, patient.id ~ test.type, value.var = "lifetime.worst")

sero$ana[sero$ana == 9] <- NA
sero$antibody.result[sero$antibody.result == 9] <- NA

fun.agg <- function(x) {
  x <- na.omit(x)
  if (length(x) > 0) mean(x) else NaN
}

sero.wide <- dcast(sero, patient.id + date ~ antibody.type, value.var = "antibody.result", fun.aggregate = fun.agg)

for (v.name in names(sero.wide)[3:length(sero.wide)]) {
  isnan <- is.nan(sero.wide[[v.name]])
  sero.wide[[v.name]][isnan] <- NA
}

max.na <- function(x) {
  x <- na.omit(x)
  if (length(x) > 0) max(x) else NA
}

sero.data <- ddply(sero.wide, ~ patient.id,
                   summarize,
                   scl.70 = max.na(scl.70),
                   aca = max.na(aca))

data <- merge(patient, patient.worst, by = "patient.id")
data <- merge(data, sero.data, by = "patient.id")

sero.data <- ddply(subset(sero.wide, antibody.type = "scl.70"), ~ patient.id, summarize, scl.70 = max.na(antibody.result))

data <- merge(data, sero.wide, by = "patient.id")

years_bw <- function(d1, d2) as.numeric(as.Date(d2) - as.Date(d1)) / 365

data <- transform(data, age = years_bw(date.of.birth, date.of.first.symptom))

sex.str <- c("male", "female")
aa.str <- c("no", "yes")

data <- transform(data, african.american = ifelse(race1 == 2, 1, 0))
data <- data[, c("patient.id", "age", "sex", "african.american", "smoker", "fvc", "dlco", "scl.70")]
data <- na.omit(data)
data <- transform(data,
                  sex = as.factor(sex.str[sex + 1]),
                  african.american = as.factor(aa.str[african.american + 1]),
                  smoker = as.factor(smoker),
                  scl.70 = as.factor(scl.70))

lm.fit <- lm(fvc ~ 1 + age + sex + smoker + african.american + dlco + scl.70, data = data)
