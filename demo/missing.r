require(plyr)
require(reshape2)
require(sclero)
require(ggplot2)
require(GGally)

data(patient)
data(clinic)
data(pft)
data(sero)

npatients <- length(unique(patient$patient.id))
nclinic <- length(unique(clinic$patient.id))
npft <- length(unique(pft$patient.id))
nsero <- length(unique(sero$patient.id))

worst.pft <- ddply(pft, ~ patient.id + test.type, summarize,
                   test.worst = median(quantile(perc.of.predicted, 0.1, na.rm=TRUE)))
worst.pft <- dcast(worst.pft, patient.id ~ test.type, value.var = "test.worst")

healthy.patients <- unique(subset(worst.pft, fvc > 70)$patient.id)

require(boot)

make_bootfn <- function(var.name) {
  function(df) {
    bootfn <- function(df, idx) {
      df <- df[idx, ]
      obs <- sum(!is.na(df[[var.name]]))
      mis <- sum(is.na(df[[var.name]]))
      c(obs, mis)
    }
    
    b <- boot(df, bootfn, 100)
    obs.mean <- mean(b$t[, 1])
    obs.se <- sd(b$t[, 1])
    mis.mean <- mean(b$t[, 2])
    mis.se <- sd(b$t[, 2])
    data.frame(observed = obs.mean,
               observed.se = obs.se,
               missing = mis.mean,
               missing.se = mis.se)
  }
}

massage <- function(df) {
  df <- melt(df, measure.vars = c("observed", "missing"))
  df <- transform(df, value.se = ifelse(variable == "observed", observed.se, missing.se))
  df <- subset(df, select = -c(observed.se, missing.se))  
}

plotbars <- function(df, title) {
  p <- ggplot(df, aes(x = variable, y = value, fill = variable)) + geom_bar(stat="identity")
  p <- p + geom_errorbar(aes(ymin = value - value.se, ymax = value + value.se, width=0.25))
  p + facet_grid(score.type ~ healthy) + labs(title = title)
}

clinic <- transform(clinic, healthy = ifelse(patient.id %in% healthy.patients, "healthy", "sick"))
clinic.counts <- ddply(clinic, ~ score.type + healthy, make_bootfn("score.value"))
clinic.counts <- massage(clinic.counts)
plotbars(clinic.counts, "Missing Clinical Variables") + facet_grid(score.type ~ healthy)

pft <- transform(pft, healthy = ifelse(patient.id %in% healthy.patients, "healthy", "sick"))
pft.counts <- ddply(pft, ~ test.type + healthy, make_bootfn("test.result"))
pft.counts <- massage(pft.counts)
plotbars(pft.counts, "Missing PFT Variables") + facet_grid(test.type ~ healthy)

sero <- transform(sero, healthy = ifelse(patient.id %in% healthy.patients, "healthy", "sick"))
sero$antibody.result[sero$antibody.result == 9] <- NA
sero.counts <- ddply(sero, ~ antibody.type + healthy, make_bootfn("antibody.result"))
sero.counts <- massage(sero.counts)
plotbars(sero.counts, "Missing Sero. Variables") + facet_grid(antibody.type ~ healthy)
