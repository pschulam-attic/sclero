fit_mixed_effects <- function(fixed, random, data) {
  require(nlme)

  fixed <- as.formula(fixed)
  random <- as.formula(random)

  lme.fit <- lme(fixed = fixed, random = random, data = data)

  return(lme.fit)
}

patient_coef <- function(lme.fit) {
  cf <- coef(lme.fit)
  new.names <- if (length(cf) == 1) {
    c("intercept")
  } else {
    l <- length(cf)
    c("intercept", names(cf)[2:l])
  }

  names(cf) <- new.names

  cf[["patient.id"]] <- rownames(cf)
  cf <- cf[, c("patient.id", new.names)]

  return(cf)
}
