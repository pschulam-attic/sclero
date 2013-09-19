fit_mixed_effects <- function(fixed, random, data) {
  require(nlme)

  fixed <- as.formula(fixed)
  random <- as.formula(random)

  lme.fit <- lme(fixed = fixed, random = random, data = data)

  return(lme.fit)
}
