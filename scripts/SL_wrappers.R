# Estimator A (main terms)
SL.glm.EstA <- function(Y, X, newX, family, ...) {
  if (family$family == 'gaussian') {
    fit.glm <- glm(Y ~ ., data = X, family = family)
    pred <- predict(fit.glm, newdata = newX, type = 'response')
    fit <- list(object = fit.glm)
  }
  if (family$family == 'binomial'){
    fit.glm <- glm(Y ~ ., data = X, family = family)
    pred <- predict(fit.glm, newdata = newX, type = 'response')
    fit <- list(object = fit.glm)
  }
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c('SL.glm.EstA')
  return(out)
}

# Estimator B (main terms + two way interactions)
SL.glm.EstB<- function(Y, X, newX, family, ...) {
  if (family$family == 'gaussian') {
    fit.glm <- glm(Y ~ (.)^2, data = X, family = family)
    pred <- predict(fit.glm, newdata = newX, type = 'response')
    fit <- list(object = fit.glm)
  }
  if (family$family == 'binomial'){
    fit.glm <- glm(Y ~ (.)^2, data = X, family = family)
    pred <- predict(fit.glm, newdata = newX, type = 'response')
    fit <- list(object = fit.glm)
  }
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c('SL.glm.EstA')
  return(out)
}
