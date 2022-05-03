library(SuperLearner)
library(dplyr)
source("SL_wrappers.R")

defaultW <- getOption("warn")
options(warn = -1)

df_all <- read.csv("delirium.csv")
df_all <- df_all %>% 
  select(-c(X, STUDYNO, Anesdur))
# df_all$y <- as.factor(df_all$y)
# df_all$Gender <- as.factor(df_all$Gender)
# df_all$HOCNS <- as.factor(df_all$HOCNS)
# df_all$Delirium <- as.factor(df_all$Delirium)
# df_all$cursmoke <- as.factor(df_all$cursmoke)
# df_all$SURGSITE <- as.factor(df_all$SURGSITE)
# df_all$Charlest <- as.factor(df_all$Charlest)
# df_all$Dementia <- as.factor(df_all$Dementia)
# df_all$Depress <- as.factor(df_all$Depress)
# df_all$StrokeOrTIA <- as.factor(df_all$StrokeOrTIA)

est_dose_response <- function(df) {
  # first stage: SL for estimating generalized propensity score
  SL.library <- c('SL.glm.EstA', 'SL.glm.EstB', 'SL.glmnet', 
                  'SL.bayesglm')
  X <- select(df, -c(y, Prodose))
  SL.out <- SuperLearner(Y = df$Prodose, X = X, SL.library = SL.library,
                         family = 'gaussian', cvControl = list(V = 10))
  pred_1 <- as.vector(SL.out$SL.predict)
  
  # second stage: SL for estimating the conditional mean
  X <- select(df, Prodose) %>% 
    mutate(Prodose_pred = pred_1)
  
  # density estimation
  prodose_vals <- seq(min(df$Prodose), 3000, length.out = 200)
  d <- density(pred_1)
  pred_den <- approx(d$x,d$y,xout=prodose_vals)$y
  pred_den[is.na(pred_den)] <- 0
  
  newX <- data.frame(Prodose = prodose_vals,
                     Prodose_pred = pred_den)
  
  SL.library <- c('SL.glm.EstA', 'SL.glm.EstB', 'SL.glmnet', 
                  'SL.bayesglm')
  SL.out_2 <- SuperLearner(Y = df$y, X = X, SL.library = SL.library,
                           family = 'binomial', cvControl = list(V = 10),
                           newX = newX)
  pred_2 <- as.vector(SL.out_2$SL.predict)
  return(pred_2)
}

set.seed(123)
B <- 1000
B_res <- matrix(NA, nrow = B, ncol = 200)
for (i in 1:B) {
  print(i)
  df <- sample_n(df_all, size = nrow(df_all), replace = TRUE)
  B_res[i,] <- est_dose_response(df)
}
B_low <- rep(NA, 200)
B_high <- rep(NA, 200)
for (j in 1:ncol(B_res)) {
  tmp <- B_res[,j]
  tmp <- quantile(tmp, probs = c(0.025, 0.975))
  B_low[j] <- as.numeric(tmp[1])
  B_high[j] <- as.numeric(tmp[2])
}

est <- est_dose_response(df_all)
x <- seq(min(df_all$Prodose), 3000, length.out = 200)
plot(x, est, ylim = c(0, 1),
     type = "l",
     xlim = c(0,3000),
     main = "ADRF (Super Learner)",
     xlab = "Propofol Dosage (mg)",
     ylab = "Prob(Delirium)")
lines(x, B_low, lty = 2)
lines(x, B_high, lty = 2)

options(warn = defaultW)



# density estimation
SL.library <- c("SL.randomForest")
X <- select(df, -c(y, Prodose))
SL.out <- SuperLearner(Y = df$Prodose, X = X, SL.library = SL.library,
                       family = 'gaussian', cvControl = list(V = 10))
pred <- as.vector(SL.out$SL.predict)

par(mfrow=c(1,2))
plot(density(pred), xlim = c(0, 2500))
plot(density(df_all$Prodose), xlim = c(0, 2500))

d <- density(pred)
approx(d$x,d$y,xout=500)$y

r <- randomForest(x = X, y = df_all$Prodose)
pred <- predict(r)
par(mfrow=c(1,2))
plot(density(pred), xlim = c(0, 2500))
plot(density(df_all$Prodose), xlim = c(0, 2500))
