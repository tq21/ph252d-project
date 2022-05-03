ObsData <- read.csv(file = "delirium.csv", header = TRUE)
# surgery site one-hot encoding
ObsData$SURGSITE <- as.factor(ObsData$SURGSITE)
ObsData <- one_hot(as.data.table(ObsData))
# select covariates
tmp_func <- function(ObsData) {
  X <- select(ObsData, c(y,Prodose,
                         Age,Gender,
                         Delirium,Dementia,Depress,StrokeOrTIA,Charlest,HOCNS,
                         ETOH,cursmoke,
                         SURGSITE_1, SURGSITE_2, SURGSITE_3))
  
  EstA <- glm(y ~ Prodose+Age+Gender+
                Delirium+Dementia+Depress+StrokeOrTIA+Charlest+HOCNS+
                ETOH+cursmoke+
                SURGSITE_1+ SURGSITE_2+ SURGSITE_3,
              data=X, family = "binomial")
  EstB <- glm(y ~ (Prodose+Age+Gender+
                     Delirium+Dementia+Depress+StrokeOrTIA+Charlest+HOCNS+
                     ETOH+cursmoke+
                     SURGSITE_1+ SURGSITE_2+ SURGSITE_3)^2,
              data=X, family = "binomial")
  
  doseResp <- matrix(NA, nrow=100, ncol=3)
  for (t in 1:100) {
    dose <- ObsData
    dose$Prodose <- t*100
    RespA <- predict(EstA, newdata=dose, type='response')
    RespB <- predict(EstB, newdata=dose, type='response')
    doseResp[t,]<- c(t*100, mean(RespA), mean(RespB))
  }
  doseResp.df <-  as.data.frame(doseResp)
  return(doseResp.df)
}

B <- 1000
B_res_1 <- matrix(NA, nrow = B, ncol = 100)
B_res_2 <- matrix(NA, nrow = B, ncol = 100)
for (i in 1:B) {
  print(i)
  df <- sample_n(ObsData, size = nrow(ObsData), replace = TRUE)
  tmp <- tmp_func(df)
  B_res_1[i,] <- tmp$V2
  B_res_2[i,] <- tmp$V3
}

B_low_1 <- rep(NA, 100)
B_high_1 <- rep(NA, 100)
B_low_2 <- rep(NA, 100)
B_high_2 <- rep(NA, 100)
for (j in 1:ncol(B_res_1)) {
  tmp <- B_res_1[,j]
  tmp <- quantile(tmp, probs = c(0.025, 0.975))
  B_low_1[j] <- as.numeric(tmp[1])
  B_high_1[j] <- as.numeric(tmp[2])
  
  tmp <- B_res_2[,j]
  tmp <- quantile(tmp, probs = c(0.025, 0.975))
  B_low_2[j] <- as.numeric(tmp[1])
  B_high_2[j] <- as.numeric(tmp[2])
}

par(mfrow=c(1,3))
est_a <- tmp_func(ObsData)
x <- est_a$V1
plot(x, est_a$V2, ylim = c(0, 1),
     type = "l",
     xlim = c(0,3000),
     main = "ADRF (MSM, main terms)",
     xlab = "Propofol Dosage (mg)",
     ylab = "Prob(Delirium)")
lines(x, B_low_1, lty = 2)
lines(x, B_high_1, lty = 2)


est_b <- tmp_func(ObsData)
x <- est_a$V1
plot(x, est_a$V3, ylim = c(0, 1),
     type = "l",
     xlim = c(0,3000),
     main = "ADRF (MSM, two-way interactions)",
     xlab = "Propofol Dosage (mg)",
     ylab = "Prob(Delirium)")
lines(x, B_low_2, lty = 2)
lines(x, B_high_2, lty = 2)


plot(y ~ Prodose, data=ObsData)
# EstA
lines(V2 ~ V1, doseResp, lwd=2, col="blue")
# EstB
lines(V3 ~ V1, doseResp, lwd=2, col="red")
abline(h = 0.5, lty = 2)


