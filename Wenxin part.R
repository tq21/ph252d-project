setwd("~/Desktop/2022 Spring/PH252D Causal Inference/project")
delirium<-read.csv("delirium.csv")

#dichotomize treatment, age and duration
delirium$hi_prodose = ifelse (delirium$Prodose>median(delirium$Prodose), 1, 0)
delirium$old = ifelse (delirium$Age>median(delirium$Age), 1, 0)
delirium$longanes = ifelse (delirium$Anesdur>median(delirium$Anesdur), 1, 0) 
delirium$alcohol = ifelse (delirium$ETOH!=0, 1, 0)

#evaluate positivity
table(delirium[, c("hi_prodose", "Gender", "old", "HOCNS", "Delirium", "cursmoke",
                   "alcohol", "SURGSITE", "longanes", "Charlest", "Dementia", 
                   "Depress", "StrokeOrTIA")])
#violated, too many strata

#generate iptw weights
library("nnet")
gAC.reg<-multinom(hi_prodose~ Gender + old + HOCNS + Delirium + cursmoke + alcohol + SURGSITE +
                     Charlest + Dementia + Depress + StrokeOrTIA, data=delirium)
gAC.pred<- predict(gAC.reg, type="probs")
gAC<-rep(NA, nrow(delirium))
gAC[delirium$hi_prodose==1] <- gAC.pred[delirium$hi_prodose==1]
gAC[delirium$hi_prodose==0] <- gAC.pred[delirium$hi_prodose==0]
wt<-1/gAC
summary(wt)

delirium$wt<-wt

#direct IPTW estimand
Y_hi<-weighted.mean(delirium[delirium$hi_prodose==1,]$y,delirium[delirium$hi_prodose==1,]$wt)
Y_low<-weighted.mean(delirium[delirium$hi_prodose==0,]$y,delirium[delirium$hi_prodose==0,]$wt)
Y_hi-Y_low
#0.00894851

#bootstrap variance calculation
library(boot)

function_1 <- function(data,i){
  temp<-data[i,]
  gAC.reg<-multinom(hi_prodose~ Gender + old + HOCNS + Delirium + cursmoke + alcohol + SURGSITE +
                       Charlest + Dementia + Depress + StrokeOrTIA, data=temp)
  gAC.pred<- predict(gAC.reg, type="probs")
  gAC<-rep(NA, nrow(temp))
  gAC[temp$hi_prodose==1] <- gAC.pred[temp$hi_prodose==1]
  gAC[temp$hi_prodose==0] <- gAC.pred[temp$hi_prodose==0]
  wt<-1/gAC
  temp$wt<-wt
  Y_hi<-weighted.mean(temp[temp$hi_prodose==1,]$y,temp[temp$hi_prodose==1,]$wt)
  Y_low<-weighted.mean(temp[temp$hi_prodose==0,]$y,temp[temp$hi_prodose==0,]$wt)
  return(Y_hi-Y_low)
}
set.seed(1)
bootstrap_1 <- boot(delirium[, -21],function_1,R=1000)
boot.ci(boot.out=bootstrap_1)
# (-0.0593,  0.3871 )

###########################

#stabilized IPTW estimand
Ys_hi<-Y_hi/mean(delirium[delirium$hi_prodose==1,]$wt)
Ys_low<-Y_low/mean(delirium[delirium$hi_prodose==0,]$wt)
Ys_hi-Ys_low
#0.06119214

#bootstrap variance 
function_2 <- function(data,i){
  temp<-data[i,]
  gAC.reg<-multinom(hi_prodose~ Gender + old + HOCNS + Delirium + cursmoke + alcohol + SURGSITE +
                       Charlest + Dementia + Depress + StrokeOrTIA, data=temp)
  gAC.pred<- predict(gAC.reg, type="probs")
  gAC<-rep(NA, nrow(temp))
  gAC[temp$hi_prodose==1] <- gAC.pred[temp$hi_prodose==1]
  gAC[temp$hi_prodose==0] <- gAC.pred[temp$hi_prodose==0]
  wt<-1/gAC
  temp$wt<-wt
  Y_hi<-weighted.mean(temp[temp$hi_prodose==1,]$y,temp[temp$hi_prodose==1,]$wt)
  Y_low<-weighted.mean(temp[temp$hi_prodose==0,]$y,temp[temp$hi_prodose==0,]$wt)
  Ys_hi<-Y_hi/mean(delirium[delirium$hi_prodose==1,]$wt)
  Ys_low<-Y_low/mean(delirium[delirium$hi_prodose==0,]$wt)
  return(Ys_hi-Ys_low)
}
set.seed(1)
bootstrap_2 <- boot(delirium[, -21],function_2,R=1000)
boot.ci(boot.out=bootstrap_2)
# ( 0.0313,  0.1513 )  

###########################
#iptw and MSM estimand
model1<-glm(y~hi_prodose, data = delirium, family = "binomial", weights = wt)
summary(model1)
#0.05168
exp(model1$coefficients[[2]])
#OR: 1.053043

#bootstrap variance: 
function_3 <- function(data,i){
  temp<-data[i,]
  gAC.reg<-multinom(hi_prodose~ Gender + old + HOCNS + Delirium + cursmoke + alcohol + SURGSITE +
                      Charlest + Dementia + Depress + StrokeOrTIA, data=temp)
  gAC.pred<- predict(gAC.reg, type="probs")
  gAC<-rep(NA, nrow(temp))
  gAC[temp$hi_prodose==1] <- gAC.pred[temp$hi_prodose==1]
  gAC[temp$hi_prodose==0] <- gAC.pred[temp$hi_prodose==0]
  wt<-1/gAC
  temp$wt<-wt
  tempmodel<-glm(y~hi_prodose, data = temp, family = "binomial", weights = wt)
  return(tempmodel$coefficients[[2]])
}
set.seed(1)
bootstrap_3 <- boot(delirium[, -21],function_3,R=1000)
boot.ci(boot.out=bootstrap_2)
#( 0.0313,  0.1513 )
#ORCI: (1.032, 1.163)

#Table 1
library(table1)
table1(~factor(Gender) + Age + factor(HOCNS) + factor(Delirium)
       + factor(cursmoke) + factor(ETOH) + SURGSITE +
         Charlest + factor(Dementia) + factor(Depress)
       + factor(StrokeOrTIA)|hi_prodose, data=delirium)
