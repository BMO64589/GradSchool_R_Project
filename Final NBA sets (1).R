NBA <- read.csv("C:/Users/brand/Desktop/R Homework/NBA.csv", header=T)
attach(NBA)
head(NBA)
library(dplyr)
install.packages('ggplot2')
install.packages('plyr')
library(ggplot2)
library(mlr)
library(ParamHelpers)
library(tidyverse)

mydata <- select(NBA, Season, Game, Date, Team, WinLoss, TeamPoints, Steals, Assists, FieldGoalsAttempted, FreeThrowsAttempted, X3PointShotsAttempted, Blocks, TotalFouls, Turnovers, OffRebounds)
mydata$StealsB <- ifelse(mydata$Steals >= median(mydata$Steals), 1, 0)
mydata <- filter(mydata, Season != '2017-2018')
mydata <- filter(mydata, Season != '2014-2015')
mydata <- createDummyFeatures(mydata, cols = "WinLoss")


Logistic.model <- glm(StealsB ~  TeamPoints + WinLoss.W + Game + Assists + FieldGoalsAttempted + X3PointShotsAttempted + FreeThrowsAttempted + Blocks + TotalFouls + Turnovers + OffRebounds,family = 'binomial', data=mydata)
Logistic.model2 <- glm(StealsB ~  TeamPoints + WinLoss.W + Assists + FieldGoalsAttempted + X3PointShotsAttempted + FreeThrowsAttempted + Blocks + TotalFouls + Turnovers + OffRebounds, family = 'binomial', data=mydata)
Logistic.model3 <- glm(StealsB ~  TeamPoints + WinLoss.W + Assists + FieldGoalsAttempted + X3PointShotsAttempted + Blocks + TotalFouls + Turnovers + OffRebounds, family = 'binomial', data=mydata)
Logistic.model4 <- glm(StealsB ~  WinLoss.W + Assists + FieldGoalsAttempted + X3PointShotsAttempted + Blocks + TotalFouls + Turnovers + OffRebounds, family = 'binomial', data=mydata)
Logistic.model5 <- glm(StealsB ~  WinLoss.W + FieldGoalsAttempted + X3PointShotsAttempted + Blocks + TotalFouls + Turnovers + OffRebounds, family = 'binomial', data=mydata)
Logistic.model6 <- glm(StealsB ~  WinLoss.W + FieldGoalsAttempted + Blocks + TotalFouls + Turnovers + OffRebounds, family = 'binomial', data=mydata)
Logistic.model7 <- glm(StealsB ~  WinLoss.W + FieldGoalsAttempted + Blocks + Turnovers + OffRebounds, family = 'binomial', data=mydata)
Logistic.model8 <- glm(StealsB ~  WinLoss.W + FieldGoalsAttempted + Turnovers + OffRebounds, family = 'binomial', data=mydata)
summary(Logistic.model1)

exp(coef(Logistic.model8))
Plot(Logistic.model8)
predict <- predict(Logistic.model8, type='response')
predict

table(mydata$StealsB>0, predict > .4819)


library(ROCR) 
ROCRpred <- prediction(predict, mydata$StealsB>0) 
ROCRperf <- performance(ROCRpred, 'tpr','fpr') 
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0.1,by=0.1),text.adj = c(-0.2,1.7))

mydata1 <- select(NBA, Season, Game, Date, Team, WinLoss, TeamPoints, Steals, Assists, FreeThrows, FieldGoals, X3PointShots, Blocks, TotalFouls, Turnovers, OffRebounds)
mydata1$StealsB <- ifelse(mydata$Steals >= median(mydata$Steals), 1, 0)
mydata1 <- filter(mydata, Season != '2017-2018')
mydata1 <- filter(mydata, Season != '2014-2015')
mydata1 <- createDummyFeatures(mydata, cols = "WinLoss")
attach(mydata1)

SLogistic.model <- glm(StealsB ~  TeamPoints + WinLoss.W + Game + Assists + FieldGoals + X3PointShots + FreeThrows + Blocks + TotalFouls + Turnovers + OffRebounds,family = 'binomial', data=mydata)
SLogistic.model2 <- glm(StealsB ~  TeamPoints + WinLoss.W + Game + Assists + X3PointShots + FreeThrows + Blocks + TotalFouls + Turnovers + OffRebounds,family = 'binomial', data=mydata)
SLogistic.model3 <- glm(StealsB ~  TeamPoints + WinLoss.W + Game + Assists + X3PointShots + Blocks + TotalFouls + Turnovers + OffRebounds,family = 'binomial', data=mydata)
SLogistic.model4 <- glm(StealsB ~  TeamPoints + WinLoss.W + Assists + X3PointShots + Blocks + TotalFouls + Turnovers + OffRebounds,family = 'binomial', data=mydata)
SLogistic.model5 <- glm(StealsB ~  WinLoss.W + Assists + X3PointShots + Blocks + TotalFouls + Turnovers + OffRebounds,family = 'binomial', data=mydata)
SLogistic.model6 <- glm(StealsB ~  WinLoss.W + Assists + X3PointShots + Blocks + Turnovers + OffRebounds,family = 'binomial', data=mydata)
SLogistic.model7 <- glm(StealsB ~  WinLoss.W + Assists + X3PointShots + Turnovers + OffRebounds,family = 'binomial', data=mydata)
SLogistic.model8 <- glm(StealsB ~  WinLoss.W + Assists + X3PointShots + Turnovers,family = 'binomial', data=mydata)
summary(SLogistic.model8)
exp(coef(SLogistic.model8))
Spredict <- predict(SLogistic.model8, type='response')
Spredict

table(mydata$StealsB>0, predict > .52)

library(ROCR) 
SROCRpred <- prediction(predict, mydata$StealsB>0) 
SROCRperf <- performance(SROCRpred, 'tpr','fpr') 
plot(SROCRperf, colorize = TRUE, print.cutoffs.at=seq(0.1,by=0.1),text.adj = c(-0.2,1.7))

GSW <- select(mydata, Team, Game, Steals, Season, WinLoss)
GSW <- filter(mydata, Season != '2017-2018')
GSW <- filter(mydata, Season != '2014-2015')
GSW <- filter(mydata, Team == 'GSW')



PlGSW <- function(GSW){
  p <- plot(GSW$Game[WinLoss=="W"], GSW$Steals[WinLoss=="W"], xlab = '', ylab = '', col = 'Yellow', pch=20)
  points(GSW$Game[WinLoss=="L"], GSW$Steals[WinLoss=='L'], col='Blue', pch=20)
  abline(h=mean(GSW$Steals), col=4, lwd=2)
  abline(h = 7.75, col=2, lwd=2)
  text(x=27, y=17, label="Avg # of Steals a game in NBA = 7.75", cex = .8, col =2, font = 4)
  text(x=27, y=18, label="Avg # of Steals a game for GSW = 9.57", cex = .8, col =4, font = 4)
  title(main = "Golden State Steals per game", xlab = 'Games', ylab = 'Steals')
  legend(x=60, y=18.5, legend = c('Win', 'Loss'), fill = c('Yellow', 'Blue'))
}

PlGSW(GSW)

BOS <- select(mydata, Team, Game, Steals, Season, WinLoss)
BOS <- filter(mydata, Season != '2017-2018')
BOS <- filter(mydata, Season != '2014-2015')
BOS <- filter(mydata, Team == 'BOS')
mean(BOS$Steals)

PlBOS <- function(BOS){
  p <- plot(BOS$Game[WinLoss=="W"], BOS$Steals[WinLoss=="W"], xlab = '', ylab = '', col = 'Green', pch=20)
  points(BOS$Game[WinLoss=="L"], BOS$Steals[WinLoss=='L'], col='BLack', pch=20)
  abline(h=mean(BOS$Steals), col=4, lwd=2)
  abline(h = 7.75, col=2, lwd=2)
  text(x=30, y=18.2, label="Avg # of Steals a game in NBA = 7.75", cex = .6, col =2, font = 4)
  text(x=20, y=17.6, label="Avg # of Steals a game for BOS = 8.34", cex = .6, col =4, font = 4)
  title(main = "Boston Steals per game", xlab = 'Games', ylab = 'Steals')
  legend(x=60, y=18.5, legend = c('Win', 'Loss'), fill = c('Green', 'Black'))
}

PlBOS(BOS)

SAC <- select(mydata, Team, Game, Steals, Season, WinLoss)
SAC <- filter(mydata, Season != '2017-2018')
SAC <- filter(mydata, Season != '2014-2015')
SAC <- filter(mydata, Team == 'SAC')
mean(SAC$Steals)

PlSAC <- function(SAC){
  p <- plot(SAC$Game[WinLoss=="W"], SAC$Steals[WinLoss=="W"], xlab = '', ylab = '', col = 'azure4', pch=20)
  points(SAC$Game[WinLoss=="L"], SAC$Steals[WinLoss=='L'], col='Purple', pch=20)
  abline(h=mean(SAC$Steals), col=4, lwd=2)
  abline(h = 7.75, col=2, lwd=2)
  text(x=30, y=17.2, label="Avg # of Steals a game in NBA = 7.75", cex = .6, col =2, font = 4)
  text(x=20, y=16.6, label="Avg # of Steals a game for SAC = 8.29", cex = .6, col =4, font = 4)
  title(main = "Sacarmento Steals per game", xlab = 'Games', ylab = 'Steals')
  legend(x=60, y=16.5, legend = c('Win', 'Loss'), fill = c('azure4', 'Purple'))
}

PlSAC(SAC)

NYK <- select(mydata, Team, Game, Steals, Season, WinLoss)
NYK <- filter(mydata, Season != '2017-2018')
NYK <- filter(mydata, Season != '2014-2015')
NYK <- filter(mydata, Team == 'NYK')
mean(NYK$Steals)

PlNYK <- function(NYK){
  p <- plot(NYK$Game[WinLoss=="W"], NYK$Steals[WinLoss=="W"], xlab = '', ylab = '', col = 'Blue', pch=20)
  points(NYK$Game[WinLoss=="L"], NYK$Steals[WinLoss=='L'], col='Orange', pch=20)
  abline(h=mean(NYK$Steals), col='Green', lwd=2)
  abline(h = 7.75, col=2, lwd=2)
  text(x=30, y=15, label="Avg # of Steals a game in NBA = 7.75", cex = .7, col =2, font = 4)
  text(x=30, y=14, label="Avg # of Steals a game for NYK = 6.39", cex = .7, col ='Green', font = 4)
  title(main = "New York per game", xlab = 'Games', ylab = 'Steals')
  legend(x=60, y=15.5, legend = c('Win', 'Loss'), fill = c('Blue', 'Orange'))
}

PlNYK(NYK)

MIL <- select(mydata, Team, Game, Steals, Season, WinLoss)
MIL <- filter(mydata, Season != '2017-2018')
MIL <- filter(mydata, Season != '2014-2015')
MIL <- filter(mydata, Team == 'MIL')
MIL1 <- MIL %<%
  select(MIL$Steals, MIL$WinLoss.W) %>%
 MIL1 <- filter(MIL1, Steals > 7.75 | MIL1, WinLoss.W == "1")
mean(MIL$Steals)

PlMIL <- function(MIL){
  p <- plot(MIL$Game[WinLoss=="W"], MIL$Steals[WinLoss=="W"], xlab = '', ylab = '', col = 'green4', pch=20)
  points(MIL$Game[WinLoss=="L"], MIL$Steals[WinLoss=='L'], col='Black', pch=20)
  abline(h=mean(MIL$Steals), col= 4, lwd=2)
  abline(h = 7.75, col=2, lwd=2)
  text(x=25, y=16.5, label="Avg # of Steals a game in NBA = 7.75", cex = .7, col =2, font = 4)
  text(x=27, y=2, label="Avg # of Steals a game for MIL = 8.15", cex = .7, col = 4, font = 4)
  title(main = "Milwaukee Steals per game", xlab = 'Games', ylab = 'Steals')
  legend(x=60, y=15.5, legend = c('Win', 'Loss'), fill = c('green4', 'Black'))
}

PlMIL(MIL)

