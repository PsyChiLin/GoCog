library(dplyr)
GoCog<-read.csv("../GoCogdata/GoCog.csv", h=T)
GoCog$Subj<-as.factor(GoCog$Subj)
head(GoCog)
GoCog$GoStage <- factor(GoCog$GoStage, levels = c("Open","Mid","End"))
GoCog$CogTask<- factor(GoCog$CogTask, levels = c("None","Spat","Reas","Calc"))
GoCog$Both_RT <- GoCog$Both_RT /1000
GoCog$Both_ACC <- GoCog$Both_ACC*100

mean(GoCog$Both_ACC)
mean(GoCog$Both_RT)


tab1 <- aggregate(Both_ACC ~ CogTask+GoStage, data = GoCog, mean)
tab1 <- tab1[,c(2,1,3)]
tab1[,3] <- round(tab1[,3],2)
tab1_sd <- aggregate(Both_ACC ~ CogTask+GoStage, data = GoCog, sd)
tab1_sd <- tab1_sd[,c(2,1,3)]
tab1_sd[,3] <- round(tab1_sd[,3],2)
colnames(tab1_sd)[3] <- "Both_ACC_SD"

BothACC <- full_join(tab1,tab1_sd)

tab1 <- aggregate(Both_RT ~ CogTask+GoStage, data = GoCog, mean)
tab1 <- tab1[,c(2,1,3)]
tab1[,3] <- round(tab1[,3],2)
tab1_sd <- aggregate(Both_RT ~ CogTask+GoStage, data = GoCog, sd)
tab1_sd <- tab1_sd[,c(2,1,3)]
tab1_sd[,3] <- round(tab1_sd[,3],2)
colnames(tab1_sd)[3] <- "Both_RT_SD"

BothRT <- full_join(tab1,tab1_sd)

Table1 <- full_join(BothACC,BothRT)
Table1

aggregate(Both_ACC ~ GoStage, data = GoCog, mean)
aggregate(Both_ACC ~ GoStage, data = GoCog, sd)
aggregate(Both_RT ~ GoStage, data = GoCog, mean)
aggregate(Both_RT ~ GoStage, data = GoCog, sd)

aggregate(Both_ACC ~ CogTask, data = GoCog, mean)
aggregate(Both_ACC ~ CogTask, data = GoCog, sd)
aggregate(Both_RT ~ CogTask, data = GoCog, mean)
aggregate(Both_RT ~ CogTask, data = GoCog, sd)
