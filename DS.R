library(dplyr)
#GoCog<-read.csv("../GoCogdata/GoCog.csv", h=T)
GoCog<-read.csv("../GoCogdata/GoCog_20180404.csv", h=T)[,1:7]
GoCog$Subj<-as.factor(GoCog$Subj)

GoCog$GoStage <- factor(GoCog$GoStage, levels = c("Open","Mid","End"))
GoCog$CogTask<- factor(GoCog$CogTask, levels = c("None","Spat","Reas","Calc"))
GoCog$Both_RT <- GoCog$Both_RT/1000
GoCog$Both_Acc <- GoCog$Both_Acc*100

head(GoCog)

mean(GoCog$Both_Acc)
sd(GoCog$Both_Acc)
mean(GoCog$Both_RT,na.rm = T)
sd(GoCog$Both_RT,na.rm = T)

aggregate(Both_Acc ~ GoStage, data = GoCog, mean)
aggregate(Both_Acc ~ GoStage, data = GoCog, sd)
aggregate(Both_RT ~ GoStage, data = GoCog, mean)
aggregate(Both_RT ~ GoStage, data = GoCog, sd)

aggregate(Both_Acc ~ CogTask, data = GoCog, mean)
aggregate(Both_Acc ~ CogTask, data = GoCog, sd)
aggregate(Both_RT ~ CogTask, data = GoCog, mean)
aggregate(Both_RT ~ CogTask, data = GoCog, sd)


# Table 1

tab1 <- aggregate(Both_Acc ~ CogTask+GoStage, data = GoCog, mean)
tab1 <- tab1[,c(2,1,3)]
tab1[,3] <- round(tab1[,3],2)
tab1_sd <- aggregate(Both_Acc ~ CogTask+GoStage, data = GoCog, sd)
tab1_sd <- tab1_sd[,c(2,1,3)]
tab1_sd[,3] <- round(tab1_sd[,3],2)
colnames(tab1_sd)[3] <- "Both_Acc_SD"

BothAcc <- full_join(tab1,tab1_sd)

tab1 <- aggregate(Both_RT ~ CogTask+GoStage, data = GoCog, mean)
tab1 <- tab1[,c(2,1,3)]
tab1[,3] <- round(tab1[,3],2)
tab1_sd <- aggregate(Both_RT ~ CogTask+GoStage, data = GoCog, sd)
tab1_sd <- tab1_sd[,c(2,1,3)]
tab1_sd[,3] <- round(tab1_sd[,3],2)
colnames(tab1_sd)[3] <- "Both_RT_SD"

BothRT <- full_join(tab1,tab1_sd)

Table1 <- full_join(BothAcc,BothRT)
Table1


