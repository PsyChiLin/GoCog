library(afex)
library(dplyr)
library(reshape2)
## Read Data
#GoCog<-read.csv("../GoCogdata/GoCog.csv", h=T)
GoCog<-read.csv("../GoCogdata/GoCog_20180528_test.csv", h=T)
GoCog$Subj<-as.factor(GoCog$Subj)
colnames(GoCog)[6] <- "Both_ACC"
head(GoCog)
str(GoCog)
GoCog$Both_RT <- GoCog$Both_RT/1000
GoCog$Both_ACC <- GoCog$Both_ACC*100
GoCog$GoStage <- factor(GoCog$GoStage, levels=c("Open", "Mid", "End"))
GoCog$CogTask<- factor(GoCog$CogTask, levels=c("None", "Spat", "Reas", "Calc"))
GoCog <- filter(GoCog, CogTask %in% c("Spat", "Reas", "Calc"))
Cog <- aggregate(Cog_ACC~Subj+CogTask, data = GoCog, FUN = mean)

aov <- aov_4(Cog_ACC~CogTask|Subj,data = Cog,anova_table=list(correction = "none"))
aov
summary(aov)
