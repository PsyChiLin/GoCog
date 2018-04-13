library(afex)
library(dplyr)
library(reshape2)
## Read Data
#GoCog<-read.csv("../GoCogdata/GoCog.csv", h=T)
GoCog<-read.csv("../GoCogdata/GoCog_20180404.csv", h=T)[,1:7]
GoCog$Subj<-as.factor(GoCog$Subj)
colnames(GoCog)[6] <- "Both_ACC"
GoCog$Both_RT <- GoCog$Both_RT/1000
GoCog$Both_ACC <- GoCog$Both_ACC*100
GoCog$GoStage <- factor(GoCog$GoStage, levels=c("Open", "Spat", "End"))
GoCog$CogTask<- factor(GoCog$CogTask, levels=c("None", "Spat", "Reas", "Calc"))
head(GoCog)
str(GoCog)
########################### Two-Way:ACC ###########################
aov2way <- aov_4(Both_ACC~(CogTask*GoStage|Subj),data = GoCog,
                 anova_table=list(correction = "none"))
aov2way
summary(aov2way)
# model01 <- aov(Both_ACC ~ Subj + GoStage * GoStage + Error(Subj), data=GoCog)
# summary(model01)
# capture.output(summary(model01), file = "Output/aov2_Overall_BothACC.txt")


#### Simple main effect : given Cog None
dta_None <- filter(GoCog, CogTask == "None")
aov2way_s_None <- aov_4(Both_ACC ~ Subj + (GoStage|Subj),
                        data = dta_None,anova_table=list(correction = "none"))
aov2way_s_None
summary(aov2way_s_None)
model01_s_None_F <- 1430/3/225.42 #aov2way$anova_table : exact 0.034915
model01_s_None_F
pf(model01_s_None_F,2,46,lower.tail = FALSE)

#### Simple main effect : given Cog Spat
dta_Spat <- filter(GoCog, CogTask == "Spat")
aov2way_s_Spat <- aov_4(Both_ACC ~ Subj + (GoStage|Subj),data = dta_Spat,anova_table=list(correction = "none"))
aov2way_s_Spat 
summary(aov2way_s_Spat)
model01_s_Spat_F <-  9067/3/225.42
model01_s_Spat_F 
pf(model01_s_Spat_F,2,46,lower.tail = F)
#### t.test
dta_Spat_agg <- aggregate(dta_Spat$Both_ACC~dta_Spat$GoStage,FUN = mean)
dta_Spat_ttest <- matrix(NA,4,4)
dta_Spat_ttest_p <- matrix(NA,4,4)
for (i in 1:4){
  for (j in (i+1):4){
    dta_Spat_ttest[i,j] <- (dta_Spat_agg[i,2] - dta_Spat_agg[j,2])/sqrt(225.42*2/24)
    dta_Spat_ttest_p[i,j] <- 2*pt(abs(dta_Spat_ttest[i,j]),46,lower.tail = F)
  }
}
colnames(dta_Spat_ttest) <- c("None", "Spat", "Reas", "Calc")
row.names(dta_Spat_ttest) <- c("None", "Spat", "Reas", "Calc")
dta_Spat_ttest
colnames(dta_Spat_ttest_p) <- c("None", "Spat", "Reas", "Calc")
row.names(dta_Spat_ttest_p) <- c("None", "Spat", "Reas", "Calc")
round(dta_Spat_ttest_p,5)
round(dta_Spat_ttest_p,5) < 0.05/9

#### Simple main effect : given GoStage End
dta_End <- filter(GoCog, GoStage == "End")
aov2way_s_End <- aov_4(Both_ACC ~ Subj + (GoStage|Subj),data = dta_End,anova_table=list(correction = "none"))
#model01_s_End <- aov(Both_ACC ~ Subj + GoStage + Error(Subj),data = dta_End)
#summary(model01_s_End)
aov2way_s_End
summary(aov2way_s_End)
model01_s_End_F <- 11454/3/225.42
model01_s_End_F 
df(model01_s_End_F,3,46)
#### t.test
dta_End_agg <- aggregate(dta_End$Both_ACC~dta_End$GoStage,FUN = mean)
dta_End_ttest <- matrix(NA,4,4)
dta_End_ttest_p <- matrix(NA,4,4)
for (i in 1:4){
  for (j in (i+1):4){
    dta_End_ttest[i,j] <- (dta_End_agg[i,2] - dta_End_agg[j,2])/sqrt(225.42*2/24)
    dta_End_ttest_p[i,j] <- 2*pt(abs(dta_End_ttest[i,j]),46,lower.tail = F)
  }
}
colnames(dta_End_ttest) <- c("None", "Spat", "Reas", "Calc")
row.names(dta_End_ttest) <- c("None", "Spat", "Reas", "Calc")
dta_End_ttest
colnames(dta_End_ttest_p) <- c("None", "Spat", "Reas", "Calc")
row.names(dta_End_ttest_p) <- c("None", "Spat", "Reas", "Calc")
round(dta_End_ttest_p,5)
round(dta_End_ttest_p,5) < 0.05/9