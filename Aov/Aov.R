## Read Data
GoCog<-read.csv("../GoCogdata/GoCog.csv", h=T)
GoCog$Subj<-as.factor(GoCog$Subj)
head(GoCog)

## Two-Way
model01 <- aov(Both_ACC ~ (GoStage*CogTask) + Error(Subj/(GoStage*CogTask)), data=GoCog)
bACC0 <- summary(model01)
capture.output(bACC0, file = "Output/aov2_Overall_BothACC.txt")

model02 <- aov(Both_RT ~ (GoStage*CogTask) + Error(Subj/(GoStage*CogTask)), data=GoCog)
bRT0 <- summary(model02)
capture.output(bRT0, file = "Output/aov2_Overall_BothRT.txt")

## three-way ANOVA_Both
model1 <- aov(Both_ACC ~ (SubjGroup*GoStage*CogTask) + Error(Subj/(GoStage*CogTask)), data=GoCog)
bACC <- summary(model1)
capture.output(bACC, file = "Output/aov3_BothACC.txt")

model2 <- aov(Both_RT ~ (SubjGroup*GoStage*CogTask) + Error(Subj/(GoStage*CogTask)), data=GoCog)
bRT <-summary(model2)
capture.output(bRT, file = "Output/aov3_BothRT.txt")

# ## three-way ANOVA_Go
# model1 <- aov(Go_ACC ~ (SubjGroup*GoStage*CogTask) + Error(Subj/(GoStage*CogTask)), data=GoCog)
# a<-summary(model1)
# capture.output(a, file = "aov_GoACC.txt")
# 
# model1 <- aov(Go_RT ~ (SubjGroup*GoStage*CogTask) + Error(Subj/(GoStage*CogTask)), data=GoCog)
# a<-summary(model1)
# capture.output(a, file = "aov_GoRT.txt")

###Tukey
t<- aov(Both_ACC ~ (SubjGroup*GoStage*CogTask) + Error(Subj/(GoStage*CogTask)), data=GoCog)
TukeyHSD(t$`Subj:GoStage:CogTask`, ordered = TRUE, conf.level = 0.95)
?TukeyHSD








# ##data
# state<-read.csv("NoneRST_All.csv", h=T)
# state1<-state[which(state$SubjGroup=="?~?l?q??(Dan)"),]
# state2<-state[which(state$SubjGroup=="?~?l?Ŧ?(Kyu)"),]
# state$SubjRank <- factor(state$SubjRank, levels = c("?~?l????", "?~?l?A??", "?~?l?Ҳ?", 
#                                                     "?~?l?G?q", "?~?l?T?q", "?~?l?|?q", 
#                                                     "?~?l???q", "?~?l???q"))
# 
# 
# ###Rank_Age_RST
# a1<-summary(lm(Go_RT~Age, data=state))
# a2<-summary(lm(Go_RT~RST1.z++RST2.z+RST3.z+RST4.z+RST5.z+RST6.z, data=state))
# a3<-summary(lm(Go_RT~SubjRank, data=state))
# a4<-summary(lm(Go_RT~SubjRank+Age, data=state))
# a5<-summary(lm(Go_RT~SubjRank+RST1.z++RST2.z+RST3.z+RST4.z+RST5.z+RST6.z, data=state))
# a6<-summary(lm(Go_RT~Age+RST1.z++RST2.z+RST3.z+RST4.z+RST5.z+RST6.z, data=state))
# a7<-summary(lm(Go_RT~SubjRank+Age+RST1.z++RST2.z+RST3.z+RST4.z+RST5.z+RST6.z, data=state))
# a8<-summary(lm(Go_RT~Age+Verbal.z+Nonverbal.z+Total.z, data=state))
# a9<-summary(lm(Go_RT~SubjRank+Verbal.z+Nonverbal.z, data=state))
# a10<-summary(lm(Go_RT~Age+SubjRank+Verbal.z+Nonverbal.z, data=state))
# a11<-summary(lm(Go_RT~Verbal.z+Nonverbal.z, data=state))
# a12<-summary(lm(Go_RT~Age+SubjGroup+RST1.z++RST2.z+RST3.z+RST4.z+RST5.z+RST6.z, data=state))
# 
# 
# capture.output(a1, file = "lm_GoRT_Age.txt")
# capture.output(a2, file = "lm_GoRT_RSTsub.txt")
# capture.output(a3, file = "lm_GoRT_SubjRank.txt")
# capture.output(a4, file = "lm_GoRT_Age_Rank.txt")
# capture.output(a5, file = "lm_GoRT_Rank_RSTsub.txt")
# capture.output(a6, file = "lm_GoRT_Age_RSTsub.txt")
# capture.output(a7, file = "lm_GoRT_Age_Rank_RSTsub.txt")
# capture.output(a8, file = "lm_GoRT_Age_RSTall.txt")
# capture.output(a9, file = "lm_GoRT_Rank_RSTall.txt")
# capture.output(a10, file = "lm_GoRT_Age_Rank_RSTall.txt")
# capture.output(a11, file = "lm_GoRT_RSTall.txt")
# 
# 
# 
# ### dan
# a1<-summary(lm(Go_RT~Age, data=state1))
# a2<-summary(lm(Go_RT~RST1.z++RST2.z+RST3.z+RST4.z+RST5.z+RST6.z, data=state1))
# a3<-summary(lm(Go_RT~SubjRank, data=state1))
# a4<-summary(lm(Go_RT~SubjRank+Age, data=state1))
# a5<-summary(lm(Go_RT~SubjRank+RST1.z++RST2.z+RST3.z+RST4.z+RST5.z+RST6.z, data=state1))
# a6<-summary(lm(Go_RT~Age+RST1.z++RST2.z+RST3.z+RST4.z+RST5.z+RST6.z, data=state1))
# a7<-summary(lm(Go_RT~SubjRank+Age+RST1.z++RST2.z+RST3.z+RST4.z+RST5.z+RST6.z, data=state1))
# a8<-summary(lm(Go_RT~Age+Verbal.z+Nonverbal.z+Total.z, data=state1))
# a9<-summary(lm(Go_RT~SubjRank+Verbal.z+Nonverbal.z, data=state1))
# a10<-summary(lm(Go_RT~Age+SubjRank+Verbal.z+Nonverbal.z, data=state1))
# a11<-summary(lm(Go_RT~Verbal.z+Nonverbal.z, data=state1))
# 
# 
# ### GoACC(dan)
# a1<-summary(lm(Go_ACC~Age, data=state1))
# a2<-summary(lm(Go_RT~RST1.z++RST2.z+RST3.z+RST4.z+RST5.z+RST6.z, data=state1))
# a3<-summary(lm(Go_RT~SubjRank, data=state1))
# a4<-summary(lm(Go_RT~SubjRank+Age, data=state1))
# a5<-summary(lm(Go_RT~SubjRank+RST1.z++RST2.z+RST3.z+RST4.z+RST5.z+RST6.z, data=state1))
# a6<-summary(lm(Go_RT~Age+RST1.z++RST2.z+RST3.z+RST4.z+RST5.z+RST6.z, data=state1))
# a7<-summary(lm(Go_ACC~SubjRank+Age+RST1.z++RST2.z+RST3.z+RST4.z+RST5.z+RST6.z, data=state1))
# a8<-summary(lm(Go_RT~Age+Verbal.z+Nonverbal.z+Total.z, data=state1))
# a9<-summary(lm(Go_RT~SubjRank+Verbal.z+Nonverbal.z, data=state1))
# a10<-summary(lm(Go_RT~Age+SubjRank+Verbal.z+Nonverbal.z, data=state1))
# a11<-summary(lm(Go_RT~Verbal.z+Nonverbal.z, data=state1))
# summary(lm(Go_ACC~RST1.z, data=state1))
# summary(lm(None_ACC~Age+Total.z, data=state1))
# ## lm_Dan
# s1<-summary(lm(Both_ACC~Total, data=state1))
# s2<-summary(lm(Both_ACC~Nonverbal, data=state1))
# s3<-summary(lm(Both_ACC~Verbal, data=state1))
# s4<-summary(lm(Both_RT~Total, data=state1))
# s5<-summary(lm(Both_RT~Nonverbal, data=state1))
# s6<-summary(lm(Both_RT~Verbal, data=state1))
# 
# s7<-summary(lm(Go_ACC~Total, data=state1))
# s8<-summary(lm(Go_ACC~Nonverbal, data=state1))
# s9<-summary(lm(Go_ACC~Verbal, data=state1))
# s10<-summary(lm(Go_RT~Total, data=state1))
# s11<-summary(lm(Go_RT~Nonverbal, data=state1))
# s12<-summary(lm(Go_RT~Verbal, data=state1))
# 
# ks13<-summary(lm(None_ACC~Total, data=state1))
# s14<-summary(lm(None_ACC~Nonverbal, data=state1))
# s15<-summary(lm(None_ACC~Verbal, data=state1))
# s16<-summary(lm(None_RT~Total, data=state1))
# s17<-summary(lm(None_RT~Nonverbal, data=state1))
# s18<-summary(lm(None_RT~Verbal, data=state1))
# 
# capture.output(s1, file = "lm_BothACC_Total(Dan).txt")
# capture.output(s2, file = "lm_BothACC_Nonverbal(Dan).txt")
# capture.output(s3, file = "lm_BothACC_Verbal(Dan).txt")
# capture.output(s4, file = "lm_BothRT_Total(Dan).txt")
# capture.output(s5, file = "lm_BothRT_Nonverbal(Dan).txt")
# capture.output(s6, file = "lm_BothRT_Verbal(Dan).txt")
# 
# capture.output(s7, file = "lm_GoACC_Total(Dan).txt")
# capture.output(s8, file = "lm_GoACC_Nonverbal(Dan).txt")
# capture.output(s9, file = "lm_GoACC_Verbal(Dan).txt")
# capture.output(s10, file = "lm_GoRT_Total(Dan).txt")
# capture.output(s11, file = "lm_GoRT_Nonverbal(Dan).txt")
# capture.output(s12, file = "lm_GoRT_Verbal(Dan).txt")
# 
# capture.output(s13, file = "lm_NoneACC_Total(Dan).txt")
# capture.output(s14, file = "lm_NoneACC_Nonverbal(Dan).txt")
# capture.output(s15, file = "lm_NoneACC_Verbal(Dan).txt")
# capture.output(s16, file = "lm_NoneRT_Total(Dan).txt")
# capture.output(s17, file = "lm_NoneRT_Nonverbal(Dan).txt")
# capture.output(s18, file = "lm_NoneRT_Verbal(Dan).txt")
# 
# 
# ##lm_Kyu
# s1<-summary(lm(Both_ACC~Total, data=state2))
# s2<-summary(lm(Both_ACC~Nonverbal, data=state2))
# s3<-summary(lm(Both_ACC~Verbal, data=state2))
# s4<-summary(lm(Both_RT~Total, data=state2))
# s5<-summary(lm(Both_RT~Nonverbal, data=state2))
# s6<-summary(lm(Both_RT~Verbal, data=state2))
# 
# s7<-summary(lm(Go_ACC~Total, data=state2))
# s8<-summary(lm(Go_ACC~Nonverbal, data=state2))
# s9<-summary(lm(Go_ACC~Verbal, data=state2))
# s10<-summary(lm(Go_RT~Total, data=state2))
# s11<-summary(lm(Go_RT~Nonverbal, data=state2))
# s12<-summary(lm(Go_RT~Verbal, data=state2))
# 
# s13<-summary(lm(None_ACC~Total, data=state2))
# s14<-summary(lm(None_ACC~Nonverbal, data=state2))
# s15<-summary(lm(None_ACC~Verbal, data=state2))
# s16<-summary(lm(None_RT~Total, data=state2))
# s17<-summary(lm(None_RT~Nonverbal, data=state2))
# s18<-summary(lm(None_RT~Verbal, data=state2))
# 
# capture.output(s1, file = "lm_BothACC_Total(Kyu).txt")
# capture.output(s2, file = "lm_BothACC_Nonverbal(Kyu).txt")
# capture.output(s3, file = "lm_BothACC_Verbal(Kyu).txt")
# capture.output(s4, file = "lm_BothRT_Total(Kyu).txt")
# capture.output(s5, file = "lm_BothRT_Nonverbal(Kyu).txt")
# capture.output(s6, file = "lm_BothRT_Verbal(Kyu).txt")
# 
# capture.output(s7, file = "lm_GoACC_Total(Kyu).txt")
# capture.output(s8, file = "lm_GoACC_Nonverbal(Kyu).txt")
# capture.output(s9, file = "lm_GoACC_Verbal(Kyu).txt")
# capture.output(s10, file = "lm_GoRT_Total(Kyu).txt")
# capture.output(s11, file = "lm_GoRT_Nonverbal(Kyu).txt")
# capture.output(s12, file = "lm_GoRT_Verbal(Kyu).txt")
# 
# capture.output(s13, file = "lm_NoneACC_Total(Kyu).txt")
# capture.output(s14, file = "lm_NoneACC_Nonverbal(Kyu).txt")
# capture.output(s15, file = "lm_NoneACC_Verbal(Kyu).txt")
# capture.output(s16, file = "lm_NoneRT_Total(Kyu).txt")
# capture.output(s17, file = "lm_NoneRT_Nonverbal(Kyu).txt")
# capture.output(s18, file = "lm_NoneRT_Verbal(Kyu).txt")
# 
# ## lm_All
# s1<-summary(lm(Both_ACC~Total, data=state))
# s2<-summary(lm(Both_ACC~Nonverbal, data=state))
# s3<-summary(lm(Both_ACC~Verbal, data=state))
# s4<-summary(lm(Both_RT~Total, data=state))
# s5<-summary(lm(Both_RT~Nonverbal, data=state))
# s6<-summary(lm(Both_RT~Verbal, data=state))
# 
# s7<-summary(lm(Go_ACC~Total, data=state))
# s8<-summary(lm(Go_ACC~Nonverbal, data=state))
# s9<-summary(lm(Go_ACC~Verbal, data=state))
# s10<-summary(lm(Go_RT~Total, data=state))
# s11<-summary(lm(Go_RT~Nonverbal, data=state))
# s12<-summary(lm(Go_RT~Verbal, data=state))
# 
# s13<-summary(lm(None_ACC~Total, data=state))
# s14<-summary(lm(None_ACC~Nonverbal, data=state))
# s15<-summary(lm(None_ACC~Verbal, data=state))
# s16<-summary(lm(None_RT~Total, data=state))
# s17<-summary(lm(None_RT~Nonverbal, data=state))
# s18<-summary(lm(None_RT~Verbal, data=state))
# 
# capture.output(s1, file = "lm_BothACC_Total.txt")
# capture.output(s2, file = "lm_BothACC_Nonverbal.txt")
# capture.output(s3, file = "lm_BothACC_Verbal.txt")
# capture.output(s4, file = "lm_BothRT_Total.txt")
# capture.output(s5, file = "lm_BothRT_Nonverbal.txt")
# capture.output(s6, file = "lm_BothRT_Verbal.txt")
# 
# capture.output(s7, file = "lm_GoACC_Total.txt")
# capture.output(s8, file = "lm_GoACC_Nonverbal.txt")
# capture.output(s9, file = "lm_GoACC_Verbal.txt")
# capture.output(s10, file = "lm_GoRT_Total.txt")
# capture.output(s11, file = "lm_GoRT_Nonverbal.txt")
# capture.output(s12, file = "lm_GoRT_Verbal.txt")
# 
# capture.output(s13, file = "lm_NoneACC_Total.txt")
# capture.output(s14, file = "lm_NoneACC_Nonverbal.txt")
# capture.output(s15, file = "lm_NoneACC_Verbal.txt")
# capture.output(s16, file = "lm_NoneRT_Total.txt")
# capture.output(s17, file = "lm_NoneRT_Nonverbal.txt")
# capture.output(s18, file = "lm_NoneRT_Verbal.txt")
# 
# 
