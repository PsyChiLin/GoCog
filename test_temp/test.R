library(afex)
dta <- read.csv("test_temp/table10_6_1.csv")
summary(aov(Value~A*B+Error(Subj), data = dta))
#aov_car(Value~A*B+Error(Subj), data = dta) can not run


# A1 <- aov(ACC ~ Group*Condition+Error(Subject/Condition) , data = Beh_2828)
# summary(A1)
# 
# # Use Within Subject Error for Within test
# # Between Subject Error for Between Test
# Beh_2828_ToM <- filter(Beh_2828,Condition == "ToM")
# A1_s <- aov(ACC ~ Group, data = Beh_2828_ToM )
# summary(A1_s)
# newF <- 0.04715/((0.7511+0.3772)/108) # Between Group anlaysis should use "All" error term 
# df(newF,1,54)
# 
# Beh_2828_RD <- filter(Beh_2828,Condition == "RD")
# A1_s2 <- aov(ACC ~ Group, data = Beh_2828_RD )
# summary(A1_s2)
# newF <- 0.002511/((0.7511+0.3772)/108)  # Between Group anlaysis should use "All" error term 
# df(newF,1,54)
# 
# Beh_2828_ASD <- filter(Beh_2828,Group == "ASD")
# A1_s3 <- aov(ACC ~ Condition, data = Beh_2828_ASD)
# summary(A1_s3)
# newF <- 0.004464/0.00699 # Within Group anlaysis should use "within" error term 
# df(newF,1,54)
# Beh_2828_CON <- filter(Beh_2828,Group == "CON")
# A1_s4 <- aov(ACC ~ Condition, data = Beh_2828_CON )
# summary(A1_s4)
# newF <- 0.04018/0.00699 # Within Group anlaysis should use "within" error term 
# df(newF,1,54)
# 
# 
# summary(R1 <- aov(RT_new ~ Group*Condition+Error(Subject/(Condition)) , data = Beh_2828))
# 
# 
# 
# t.test(filter(Beh_2828,Condition == "ToM")$ACC~filter(Beh_2828,Condition == "ToM")$Group,var.equal = T)
# t.test(filter(Beh_2828,Condition == "RD")$ACC~filter(Beh_2828,Condition == "RD")$Group,var.equal = T)
# t.test(filter(Beh_2828,Condition == "ToM")$RT_new~filter(Beh_2828,Condition == "ToM")$Group,var.equal = T)
# t.test(filter(Beh_2828,Condition == "RD")$RT_new~filter(Beh_2828,Condition == "RD")$Group,var.equal = T)
# 
