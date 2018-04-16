######### CogTask Result #########
library(randomForest)
library(dplyr)
library(reshape2)
library(e1071)
library(pROC)

# Read Data
rm(list = ls())
dta = read.csv("../GoCogdata/GoCog.csv")
head(dta)

# Create Data For "All", "Dan", "Kyu"
dta_bothacc <- dcast(dta,Subj+Age+SubjGroup+CogTask~GoStage,
                     value.var = "Both_ACC")
colnames(dta_bothacc)[5:7] <- c("End_Both_ACC","Mid_Both_ACC","Open_Both_ACC")
dta_bothrt <- dcast(dta,Subj+Age+SubjGroup+CogTask~GoStage,
                    value.var = "Both_RT")
colnames(dta_bothrt)[5:7] <- c("End_Both_RT","Mid_Both_RT","Open_Both_RT")
dta_4c <- full_join(dta_bothacc,dta_bothrt)
dta_4c$Subj <- as.factor(dta_4c$Subj)
head(dta_4c)
dta_4c_dan <- filter(dta_4c, SubjGroup == "Dan")
dta_4c_kyu <- filter(dta_4c, SubjGroup == "Kyu")

# Create Data Frame to store the output
RF_CogTask <- as.data.frame(matrix(NA,3,6))
colnames(RF_CogTask) <- c("Type","All","Calc","Reas","Spat","None")
RF_CogTask$Type <- c("AllSubj","Dan","Kyu")

############### Parameter ###############
# allmtry <- 2
# allnodesize <- 20
# danmtry <- 2
# dannodesize <- 10
# kyumtry <- 3
# kyunodesize <- 5
# 
# overallntree <-  1000

################ Allsubj ################ 
set.seed(1)
# equally sampling
dta_4c_Calc <- filter(dta_4c, CogTask == "Calc")
dta_4c_Reas <- filter(dta_4c, CogTask == "Reas")
dta_4c_Spat <- filter(dta_4c, CogTask == "Spat")
dta_4c_None <- filter(dta_4c, CogTask == "None")
# random order
dta_4c_Calc <- dta_4c_Calc[sample(nrow(dta_4c_Calc)),]
dta_4c_Reas  <- dta_4c_Reas[sample(nrow(dta_4c_Reas)),]
dta_4c_Spat <- dta_4c_Spat[sample(nrow(dta_4c_Spat)),]
dta_4c_None <- dta_4c_None[sample(nrow(dta_4c_None)),]
# bind data
dta_4c_new <- rbind(dta_4c_Calc,dta_4c_Reas,dta_4c_Spat,dta_4c_None)
row.names(dta_4c_new) <- 1:dim(dta_4c_new)[1]

head(class.ind(dta_4c_new$CogTask))
dta_4c_new <- cbind(dta_4c_new, class.ind(dta_4c_new$CogTask))
formula.bpn <- Calc+None+Reas+Spat~End_Both_ACC+Mid_Both_ACC+Open_Both_ACC+End_Both_RT+Mid_Both_RT+Open_Both_RT
  
bpn <- neuralnet(formula = formula.bpn, 
                 data = dta_4c_new ,
                 hidden = c(4,2),       
                 learningrate = 0.001, # learning rate
                 threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                 stepmax = 5e5)
pred <- compute(bpn,  dta_4c_new[,5:10])  
pred$net.result



