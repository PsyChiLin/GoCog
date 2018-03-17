############## Bootstrapping Dan ##############
library(randomForest)
library(dplyr)
library(reshape2)
library(e1071)
library(pROC)

rm(list = ls())
dta = read.csv("../GoCogdata/GoCog.csv")
head(dta)

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

meanrst_boot <- list()
meanrst_boot$train <- matrix(NA,10000,5) # 5 column : all, None
meanrst_boot$test <- matrix(NA,10000,5)  # 4 column : all, open, mid, end


for (t in 1:10){
  print(t)
  set.seed(t)
  # equally sampling
  dta_4c_dan_Calc <- filter(dta_4c_dan, CogTask == "Calc")
  dta_4c_dan_Reas <- filter(dta_4c_dan, CogTask == "Reas")
  dta_4c_dan_Spat <- filter(dta_4c_dan, CogTask == "Spat")
  dta_4c_dan_None <- filter(dta_4c_dan, CogTask == "None")
  # Bootstrapping
  dta_4c_dan_Calc_boot <- dta_4c_dan_Calc[sample(nrow(dta_4c_dan_Calc),replace = T),]
  dta_4c_dan_Reas_boot  <- dta_4c_dan_Reas[sample(nrow(dta_4c_dan_Reas),replace = T),]
  dta_4c_dan_Spat_boot  <- dta_4c_dan_Spat[sample(nrow(dta_4c_dan_Spat),replace = T),]
  dta_4c_dan_None_boot  <- dta_4c_dan_None[sample(nrow(dta_4c_dan_None),replace = T),]
  # random order
  dta_4c_dan_Calc_boot <- dta_4c_dan_Calc_boot[sample(nrow(dta_4c_dan_Calc_boot)),]
  dta_4c_dan_Reas_boot  <- dta_4c_dan_Reas_boot[sample(nrow(dta_4c_dan_Reas_boot)),]
  dta_4c_dan_Spat_boot <- dta_4c_dan_Spat_boot[sample(nrow(dta_4c_dan_Spat_boot)),]
  dta_4c_dan_None_boot <- dta_4c_dan_None_boot[sample(nrow(dta_4c_dan_None_boot)),]
  # bind data
  dta_4c_dan_boot <- rbind(dta_4c_dan_Calc_boot,dta_4c_dan_Reas_boot,dta_4c_dan_Spat_boot,dta_4c_dan_None_boot)
  # 8 folds
  folds <- rep(rep(1:8,2),4)
  
  # 5 column : all, Calc, Reas, Spat, None
  rst_boot <- list()
  rst_boot$train <- matrix(NA,8,5)
  rst_boot$test <- matrix(NA,8,5)
  
  for(i in 1:8){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dta_4c_dan_boot[testIndexes, c(4:10)]
    trainData <- dta_4c_dan_boot[-testIndexes, c(4:10)]
    rst_forests <- randomForest(CogTask~., data = trainData,
                                mtry = 8, 
                                ntree = 1000, 
                                nodesize = 4,
                                importance = T)
    y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
    y_train_hat <- predict(rst_forests,trainData,type="response")
    y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
    y_test_hat <- predict(rst_forests,testData,type="response")
    ce_train <- mean(y_train_hat!=trainData$CogTask, na.rm = T)
    ce_test <- mean(y_test_hat!=testData$CogTask, na.rm = T)
    
    rst_boot$train[i,1] <- ce_train 
    rst_boot$test[i,1] <- ce_test
    
    Calc_train <- filter(trainData, CogTask == "Calc")
    Calc_test <- filter(testData, CogTask == "Calc")
    y_train_prob_tree <- predict(rst_forests,Calc_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,Calc_train,type="response")
    y_test_prob_tree <- predict(rst_forests,Calc_test ,type="prob")[,2]
    y_test_hat <- predict(rst_forests,Calc_test,type="response")
    ce_train <- mean(y_train_hat!=Calc_train$CogTask)
    ce_test <- mean(y_test_hat!=Calc_test$CogTask)
    # 5 column : all, Calc, Reas, Spat, None
    rst_boot$train[i,2] <- ce_train
    rst_boot$test[i,2] <- ce_test
    
    Reas_train <- filter(trainData, CogTask == "Reas")
    Reas_test <- filter(testData, CogTask == "Reas")
    y_train_prob_tree <- predict(rst_forests,Reas_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,Reas_train,type="response")
    y_test_prob_tree <- predict(rst_forests,Reas_test,type="prob")[,2]
    y_test_hat <- predict(rst_forests,Reas_test,type="response")
    ce_train <- mean(y_train_hat != Reas_train $CogTask)
    ce_test <- mean(y_test_hat != Reas_test$CogTask)
    # 5 column : all, Calc, Reas, Spat, None
    rst_boot$train[i,3] <- ce_train
    rst_boot$test[i,3] <- ce_test
    
    Spat_train <- filter(trainData, CogTask == "Spat")
    Spat_test <- filter(testData, CogTask == "Spat")
    y_train_prob_tree <- predict(rst_forests,Spat_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,Spat_train,type="response")
    y_test_prob_tree <- predict(rst_forests,Spat_test,type="prob")[,2]
    y_test_hat <- predict(rst_forests,Spat_test,type="response")
    ce_train <- mean(y_train_hat != Spat_train $CogTask)
    ce_test <- mean(y_test_hat != Spat_test$CogTask)
    # 5 column : all, Calc, Reas, Spat, None
    rst_boot$train[i,4] <- ce_train
    rst_boot$test[i,4] <- ce_test
    
    None_train <- filter(trainData, CogTask == "None")
    None_test <- filter(testData, CogTask == "None")
    y_train_prob_tree <- predict(rst_forests,None_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,None_train,type="response")
    y_test_prob_tree <- predict(rst_forests,None_test,type="prob")[,2]
    y_test_hat <- predict(rst_forests,None_test,type="response")
    ce_train <- mean(y_train_hat != None_train $CogTask)
    ce_test <- mean(y_test_hat != None_test$CogTask)
    # 5 column : all, Calc, Reas, Spat, None
    rst_boot$train[i,5] <- ce_train 
    rst_boot$test[i,5] <- ce_test
    
  }
  # 5 column : all, Calc, Reas, Spat, None
  meanrst_boot$train[t,1] <- mean(rst_boot$train[,1])
  meanrst_boot$test[t,1] <- mean(rst_boot$test[,1])
  meanrst_boot$train[t,2] <- mean(rst_boot$train[,2])
  meanrst_boot$test[t,2] <- mean(rst_boot$test[,2])
  meanrst_boot$train[t,3] <- mean(rst_boot$train[,3])
  meanrst_boot$test[t,3] <- mean(rst_boot$test[,3])
  meanrst_boot$train[t,4] <- mean(rst_boot$train[,4])
  meanrst_boot$test[t,4] <- mean(rst_boot$test[,4])
  meanrst_boot$train[t,5] <- mean(rst_boot$train[,5])
  meanrst_boot$test[t,5] <- mean(rst_boot$test[,5])
  
}
# 4 column : all, Calc, Reas, Spat
quantile(meanrst_boot$test[,1],c(.025,.975),na.rm = T)
quantile(meanrst_boot$test[,2],c(.025,.975),na.rm = T) 
quantile(meanrst_boot$test[,3],c(.025,.975),na.rm = T) 
quantile(meanrst_boot$test[,4],c(.025,.975),na.rm = T) 
quantile(meanrst_boot$test[,5],c(.025,.975),na.rm = T) 

saveRDS(meanrst_boot,file = "../GoCogdata/CogTask_RF_Boot_10000_Ex_Dan.Rdata")

############## Bootstrapping Kyu ##############

library(randomForest)
library(dplyr)
library(reshape2)
library(e1071)
library(pROC)

rm(list = ls())
dta = read.csv("../GoCogdata/GoCog.csv")
head(dta)

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

meanrst_boot <- list()
meanrst_boot$train <- matrix(NA,10000,5) # 5 column : all, None
meanrst_boot$test <- matrix(NA,10000,5)  # 4 column : all, open, mid, end


for (t in 1:10){
  print(t)
  set.seed(t)
  # equally sampling
  dta_4c_kyu_Calc <- filter(dta_4c_kyu, CogTask == "Calc")
  dta_4c_kyu_Reas <- filter(dta_4c_kyu, CogTask == "Reas")
  dta_4c_kyu_Spat <- filter(dta_4c_kyu, CogTask == "Spat")
  dta_4c_kyu_None <- filter(dta_4c_kyu, CogTask == "None")
  # Bootstrapping
  dta_4c_kyu_Calc_boot <- dta_4c_kyu_Calc[sample(nrow(dta_4c_kyu_Calc),replace = T),]
  dta_4c_kyu_Reas_boot  <- dta_4c_kyu_Reas[sample(nrow(dta_4c_kyu_Reas),replace = T),]
  dta_4c_kyu_Spat_boot  <- dta_4c_kyu_Spat[sample(nrow(dta_4c_kyu_Spat),replace = T),]
  dta_4c_kyu_None_boot  <- dta_4c_kyu_None[sample(nrow(dta_4c_kyu_None),replace = T),]
  # random order
  dta_4c_kyu_Calc_boot <- dta_4c_kyu_Calc_boot[sample(nrow(dta_4c_kyu_Calc_boot)),]
  dta_4c_kyu_Reas_boot  <- dta_4c_kyu_Reas_boot[sample(nrow(dta_4c_kyu_Reas_boot)),]
  dta_4c_kyu_Spat_boot <- dta_4c_kyu_Spat_boot[sample(nrow(dta_4c_kyu_Spat_boot)),]
  dta_4c_kyu_None_boot <- dta_4c_kyu_None_boot[sample(nrow(dta_4c_kyu_None_boot)),]
  # bind data
  dta_4c_kyu_boot <- rbind(dta_4c_kyu_Calc_boot,dta_4c_kyu_Reas_boot,dta_4c_kyu_Spat_boot,dta_4c_kyu_None_boot)
  # 8 folds
  folds <- rep(rep(1:8,1),4)
  
  # 5 column : all, Calc, Reas, Spat, None
  rst_boot <- list()
  rst_boot$train <- matrix(NA,8,5)
  rst_boot$test <- matrix(NA,8,5)
  
  for(i in 1:8){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dta_4c_kyu_boot[testIndexes, c(4:10)]
    trainData <- dta_4c_kyu_boot[-testIndexes, c(4:10)]
    rst_forests <- randomForest(CogTask~., data = trainData,
                                mtry = 8, 
                                ntree = 1000, 
                                nodesize = 4,
                                importance = T)
    y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
    y_train_hat <- predict(rst_forests,trainData,type="response")
    y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
    y_test_hat <- predict(rst_forests,testData,type="response")
    ce_train <- mean(y_train_hat!=trainData$CogTask, na.rm = T)
    ce_test <- mean(y_test_hat!=testData$CogTask, na.rm = T)
    
    rst_boot$train[i,1] <- ce_train 
    rst_boot$test[i,1] <- ce_test
    
    Calc_train <- filter(trainData, CogTask == "Calc")
    Calc_test <- filter(testData, CogTask == "Calc")
    y_train_prob_tree <- predict(rst_forests,Calc_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,Calc_train,type="response")
    y_test_prob_tree <- predict(rst_forests,Calc_test ,type="prob")[,2]
    y_test_hat <- predict(rst_forests,Calc_test,type="response")
    ce_train <- mean(y_train_hat!=Calc_train$CogTask)
    ce_test <- mean(y_test_hat!=Calc_test$CogTask)
    # 5 column : all, Calc, Reas, Spat, None
    rst_boot$train[i,2] <- ce_train
    rst_boot$test[i,2] <- ce_test
    
    Reas_train <- filter(trainData, CogTask == "Reas")
    Reas_test <- filter(testData, CogTask == "Reas")
    y_train_prob_tree <- predict(rst_forests,Reas_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,Reas_train,type="response")
    y_test_prob_tree <- predict(rst_forests,Reas_test,type="prob")[,2]
    y_test_hat <- predict(rst_forests,Reas_test,type="response")
    ce_train <- mean(y_train_hat != Reas_train $CogTask)
    ce_test <- mean(y_test_hat != Reas_test$CogTask)
    # 5 column : all, Calc, Reas, Spat, None
    rst_boot$train[i,3] <- ce_train
    rst_boot$test[i,3] <- ce_test
    
    Spat_train <- filter(trainData, CogTask == "Spat")
    Spat_test <- filter(testData, CogTask == "Spat")
    y_train_prob_tree <- predict(rst_forests,Spat_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,Spat_train,type="response")
    y_test_prob_tree <- predict(rst_forests,Spat_test,type="prob")[,2]
    y_test_hat <- predict(rst_forests,Spat_test,type="response")
    ce_train <- mean(y_train_hat != Spat_train $CogTask)
    ce_test <- mean(y_test_hat != Spat_test$CogTask)
    # 5 column : all, Calc, Reas, Spat, None
    rst_boot$train[i,4] <- ce_train
    rst_boot$test[i,4] <- ce_test
    
    None_train <- filter(trainData, CogTask == "None")
    None_test <- filter(testData, CogTask == "None")
    y_train_prob_tree <- predict(rst_forests,None_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,None_train,type="response")
    y_test_prob_tree <- predict(rst_forests,None_test,type="prob")[,2]
    y_test_hat <- predict(rst_forests,None_test,type="response")
    ce_train <- mean(y_train_hat != None_train $CogTask)
    ce_test <- mean(y_test_hat != None_test$CogTask)
    # 5 column : all, Calc, Reas, Spat, None
    rst_boot$train[i,5] <- ce_train 
    rst_boot$test[i,5] <- ce_test
    
  }
  # 5 column : all, Calc, Reas, Spat, None
  meanrst_boot$train[t,1] <- mean(rst_boot$train[,1])
  meanrst_boot$test[t,1] <- mean(rst_boot$test[,1])
  meanrst_boot$train[t,2] <- mean(rst_boot$train[,2])
  meanrst_boot$test[t,2] <- mean(rst_boot$test[,2])
  meanrst_boot$train[t,3] <- mean(rst_boot$train[,3])
  meanrst_boot$test[t,3] <- mean(rst_boot$test[,3])
  meanrst_boot$train[t,4] <- mean(rst_boot$train[,4])
  meanrst_boot$test[t,4] <- mean(rst_boot$test[,4])
  meanrst_boot$train[t,5] <- mean(rst_boot$train[,5])
  meanrst_boot$test[t,5] <- mean(rst_boot$test[,5])
  
}
# 4 column : all, Calc, Reas, Spat
quantile(meanrst_boot$test[,1],c(.025,.975),na.rm = T)
quantile(meanrst_boot$test[,2],c(.025,.975),na.rm = T) 
quantile(meanrst_boot$test[,3],c(.025,.975),na.rm = T) 
quantile(meanrst_boot$test[,4],c(.025,.975),na.rm = T) 
quantile(meanrst_boot$test[,5],c(.025,.975),na.rm = T) 

saveRDS(meanrst_boot,file = "../GoCogdata/CogTask_RF_Boot_10000_Ex_Kyu.Rdata")

