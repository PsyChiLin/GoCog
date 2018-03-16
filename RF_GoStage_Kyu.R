library(randomForest)
library(dplyr)
library(reshape2)
library(e1071)
library(pROC)

rm(list = ls())
dta = read.csv("../GoCogdata/GoCog.csv")
head(dta)

dta_bothacc <- dcast(dta,Subj+Age+SubjGroup+GoStage~CogTask,
                     value.var = "Both_ACC")
colnames(dta_bothacc)[5:8] <- c("Calc_Both_ACC","None_Both_ACC",
                                "Reas_Both_ACC","Spat_Both_ACC")
dta_bothrt <- dcast(dta,Subj+Age+SubjGroup+GoStage~CogTask,
                    value.var = "Both_RT")
colnames(dta_bothrt)[5:8] <- c("Calc_Both_RT","None_Both_RT",
                               "Reas_Both_RT","Spat_Both_RT")
dta_3s <- full_join(dta_bothacc,dta_bothrt)
dta_3s$Subj <- as.factor(dta_3s$Subj)
dta_3s_dan <- filter(dta_3s, SubjGroup == "Dan")
dta_3s_kyu <- filter(dta_3s, SubjGroup == "Kyu")

############## Bootstrapping : Dan ############

set.seed(1)
meanrst_boot <- list()
meanrst_boot$train <- matrix(NA,10000,4) # 4 column : all, open, mid, end
meanrst_boot$test <- matrix(NA,10000,4)  # 4 column : all, open, mid, end


for (t in 1:10000){
  print(t)
  set.seed(t)
  # equally sampling
  dta_3s_kyu_Open <- filter(dta_3s_kyu, GoStage == "Open")
  dta_3s_kyu_Mid <- filter(dta_3s_kyu, GoStage == "Mid")
  dta_3s_kyu_End <- filter(dta_3s_kyu, GoStage == "End")
  # Bootstrapping
  dta_3s_kyu_Open_boot <- dta_3s_kyu_Open[sample(nrow(dta_3s_kyu_Open),replace = T),]
  dta_3s_kyu_Mid_boot  <- dta_3s_kyu_Mid[sample(nrow(dta_3s_kyu_Mid),replace = T),]
  dta_3s_kyu_End_boot  <- dta_3s_kyu_End[sample(nrow(dta_3s_kyu_End),replace = T),]
  # random order
  dta_3s_kyu_Open_boot <- dta_3s_kyu_Open_boot[sample(nrow(dta_3s_kyu_Open_boot)),]
  dta_3s_kyu_Mid_boot  <- dta_3s_kyu_Mid_boot[sample(nrow(dta_3s_kyu_Mid_boot)),]
  dta_3s_kyu_End_boot <- dta_3s_kyu_End_boot[sample(nrow(dta_3s_kyu_End_boot)),]
  # bind data
  dta_3s_kyu_boot <- rbind(dta_3s_kyu_Open_boot,dta_3s_kyu_Mid_boot,dta_3s_kyu_End_boot)
  folds <- c(1:8,1:8,1:8)
  
  rst_boot <- list()
  rst_boot$train <- matrix(NA,8,4) # 4 column : all, open, mid, end
  rst_boot$test <- matrix(NA,8,4)  # 4 column : all, open, mid, end
  
  for(i in 1:8){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dta_3s_kyu_boot[testIndexes, c(4:12)]
    trainData <- dta_3s_kyu_boot[-testIndexes, c(4:12)]
    rst_forests <- randomForest(GoStage~., data = trainData,
                                mtry = 8, 
                                ntree = 1000, 
                                nodesize = 4,
                                importance = T)
    y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
    y_train_hat <- predict(rst_forests,trainData,type="response")
    y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
    y_test_hat <- predict(rst_forests,testData,type="response")
    ce_train <- mean(y_train_hat!=trainData$GoStage, na.rm = T)
    ce_test <- mean(y_test_hat!=testData$GoStage, na.rm = T)
    #auc_train <- pROC::auc(trainData$GoStage, y_train_prob_tree)
    #auc_test <- pROC::auc(testData$GoStage, y_test_prob_tree) 
    rst_boot$train[i,1] <- ce_train 
    rst_boot$test[i,1] <- ce_test
    #rst_boot$train[i,2] <-auc_train
    #rst_boot$test[i,2] <- auc_test
    
    Open_train <- filter(trainData, GoStage == "Open")
    Open_test <- filter(testData, GoStage == "Open")
    y_train_prob_tree <- predict(rst_forests,Open_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,Open_train,type="response")
    y_test_prob_tree <- predict(rst_forests,Open_test ,type="prob")[,2]
    y_test_hat <- predict(rst_forests,Open_test,type="response")
    ce_train <- mean(y_train_hat!=Open_train$GoStage)
    ce_test <- mean(y_test_hat!=Open_test$GoStage)
    rst_boot$train[i,2] <- ce_train # 4 column : all, open, mid, end
    rst_boot$test[i,2] <- ce_test   # 4 column : all, open, mid, end
    
    Mid_train <- filter(trainData, GoStage == "Mid")
    Mid_test <- filter(testData, GoStage == "Mid")
    y_train_prob_tree <- predict(rst_forests,Mid_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,Mid_train,type="response")
    y_test_prob_tree <- predict(rst_forests,Mid_test,type="prob")[,2]
    y_test_hat <- predict(rst_forests,Mid_test,type="response")
    ce_train <- mean(y_train_hat != Mid_train $GoStage)
    ce_test <- mean(y_test_hat != Mid_test$GoStage)
    rst_boot$train[i,3] <- ce_train # 4 column : all, open, mid, end
    rst_boot$test[i,3] <- ce_test   # 4 column : all, open, mid, end
    
    End_train <- filter(trainData, GoStage == "End")
    End_test <- filter(testData, GoStage == "End")
    y_train_prob_tree <- predict(rst_forests,End_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,End_train,type="response")
    y_test_prob_tree <- predict(rst_forests,End_test,type="prob")[,2]
    y_test_hat <- predict(rst_forests,End_test,type="response")
    ce_train <- mean(y_train_hat != End_train $GoStage)
    ce_test <- mean(y_test_hat != End_test$GoStage)
    rst_boot$train[i,4] <- ce_train # 4 column : all, open, mid, end
    rst_boot$test[i,4] <- ce_test   # 4 column : all, open, mid, end
    
  }
  # 4 column : all, open, mid, end
  meanrst_boot$train[t,1] <- mean(rst_boot$train[,1])
  meanrst_boot$test[t,1] <- mean(rst_boot$test[,1])
  meanrst_boot$train[t,2] <- mean(rst_boot$train[,2])
  meanrst_boot$test[t,2] <- mean(rst_boot$test[,2])
  meanrst_boot$train[t,3] <- mean(rst_boot$train[,3])
  meanrst_boot$test[t,3] <- mean(rst_boot$test[,3])
  meanrst_boot$train[t,4] <- mean(rst_boot$train[,4])
  meanrst_boot$test[t,4] <- mean(rst_boot$test[,4])
  
}
# 4 column : all, open, mid, end
quantile(meanrst_boot$test[,1],c(.025,.975),na.rm = T)
quantile(meanrst_boot$test[,2],c(.025,.975),na.rm = T) 
quantile(meanrst_boot$test[,3],c(.025,.975),na.rm = T) 
quantile(meanrst_boot$test[,4],c(.025,.975),na.rm = T) 

saveRDS(meanrst_boot,file = "../GoCogdata/GoStage_BothACC_RF_10000_Ex_Kyu.Rdata")

