library(randomForest)
library(dplyr)
library(reshape2)
library(e1071)
library(pROC)

############## Bootstrapping Exact ##############
rm(list = ls())
dta = read.csv("../GoCogdata/GoCog.csv")
head(dta)
str(dta)

dta_bothacc <- dcast(dta,Subj+Age+SubjGroup~GoStage+CogTask,
                     value.var = "Both_ACC")
dta_bothacc_dan <- filter(dta_bothacc,SubjGroup != "Kyu")
dta_bothacc_kyu <- filter(dta_bothacc,SubjGroup == "Kyu")
dta_bothacc2 <- rbind(dta_bothacc_kyu,dta_bothacc_dan)

meanrst_boot <- list()
meanrst_boot$train <- matrix(NA,10000,2)
meanrst_boot$test <- matrix(NA,10000,2)

for (t in 1:10000){
  set.seed(t)
  print(t)
  
  d <- dta_bothacc_dan[sample(nrow(dta_bothacc_dan),replace = T),] 
  k <- dta_bothacc_kyu[sample(nrow(dta_bothacc_kyu),replace = T),]
  dta_bothacc_boot <- rbind(d,k)

  dta_bothacc_boot_dan <- filter(dta_bothacc_boot,SubjGroup != "Kyu")
  dta_bothacc_boot_dan <- dta_bothacc_boot_dan[sample(nrow(dta_bothacc_boot_dan)),]
  dta_bothacc_boot_kyu <- filter(dta_bothacc_boot,SubjGroup == "Kyu")
  dta_bothacc_boot_kyu <- dta_bothacc_boot_kyu[sample(nrow(dta_bothacc_boot_kyu)),]
  dta_bothacc_boot2 <- rbind(dta_bothacc_boot_kyu,dta_bothacc_boot_dan)

  folds <- c(sample(1:8),sample(c(sample(1:8),sample(1:8))))
  rst_boot <- list()
  rst_boot$train <- matrix(NA,8,2)
  rst_boot$test <- matrix(NA,8,2)
  for(i in 1:8){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dta_bothacc_boot2[testIndexes, c(3:15)]
    trainData <- dta_bothacc_boot2[-testIndexes, c(3:15)]
    rst_forests <- randomForest(SubjGroup~.,
                                data = trainData,
                                mtry = 2, 
                                ntree = 1000, 
                                nodesize = 1,
                                importance = T)
    y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
    y_train_hat <- predict(rst_forests,trainData,type="response")
    y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
    y_test_hat <- predict(rst_forests,testData,type="response")
    ce_train <- mean(y_train_hat!=trainData$SubjGroup, na.rm = T)
    ce_test <- mean(y_test_hat!=testData$SubjGroup, na.rm = T)
    auc_train <- pROC::auc(trainData$SubjGroup, y_train_prob_tree)
    auc_test <- pROC::auc(testData$SubjGroup, y_test_prob_tree) 
    rst_boot$train[i,1] <- ce_train 
    rst_boot$test[i,1] <- ce_test
    rst_boot$train[i,2] <-auc_train
    rst_boot$test[i,2] <- auc_test
  }
  meanrst_boot$train[t,1] <- mean(rst_boot$train[,1])
  meanrst_boot$train[t,2] <- mean(rst_boot$train[,2])
  meanrst_boot$test[t,1] <- mean(rst_boot$test[,1])
  meanrst_boot$test[t,2] <- mean(rst_boot$test[,2])
}

quantile(meanrst_boot$test[,1],c(.025,.975),na.rm = T)
mean(meanrst_boot$test[,1],na.rm = T)
quantile(meanrst_boot$test[,2],c(.025,.975),na.rm = T)
mean(meanrst_boot$test[,2],na.rm = T)

saveRDS(meanrst_boot,file = "../GoCogdata/DanKyu_BothACC_RF_10000_Ex.Rdata")

############## Bootstrapping Undersampling ##############
rm(list = ls())
dta = read.csv("../GoCogdata/GoCog.csv")
head(dta)
str(dta)

dta_bothacc <- dcast(dta,Subj+Age+SubjGroup~GoStage+CogTask,
                     value.var = "Both_ACC")
dta_bothacc_dan <- filter(dta_bothacc,SubjGroup != "Kyu")
dta_bothacc_kyu <- filter(dta_bothacc,SubjGroup == "Kyu")
dta_bothacc2 <- rbind(dta_bothacc_kyu,dta_bothacc_dan)

meanrst_boot <- list()
meanrst_boot$train <- matrix(NA,10000,2)
meanrst_boot$test <- matrix(NA,10000,2)

for (t in 1:10000){
  set.seed(t)
  print(t)
  
  d <- dta_bothacc_dan[sample(nrow(dta_bothacc_dan),8,replace = T),] 
  k <- dta_bothacc_kyu[sample(nrow(dta_bothacc_kyu),replace = T),]
  dta_bothacc_boot <- rbind(d,k)
  
  dta_bothacc_boot_dan <- filter(dta_bothacc_boot,SubjGroup != "Kyu")
  dta_bothacc_boot_dan <- dta_bothacc_boot_dan[sample(nrow(dta_bothacc_boot_dan)),]
  dta_bothacc_boot_kyu <- filter(dta_bothacc_boot,SubjGroup == "Kyu")
  dta_bothacc_boot_kyu <- dta_bothacc_boot_kyu[sample(nrow(dta_bothacc_boot_kyu)),]
  dta_bothacc_boot2 <- rbind(dta_bothacc_boot_kyu,dta_bothacc_boot_dan)
  
  folds <- c(sample(1:8),sample(1:8))
  rst_boot <- list()
  rst_boot$train <- matrix(NA,8,2)
  rst_boot$test <- matrix(NA,8,2)
  for(i in 1:8){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dta_bothacc_boot2[testIndexes, c(3:15)]
    trainData <- dta_bothacc_boot2[-testIndexes, c(3:15)]
    rst_forests <- randomForest(SubjGroup~.,
                                data = trainData,
                                mtry = 2, 
                                ntree = 1000, 
                                nodesize = 1,
                                importance = T)
    y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
    y_train_hat <- predict(rst_forests,trainData,type="response")
    y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
    y_test_hat <- predict(rst_forests,testData,type="response")
    ce_train <- mean(y_train_hat!=trainData$SubjGroup, na.rm = T)
    ce_test <- mean(y_test_hat!=testData$SubjGroup, na.rm = T)
    auc_train <- pROC::auc(trainData$SubjGroup, y_train_prob_tree)
    auc_test <- pROC::auc(testData$SubjGroup, y_test_prob_tree) 
    rst_boot$train[i,1] <- ce_train 
    rst_boot$test[i,1] <- ce_test
    rst_boot$train[i,2] <-auc_train
    rst_boot$test[i,2] <- auc_test
  }
  meanrst_boot$train[t,1] <- mean(rst_boot$train[,1])
  meanrst_boot$train[t,2] <- mean(rst_boot$train[,2])
  meanrst_boot$test[t,1] <- mean(rst_boot$test[,1])
  meanrst_boot$test[t,2] <- mean(rst_boot$test[,2])
}

quantile(meanrst_boot$test[,1],c(.025,.975),na.rm = T)
mean(meanrst_boot$test[,1],na.rm = T)
quantile(meanrst_boot$test[,2],c(.025,.975),na.rm = T)
mean(meanrst_boot$test[,2],na.rm = T)

saveRDS(meanrst_boot,file = "../GoCogdata/DanKyu_BothACC_RF_10000_US.Rdata")

############## Bootstrapping Oversampling ##############
rm(list = ls())
dta = read.csv("../GoCogdata/GoCog.csv")
head(dta)
str(dta)

dta_bothacc <- dcast(dta,Subj+Age+SubjGroup~GoStage+CogTask,
                     value.var = "Both_ACC")
dta_bothacc_dan <- filter(dta_bothacc,SubjGroup != "Kyu")
dta_bothacc_kyu <- filter(dta_bothacc,SubjGroup == "Kyu")
dta_bothacc2 <- rbind(dta_bothacc_kyu,dta_bothacc_dan)

meanrst_boot <- list()
meanrst_boot$train <- matrix(NA,10000,2)
meanrst_boot$test <- matrix(NA,10000,2)

for (t in 1:10000){
  set.seed(t)
  print(t)
  
  d <- dta_bothacc_dan[sample(nrow(dta_bothacc_dan),replace = T),] 
  k <- dta_bothacc_kyu[sample(nrow(dta_bothacc_kyu),replace = T),]
  dta_bothacc_boot <- rbind(d,k)
  
  dta_bothacc_boot_dan <- filter(dta_bothacc_boot,SubjGroup != "Kyu")
  dta_bothacc_boot_dan <- dta_bothacc_boot_dan[sample(nrow(dta_bothacc_boot_dan)),]
  dta_bothacc_boot_kyu <- filter(dta_bothacc_boot,SubjGroup == "Kyu")
  dta_bothacc_boot_kyu <- dta_bothacc_boot_kyu[sample(nrow(dta_bothacc_boot_kyu)),]
  dta_bothacc_boot2 <- rbind(dta_bothacc_boot_kyu,dta_bothacc_boot_dan)
  
  folds <- c(1:8,cut(seq(1,nrow(dta_bothacc_boot_dan)),breaks=8,labels=FALSE))
  rst_boot <- list()
  rst_boot$train <- matrix(NA,8,2)
  rst_boot$test <- matrix(NA,8,2)
  for(i in 1:8){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dta_bothacc_boot2[testIndexes, c(3:15)]
    trainData <- dta_bothacc_boot2[-testIndexes, c(3:15)]
    kyu <- filter(trainData,SubjGroup == "Kyu")
    dan <- filter(trainData,SubjGroup != "Kyu")
    trainData <- rbind(trainData,kyu[sample(1:nrow(kyu), (dim(dan)[1] - dim(kyu)[1])),])
    rst_forests <- randomForest(SubjGroup~.,
                                data = trainData,
                                mtry = 2, 
                                ntree = 1000, 
                                nodesize = 1,
                                importance = T)
    y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
    y_train_hat <- predict(rst_forests,trainData,type="response")
    y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
    y_test_hat <- predict(rst_forests,testData,type="response")
    ce_train <- mean(y_train_hat!=trainData$SubjGroup, na.rm = T)
    ce_test <- mean(y_test_hat!=testData$SubjGroup, na.rm = T)
    auc_train <- pROC::auc(trainData$SubjGroup, y_train_prob_tree)
    auc_test <- pROC::auc(testData$SubjGroup, y_test_prob_tree) 
    rst_boot$train[i,1] <- ce_train 
    rst_boot$test[i,1] <- ce_test
    rst_boot$train[i,2] <-auc_train
    rst_boot$test[i,2] <- auc_test
  }
  meanrst_boot$train[t,1] <- mean(rst_boot$train[,1])
  meanrst_boot$train[t,2] <- mean(rst_boot$train[,2])
  meanrst_boot$test[t,1] <- mean(rst_boot$test[,1])
  meanrst_boot$test[t,2] <- mean(rst_boot$test[,2])
}

quantile(meanrst_boot$test[,1],c(.025,.975),na.rm = T)
mean(meanrst_boot$test[,1],na.rm = T)
quantile(meanrst_boot$test[,2],c(.025,.975),na.rm = T)
mean(meanrst_boot$test[,2],na.rm = T)

saveRDS(meanrst_boot,file = "../GoCogdata/DanKyu_BothACC_RF_10000_OS.Rdata")