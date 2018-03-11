library(dplyr)
library(reshape2)
library(e1071)
library(pROC)

dta = read.csv("data/GoCog_from20170706.csv")
#head(dta)

dta_bothacc <- dcast(dta,Subj+Age+SubjGroup+SubjRank~GoStage+CogTask,
                     value.var = "Both_ACC")
#table(dta_bothacc$SubjGroup)

#Randomly shuffle the data
set.seed(1)
dta_bothacc_dan <- filter(dta_bothacc,SubjGroup != "業餘級位(Kyu)")
dta_bothacc_dan<-dta_bothacc_dan[sample(nrow(dta_bothacc_dan)),]
dta_bothacc_kyu <- filter(dta_bothacc,SubjGroup == "業餘級位(Kyu)")
dta_bothacc_kyu<-dta_bothacc_kyu[sample(nrow(dta_bothacc_kyu)),]
dta_bothacc2 <- rbind(dta_bothacc_kyu,dta_bothacc_dan)

#Create 10 equally size folds
folds <- c(1:8,cut(seq(1,nrow(dta_bothacc_dan)),breaks=8,labels=FALSE))

#Perform 8 fold cross validation
rst <- list()
rst$train <- matrix(NA,8,2)
rst$test <- matrix(NA,8,2)
for(i in 1:8){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- dta_bothacc2[testIndexes, c(3,5:16)]
  trainData <- dta_bothacc2[-testIndexes, c(3,5:16)]
  kyu <- filter(trainData,SubjGroup == "業餘級位(Kyu)")
  dan <- filter(trainData,SubjGroup != "業餘級位(Kyu)")
  trainData <- rbind(trainData,kyu[sample(1:nrow(kyu), (dim(dan)[1] - dim(kyu)[1])),])
  rst_svm_tune <- tune(svm, SubjGroup~., data = trainData,
                       type = "C-classification", 
                       ranges =  list(cost = seq(.1,2,.1)),
                                     #gamma = seq(.1,1,.1)),
                       kernel = "linear",probability = T,
                       tunecontrol = tune.control(sampling = "cross", cross = 3))
  rst_svm <- svm(SubjGroup~., data = trainData,
                 type = "C-classification", 
                 cost =  rst_svm_tune$best.model$cost,
                 #gamma =  rst_svm_tune$best.model$gamma,
                 kernel = "linear",probability = T)
  yhat_train_svm <- predict(rst_svm, 
                            newdata = trainData,
                            probability = T)
  yhat_test_svm <- predict(rst_svm, 
                           newdata = testData,
                           probability = T)
  rst$train[i,1] <- mean(trainData$SubjGroup != yhat_train_svm)
  rst$test[i,1] <- mean(testData$SubjGroup != yhat_test_svm)
  rst$train[i,2] <- auc(trainData$SubjGroup,attr(yhat_train_svm,"probabilities")[,1])
  rst$test[i,2] <- auc(testData$SubjGroup,attr(yhat_test_svm,"probabilities")[,1])
}

rst$test[,1];mean(rst$test[,1])
rst$test[,2];mean(rst$test[,2])

## Permutation test
meanrst_per <- list()
meanrst_per$train <- matrix(NA,1000,2)
meanrst_per$test <- matrix(NA,1000,2)
set.seed(1)
for (t in 1:1000){
  print(t)
  dta_bothacc_permute <- dta_bothacc[sample(nrow(dta_bothacc)),]
  dta_bothacc_permute$SubjGroup <- sample(dta_bothacc_permute$SubjGroup)
  
  dta_bothacc_permute_dan <- filter(dta_bothacc_permute,SubjGroup != "業餘級位(Kyu)")
  dta_bothacc_permute_dan<-dta_bothacc_permute_dan[sample(nrow(dta_bothacc_permute_dan)),]
  dta_bothacc_permute_kyu <- filter(dta_bothacc_permute,SubjGroup == "業餘級位(Kyu)")
  dta_bothacc_permute_kyu<-dta_bothacc_permute_kyu[sample(nrow(dta_bothacc_permute_kyu)),]
  dta_bothacc_permute2 <- rbind(dta_bothacc_permute_kyu,dta_bothacc_permute_dan)
  
  folds <- c(1:8,cut(seq(1,nrow(dta_bothacc_permute_dan)),breaks=8,labels=FALSE))

  rst_per <- list()
  rst_per$train <- matrix(NA,8,2)
  rst_per$test <- matrix(NA,8,2)
  for(i in 1:8){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dta_bothacc2[testIndexes, c(3,5:16)]
    trainData <- dta_bothacc2[-testIndexes, c(3,5:16)]
    kyu <- filter(trainData,SubjGroup == "業餘級位(Kyu)")
    dan <- filter(trainData,SubjGroup != "業餘級位(Kyu)")
    trainData <- rbind(trainData,kyu[sample(1:nrow(kyu), (dim(dan)[1] - dim(kyu)[1])),])
    rst_svm_tune <- tune(svm, SubjGroup~., data = trainData,
                         type = "C-classification", 
                         ranges =  list(cost = seq(.1,2,.1)),
                         #gamma = seq(.1,1,.1)),
                         kernel = "linear",probability = T,
                         tunecontrol = tune.control(sampling = "cross", cross = 3))
    rst_svm <- svm(SubjGroup~., data = trainData,
                   type = "C-classification", 
                   cost =  rst_svm_tune$best.model$cost,
                   #gamma =  rst_svm_tune$best.model$gamma,
                   kernel = "linear",probability = T)
    yhat_train_svm <- predict(rst_svm, 
                              newdata = trainData,
                              probability = T)
    yhat_test_svm <- predict(rst_svm, 
                             newdata = testData,
                             probability = T)
    rst_per$train[i,1] <- mean(trainData$SubjGroup != yhat_train_svm)
    rst_per$test[i,1] <- mean(testData$SubjGroup != yhat_test_svm)
    rst_per$train[i,2] <- auc(trainData$SubjGroup,attr(yhat_train_svm,"probabilities")[,1])
    rst_per$test[i,2] <- auc(testData$SubjGroup,attr(yhat_test_svm,"probabilities")[,1])
  }
  meanrst_per$train[t,1] <- mean(rst_per$train[,1])
  meanrst_per$train[t,2] <- mean(rst_per$train[,2])
  meanrst_per$test[t,1] <- mean(rst_per$test[,1])
  meanrst_per$test[t,2] <- mean(rst_per$test[,2])
}

