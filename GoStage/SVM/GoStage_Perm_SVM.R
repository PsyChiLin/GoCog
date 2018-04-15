######### GoStage Permutation #########
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
dta_bothacc <- dcast(dta,Subj+Age+SubjGroup+GoStage~CogTask,value.var = "Both_ACC")
colnames(dta_bothacc)[5:8] <- c("Calc_Both_ACC","None_Both_ACC","Reas_Both_ACC","Spat_Both_ACC")
dta_bothrt <- dcast(dta,Subj+Age+SubjGroup+GoStage~CogTask,value.var = "Both_RT")
colnames(dta_bothrt)[5:8] <- c("Calc_Both_RT","None_Both_RT","Reas_Both_RT","Spat_Both_RT")
dta_3s <- full_join(dta_bothacc,dta_bothrt)
dta_3s$Subj <- as.factor(dta_3s$Subj)
dta_3s_dan <- filter(dta_3s, SubjGroup == "Dan")
dta_3s_kyu <- filter(dta_3s, SubjGroup == "Kyu")

############### Parameter ###############
costvalue = 0.001
gammavalue = 0.01
kerneltype = "linear"

Perm_time <- 10000
############### All Permutation ###############
meanrst_perm <- list()
meanrst_perm$train <- matrix(NA,Perm_time,4) # 4 column : all, open, mid, end
meanrst_perm$test <- matrix(NA,Perm_time,4)  # 4 column : all, open, mid, end

for (t in 1:Perm_time){
  print(t)
  set.seed(t)
  dta_3s_perm <- dta_3s
  # Permutation
  dta_3s_perm$GoStage <- sample(dta_3s_perm$GoStage) 
  # equally sampling
  dta_3s_Open <- filter(dta_3s_perm, GoStage == "Open")
  dta_3s_Mid <- filter(dta_3s_perm, GoStage == "Mid")
  dta_3s_End <- filter(dta_3s_perm, GoStage == "End")
  # bind data
  dta_3s_perm <- rbind(dta_3s_Open,dta_3s_Mid,dta_3s_End)
  # 8 folds
  folds <- rep(rep(1:8,3),3)
  # 4 columns : all, open, mid, end
  rst_perm <- list()
  rst_perm$train <- matrix(NA,8,4) # 4 column : all, open, mid, end
  rst_perm$test <- matrix(NA,8,4)  # 4 column : all, open, mid, end
  
  for(i in 1:8){
    # Use dta_3s_perm
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dta_3s_perm[testIndexes, c(4:12)]
    trainData <- dta_3s_perm[-testIndexes, c(4:12)]
    rst_svm <- svm(GoStage~., data = trainData,
      type = "C-classification", cost = costvalue,
      gamma = gammavalue, kernel = kerneltype,
      probability = T)
    y_train_prob_tree <- predict(rst_svm,trainData,probability = T)
    y_train_hat <- predict(rst_svm,trainData)
    y_test_prob_tree <- predict(rst_svm,testData ,probability = T)
    y_test_hat <- predict(rst_svm,testData)
    ce_train <- mean(y_train_hat!=trainData$GoStage, na.rm = T)
    ce_test <- mean(y_test_hat!=testData$GoStage, na.rm = T)
    
    rst_perm$train[i,1] <- ce_train 
    rst_perm$test[i,1] <- ce_test
    
    Open_train <- filter(trainData, GoStage == "Open")
    Open_test <- filter(testData, GoStage == "Open")
    y_train_prob_tree <- predict(rst_svm,Open_train,probability = T)
    y_train_hat <- predict(rst_svm,Open_train)
    y_test_prob_tree <- predict(rst_svm,Open_test ,probability = T)
    y_test_hat <- predict(rst_svm,Open_test)
    ce_train <- mean(y_train_hat!=Open_train$GoStage)
    ce_test <- mean(y_test_hat!=Open_test$GoStage)
    rst_perm$train[i,2] <- ce_train # 4 column : all, open, mid, end
    rst_perm$test[i,2] <- ce_test   # 4 column : all, open, mid, end
    
    Mid_train <- filter(trainData, GoStage == "Mid")
    Mid_test <- filter(testData, GoStage == "Mid")
    y_train_prob_tree <- predict(rst_svm,Mid_train,probability = T)
    y_train_hat <- predict(rst_svm,Mid_train)
    y_test_prob_tree <- predict(rst_svm,Mid_test,probability = T)
    y_test_hat <- predict(rst_svm,Mid_test)
    ce_train <- mean(y_train_hat != Mid_train $GoStage)
    ce_test <- mean(y_test_hat != Mid_test$GoStage)
    rst_perm$train[i,3] <- ce_train # 4 column : all, open, mid, end
    rst_perm$test[i,3] <- ce_test   # 4 column : all, open, mid, end
    
    End_train <- filter(trainData, GoStage == "End")
    End_test <- filter(testData, GoStage == "End")
    y_train_prob_tree <- predict(rst_svm,End_train,probability = T)
    y_train_hat <- predict(rst_svm,End_train)
    y_test_prob_tree <- predict(rst_svm,End_test,probability = T)
    y_test_hat <- predict(rst_svm,End_test)
    ce_train <- mean(y_train_hat != End_train $GoStage)
    ce_test <- mean(y_test_hat != End_test$GoStage)
    rst_perm$train[i,4] <- ce_train # 4 column : all, open, mid, end
    rst_perm$test[i,4] <- ce_test   # 4 column : all, open, mid, end
    
  }
  # 4 column : all, open, mid, end
  meanrst_perm$train[t,1] <- mean(rst_perm$train[,1])
  meanrst_perm$test[t,1] <- mean(rst_perm$test[,1])
  meanrst_perm$train[t,2] <- mean(rst_perm$train[,2])
  meanrst_perm$test[t,2] <- mean(rst_perm$test[,2])
  meanrst_perm$train[t,3] <- mean(rst_perm$train[,3])
  meanrst_perm$test[t,3] <- mean(rst_perm$test[,3])
  meanrst_perm$train[t,4] <- mean(rst_perm$train[,4])
  meanrst_perm$test[t,4] <- mean(rst_perm$test[,4])
  
}
# 4 column : all, open, mid, end
quantile(meanrst_perm$test[,1],c(.025,.975),na.rm = T)
quantile(meanrst_perm$test[,2],c(.025,.975),na.rm = T) 
quantile(meanrst_perm$test[,3],c(.025,.975),na.rm = T) 
quantile(meanrst_perm$test[,4],c(.025,.975),na.rm = T) 

saveRDS(meanrst_perm,file = "Output/SVM/GoStage_bACCbRT_SVM_All_Perm10000.Rdata")

############### Dan Permutation ###############

set.seed(1)
meanrst_perm <- list()
meanrst_perm$train <- matrix(NA,Perm_time,4) # 4 column : all, open, mid, end
meanrst_perm$test <- matrix(NA,Perm_time,4)  # 4 column : all, open, mid, end

for (t in 1:Perm_time){
  print(t)
  set.seed(t)
  dta_3s_dan_perm <- dta_3s_dan
  # Permu
  dta_3s_dan_perm$GoStage <- sample(dta_3s_dan_perm$GoStage)
  # equally sampling
  dta_3s_dan_perm_Open <- filter(dta_3s_dan_perm, GoStage == "Open")
  dta_3s_dan_perm_Mid <- filter(dta_3s_dan_perm, GoStage == "Mid")
  dta_3s_dan_perm_End <- filter(dta_3s_dan_perm, GoStage == "End")
  # random order
  dta_3s_dan_perm_Open <- dta_3s_dan_perm_Open[sample(nrow(dta_3s_dan_perm_Open)),]
  dta_3s_dan_perm_Mid  <- dta_3s_dan_perm_Mid[sample(nrow(dta_3s_dan_perm_Mid)),]
  dta_3s_dan_perm_End <- dta_3s_dan_perm_End[sample(nrow(dta_3s_dan_perm_End)),]
  # bind data
  dta_3s_dan_perm <- rbind(dta_3s_dan_perm_Open,dta_3s_dan_perm_Mid,dta_3s_dan_perm_End)
  folds <- rep(rep(1:8,2),3)
  
  rst_perm <- list()
  rst_perm$train <- matrix(NA,8,4) # 4 column : all, open, mid, end
  rst_perm$test <- matrix(NA,8,4)  # 4 column : all, open, mid, end
  
  for(i in 1:8){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dta_3s_dan_perm[testIndexes, c(4:12)]
    trainData <- dta_3s_dan_perm[-testIndexes, c(4:12)]
    rst_svm <- svm(GoStage~., data = trainData,
      type = "C-classification", cost = costvalue,
      gamma = gammavalue, kernel = kerneltype,
      probability = T)
    y_train_prob_tree <- predict(rst_svm,trainData,probability = T)
    y_train_hat <- predict(rst_svm,trainData)
    y_test_prob_tree <- predict(rst_svm,testData ,probability = T)
    y_test_hat <- predict(rst_svm,testData)
    ce_train <- mean(y_train_hat!=trainData$GoStage, na.rm = T)
    ce_test <- mean(y_test_hat!=testData$GoStage, na.rm = T)
    rst_perm$train[i,1] <- ce_train 
    rst_perm$test[i,1] <- ce_test
    
    Open_train <- filter(trainData, GoStage == "Open")
    Open_test <- filter(testData, GoStage == "Open")
    y_train_prob_tree <- predict(rst_svm,Open_train,probability = T)
    y_train_hat <- predict(rst_svm,Open_train)
    y_test_prob_tree <- predict(rst_svm,Open_test ,probability = T)
    y_test_hat <- predict(rst_svm,Open_test)
    ce_train <- mean(y_train_hat!=Open_train$GoStage)
    ce_test <- mean(y_test_hat!=Open_test$GoStage)
    rst_perm$train[i,2] <- ce_train # 4 column : all, open, mid, end
    rst_perm$test[i,2] <- ce_test   # 4 column : all, open, mid, end
    
    Mid_train <- filter(trainData, GoStage == "Mid")
    Mid_test <- filter(testData, GoStage == "Mid")
    y_train_prob_tree <- predict(rst_svm,Mid_train,probability = T)
    y_train_hat <- predict(rst_svm,Mid_train)
    y_test_prob_tree <- predict(rst_svm,Mid_test,probability = T)
    y_test_hat <- predict(rst_svm,Mid_test)
    ce_train <- mean(y_train_hat != Mid_train $GoStage)
    ce_test <- mean(y_test_hat != Mid_test$GoStage)
    rst_perm$train[i,3] <- ce_train # 4 column : all, open, mid, end
    rst_perm$test[i,3] <- ce_test   # 4 column : all, open, mid, end
    
    End_train <- filter(trainData, GoStage == "End")
    End_test <- filter(testData, GoStage == "End")
    y_train_prob_tree <- predict(rst_svm,End_train,probability = T)
    y_train_hat <- predict(rst_svm,End_train)
    y_test_prob_tree <- predict(rst_svm,End_test,probability = T)
    y_test_hat <- predict(rst_svm,End_test)
    ce_train <- mean(y_train_hat != End_train $GoStage)
    ce_test <- mean(y_test_hat != End_test$GoStage)
    rst_perm$train[i,4] <- ce_train # 4 column : all, open, mid, end
    rst_perm$test[i,4] <- ce_test   # 4 column : all, open, mid, end
    
  }
  # 4 column : all, open, mid, end
  meanrst_perm$train[t,1] <- mean(rst_perm$train[,1])
  meanrst_perm$test[t,1] <- mean(rst_perm$test[,1])
  meanrst_perm$train[t,2] <- mean(rst_perm$train[,2])
  meanrst_perm$test[t,2] <- mean(rst_perm$test[,2])
  meanrst_perm$train[t,3] <- mean(rst_perm$train[,3])
  meanrst_perm$test[t,3] <- mean(rst_perm$test[,3])
  meanrst_perm$train[t,4] <- mean(rst_perm$train[,4])
  meanrst_perm$test[t,4] <- mean(rst_perm$test[,4])
  
}
# 4 column : all, open, mid, end
quantile(meanrst_perm$test[,1],c(.025,.975),na.rm = T)
quantile(meanrst_perm$test[,2],c(.025,.975),na.rm = T) 
quantile(meanrst_perm$test[,3],c(.025,.975),na.rm = T) 
quantile(meanrst_perm$test[,4],c(.025,.975),na.rm = T) 

saveRDS(meanrst_perm,file = "Output/SVM/GoStage_bACCbRT_SVM_Dan_Perm10000.Rdata")


############### Kyu Permutation ###############
set.seed(1)
meanrst_perm <- list()
meanrst_perm$train <- matrix(NA,Perm_time,4) # 4 column : all, open, mid, end
meanrst_perm$test <- matrix(NA,Perm_time,4)  # 4 column : all, open, mid, end

for (t in 1:Perm_time){
  print(t)
  set.seed(t)
  dta_3s_kyu_perm <- dta_3s_kyu
  # Permu
  dta_3s_kyu_perm$GoStage <- sample(dta_3s_kyu_perm$GoStage)
  # equally sampling
  dta_3s_kyu_perm_Open <- filter(dta_3s_kyu_perm, GoStage == "Open")
  dta_3s_kyu_perm_Mid <- filter(dta_3s_kyu_perm, GoStage == "Mid")
  dta_3s_kyu_perm_End <- filter(dta_3s_kyu_perm, GoStage == "End")
  # random order
  dta_3s_kyu_perm_Open <- dta_3s_kyu_perm_Open[sample(nrow(dta_3s_kyu_perm_Open)),]
  dta_3s_kyu_perm_Mid  <- dta_3s_kyu_perm_Mid[sample(nrow(dta_3s_kyu_perm_Mid)),]
  dta_3s_kyu_perm_End <- dta_3s_kyu_perm_End[sample(nrow(dta_3s_kyu_perm_End)),]
  # bind data
  dta_3s_kyu_perm <- rbind(dta_3s_kyu_perm_Open,dta_3s_kyu_perm_Mid,dta_3s_kyu_perm_End)
  folds <- rep(rep(1:8,1),3)
  
  rst_perm <- list()
  rst_perm$train <- matrix(NA,8,4) # 4 column : all, open, mid, end
  rst_perm$test <- matrix(NA,8,4)  # 4 column : all, open, mid, end
  
  for(i in 1:8){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dta_3s_kyu_perm[testIndexes, c(4:12)]
    trainData <- dta_3s_kyu_perm[-testIndexes, c(4:12)]
    rst_svm <- svm(GoStage~., data = trainData,
      type = "C-classification", cost = costvalue,
      gamma = gammavalue, kernel = kerneltype,
      probability = T)
    y_train_prob_tree <- predict(rst_svm,trainData,probability = T)
    y_train_hat <- predict(rst_svm,trainData)
    y_test_prob_tree <- predict(rst_svm,testData ,probability = T)
    y_test_hat <- predict(rst_svm,testData)
    ce_train <- mean(y_train_hat!=trainData$GoStage, na.rm = T)
    ce_test <- mean(y_test_hat!=testData$GoStage, na.rm = T)
    rst_perm$train[i,1] <- ce_train 
    rst_perm$test[i,1] <- ce_test
    
    Open_train <- filter(trainData, GoStage == "Open")
    Open_test <- filter(testData, GoStage == "Open")
    y_train_prob_tree <- predict(rst_svm,Open_train,probability = T)
    y_train_hat <- predict(rst_svm,Open_train)
    y_test_prob_tree <- predict(rst_svm,Open_test ,probability = T)
    y_test_hat <- predict(rst_svm,Open_test)
    ce_train <- mean(y_train_hat!=Open_train$GoStage)
    ce_test <- mean(y_test_hat!=Open_test$GoStage)
    rst_perm$train[i,2] <- ce_train # 4 column : all, open, mid, end
    rst_perm$test[i,2] <- ce_test   # 4 column : all, open, mid, end
    
    Mid_train <- filter(trainData, GoStage == "Mid")
    Mid_test <- filter(testData, GoStage == "Mid")
    y_train_prob_tree <- predict(rst_svm,Mid_train,probability = T)
    y_train_hat <- predict(rst_svm,Mid_train)
    y_test_prob_tree <- predict(rst_svm,Mid_test,probability = T)
    y_test_hat <- predict(rst_svm,Mid_test)
    ce_train <- mean(y_train_hat != Mid_train $GoStage)
    ce_test <- mean(y_test_hat != Mid_test$GoStage)
    rst_perm$train[i,3] <- ce_train # 4 column : all, open, mid, end
    rst_perm$test[i,3] <- ce_test   # 4 column : all, open, mid, end
    
    End_train <- filter(trainData, GoStage == "End")
    End_test <- filter(testData, GoStage == "End")
    y_train_prob_tree <- predict(rst_svm,End_train,probability = T)
    y_train_hat <- predict(rst_svm,End_train)
    y_test_prob_tree <- predict(rst_svm,End_test,probability = T)
    y_test_hat <- predict(rst_svm,End_test)
    ce_train <- mean(y_train_hat != End_train $GoStage)
    ce_test <- mean(y_test_hat != End_test$GoStage)
    rst_perm$train[i,4] <- ce_train # 4 column : all, open, mid, end
    rst_perm$test[i,4] <- ce_test   # 4 column : all, open, mid, end
    
  }
  # 4 column : all, open, mid, end
  meanrst_perm$train[t,1] <- mean(rst_perm$train[,1])
  meanrst_perm$test[t,1] <- mean(rst_perm$test[,1])
  meanrst_perm$train[t,2] <- mean(rst_perm$train[,2])
  meanrst_perm$test[t,2] <- mean(rst_perm$test[,2])
  meanrst_perm$train[t,3] <- mean(rst_perm$train[,3])
  meanrst_perm$test[t,3] <- mean(rst_perm$test[,3])
  meanrst_perm$train[t,4] <- mean(rst_perm$train[,4])
  meanrst_perm$test[t,4] <- mean(rst_perm$test[,4])
  
}
# 4 column : all, open, mid, end
quantile(meanrst_perm$test[,1],c(.025,.975),na.rm = T)
quantile(meanrst_perm$test[,2],c(.025,.975),na.rm = T) 
quantile(meanrst_perm$test[,3],c(.025,.975),na.rm = T) 
quantile(meanrst_perm$test[,4],c(.025,.975),na.rm = T) 

saveRDS(meanrst_perm,file = "Output/SVM/GoStage_bACCbRT_SVM_Kyu_Perm10000.Rdata")

