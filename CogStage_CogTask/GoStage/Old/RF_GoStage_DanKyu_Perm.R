library(randomForest)
library(dplyr)
library(reshape2)
library(e1071)
library(pROC)

############## Bootstrapping : Dan ############

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

set.seed(1)
meanrst_perm <- list()
meanrst_perm$train <- matrix(NA,10000,4) # 4 column : all, open, mid, end
meanrst_perm$test <- matrix(NA,10000,4)  # 4 column : all, open, mid, end

for (t in 1:10000){
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
    rst_forests <- randomForest(GoStage~., data = trainData,
                                mtry = 7, 
                                ntree = 1000, 
                                nodesize = 20,
                                importance = T)
    y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
    y_train_hat <- predict(rst_forests,trainData,type="response")
    y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
    y_test_hat <- predict(rst_forests,testData,type="response")
    ce_train <- mean(y_train_hat!=trainData$GoStage, na.rm = T)
    ce_test <- mean(y_test_hat!=testData$GoStage, na.rm = T)
    rst_perm$train[i,1] <- ce_train 
    rst_perm$test[i,1] <- ce_test
    
    Open_train <- filter(trainData, GoStage == "Open")
    Open_test <- filter(testData, GoStage == "Open")
    y_train_prob_tree <- predict(rst_forests,Open_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,Open_train,type="response")
    y_test_prob_tree <- predict(rst_forests,Open_test ,type="prob")[,2]
    y_test_hat <- predict(rst_forests,Open_test,type="response")
    ce_train <- mean(y_train_hat!=Open_train$GoStage)
    ce_test <- mean(y_test_hat!=Open_test$GoStage)
    rst_perm$train[i,2] <- ce_train # 4 column : all, open, mid, end
    rst_perm$test[i,2] <- ce_test   # 4 column : all, open, mid, end
    
    Mid_train <- filter(trainData, GoStage == "Mid")
    Mid_test <- filter(testData, GoStage == "Mid")
    y_train_prob_tree <- predict(rst_forests,Mid_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,Mid_train,type="response")
    y_test_prob_tree <- predict(rst_forests,Mid_test,type="prob")[,2]
    y_test_hat <- predict(rst_forests,Mid_test,type="response")
    ce_train <- mean(y_train_hat != Mid_train $GoStage)
    ce_test <- mean(y_test_hat != Mid_test$GoStage)
    rst_perm$train[i,3] <- ce_train # 4 column : all, open, mid, end
    rst_perm$test[i,3] <- ce_test   # 4 column : all, open, mid, end
    
    End_train <- filter(trainData, GoStage == "End")
    End_test <- filter(testData, GoStage == "End")
    y_train_prob_tree <- predict(rst_forests,End_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,End_train,type="response")
    y_test_prob_tree <- predict(rst_forests,End_test,type="prob")[,2]
    y_test_hat <- predict(rst_forests,End_test,type="response")
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

saveRDS(meanrst_perm,file = "../GoCogdata/GoStage_bACCbRT_RF_10000_Ex_Dan_Perm.Rdata")


############## Bootstrapping : Kyu ############

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

set.seed(1)
meanrst_perm <- list()
meanrst_perm$train <- matrix(NA,10000,4) # 4 column : all, open, mid, end
meanrst_perm$test <- matrix(NA,10000,4)  # 4 column : all, open, mid, end

for (t in 1:10000){
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
  folds <- rep(rep(1:8,2),3)
  
  rst_perm <- list()
  rst_perm$train <- matrix(NA,8,4) # 4 column : all, open, mid, end
  rst_perm$test <- matrix(NA,8,4)  # 4 column : all, open, mid, end
  
  for(i in 1:8){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dta_3s_kyu_perm[testIndexes, c(4:12)]
    trainData <- dta_3s_kyu_perm[-testIndexes, c(4:12)]
    rst_forests <- randomForest(GoStage~., data = trainData,
                                mtry = 7, 
                                ntree = 1000, 
                                nodesize = 20,
                                importance = T)
    y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
    y_train_hat <- predict(rst_forests,trainData,type="response")
    y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
    y_test_hat <- predict(rst_forests,testData,type="response")
    ce_train <- mean(y_train_hat!=trainData$GoStage, na.rm = T)
    ce_test <- mean(y_test_hat!=testData$GoStage, na.rm = T)
    rst_perm$train[i,1] <- ce_train 
    rst_perm$test[i,1] <- ce_test
    
    Open_train <- filter(trainData, GoStage == "Open")
    Open_test <- filter(testData, GoStage == "Open")
    y_train_prob_tree <- predict(rst_forests,Open_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,Open_train,type="response")
    y_test_prob_tree <- predict(rst_forests,Open_test ,type="prob")[,2]
    y_test_hat <- predict(rst_forests,Open_test,type="response")
    ce_train <- mean(y_train_hat!=Open_train$GoStage)
    ce_test <- mean(y_test_hat!=Open_test$GoStage)
    rst_perm$train[i,2] <- ce_train # 4 column : all, open, mid, end
    rst_perm$test[i,2] <- ce_test   # 4 column : all, open, mid, end
    
    Mid_train <- filter(trainData, GoStage == "Mid")
    Mid_test <- filter(testData, GoStage == "Mid")
    y_train_prob_tree <- predict(rst_forests,Mid_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,Mid_train,type="response")
    y_test_prob_tree <- predict(rst_forests,Mid_test,type="prob")[,2]
    y_test_hat <- predict(rst_forests,Mid_test,type="response")
    ce_train <- mean(y_train_hat != Mid_train $GoStage)
    ce_test <- mean(y_test_hat != Mid_test$GoStage)
    rst_perm$train[i,3] <- ce_train # 4 column : all, open, mid, end
    rst_perm$test[i,3] <- ce_test   # 4 column : all, open, mid, end
    
    End_train <- filter(trainData, GoStage == "End")
    End_test <- filter(testData, GoStage == "End")
    y_train_prob_tree <- predict(rst_forests,End_train,type="prob")[,2]
    y_train_hat <- predict(rst_forests,End_train,type="response")
    y_test_prob_tree <- predict(rst_forests,End_test,type="prob")[,2]
    y_test_hat <- predict(rst_forests,End_test,type="response")
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

saveRDS(meanrst_perm,file = "../GoCogdata/GoStage_bACCbRT_RF_10000_Ex_kyu_Perm.Rdata")
