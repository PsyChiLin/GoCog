######### GoStage Result #########
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

# Create Data Frame to store the output
SVM_GoStage <- as.data.frame(matrix(NA,3,5))
colnames(SVM_GoStage) <- c("Type","All","Open","Mid","End")
SVM_GoStage$Type <- c("AllSubj","Dan","Kyu")

############### Parameter ###############
costvalue = 0.001
gammavalue = 0.01
kerneltype = "linear"

################ Allsubj ################ 
set.seed(1)
# equally sampling
dta_3s_Open <- filter(dta_3s, GoStage == "Open")
dta_3s_Mid <- filter(dta_3s, GoStage == "Mid")
dta_3s_End <- filter(dta_3s, GoStage == "End")
# random order
dta_3s_Open <- dta_3s_Open[sample(nrow(dta_3s_Open)),]
dta_3s_Mid  <- dta_3s_Mid[sample(nrow(dta_3s_Mid)),]
dta_3s_End <- dta_3s_End[sample(nrow(dta_3s_End)),]
# bind data
dta_3s_new <- rbind(dta_3s_Open,dta_3s_Mid,dta_3s_End)
row.names(dta_3s_new) <- 1:dim(dta_3s_new)[1]
dta_3s_rst <- rbind(dta_3s_Open,dta_3s_Mid,dta_3s_End)
dta_3s_rst$pred_GoStage <- NA
dta_3s_rst$pred_GoStage <- as.factor(dta_3s_rst$pred_GoStage)
dta_3s_rst$pred_GoStage <- factor(dta_3s_rst$pred_GoStage,levels = c("End","Mid","Open"))
row.names(dta_3s_rst) <- 1:dim(dta_3s_rst)[1]
#head(dta_3s_rst)
# 8 folds
folds <- rep(rep(1:8,3),3)
# 4 columns : all, open, mid, end
rst <- list()
rst$train <- matrix(NA,8,4) # 4 column : all, open, mid, end
rst$test <- matrix(NA,8,4)  # 4 column : all, open, mid, end
for(i in 1:8){
  # Use dta_3s_new
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- dta_3s_new[testIndexes, c(4:12)]
  trainData <- dta_3s_new[-testIndexes, c(4:12)]
  rst_svm <- svm(GoStage~., data = trainData,
    type = "C-classification", cost = costvalue,
    gamma = gammavalue, kernel = kerneltype,
    probability = T)
  # Overall
  y_train_prob_svm <- predict(rst_svm,trainData,probability = T)
  y_train_hat <- predict(rst_svm,trainData)
  y_test_prob_svm <- predict(rst_svm,testData,probability = T)
  y_test_hat <- predict(rst_svm,testData)
  ce_train <- mean(y_train_hat!=trainData$GoStage, na.rm = T)
  ce_test <- mean(y_test_hat!=testData$GoStage, na.rm = T)
  # for 3d plot
  dta_3s_rst[testIndexes,]$pred_GoStage <- y_test_hat
  # 4 column : all, open, mid, end
  rst$train[i,1] <- ce_train 
  rst$test[i,1] <- ce_test
  # Open
  Open_train <- filter(trainData, GoStage == "Open")
  Open_test <- filter(testData, GoStage == "Open")
  y_train_prob_svm <- predict(rst_svm,Open_train,probability = T)
  y_train_hat <- predict(rst_svm,Open_train)
  y_test_prob_svm <- predict(rst_svm,Open_test,probability = T)
  y_test_hat <- predict(rst_svm,Open_test)
  ce_train <- mean(y_train_hat!=Open_train$GoStage)
  ce_test <- mean(y_test_hat!=Open_test$GoStage)
  # 4 column : all, open, mid, end
  rst$train[i,2] <- ce_train
  rst$test[i,2] <- ce_test
  # Mid
  Mid_train <- filter(trainData, GoStage == "Mid")
  Mid_test <- filter(testData, GoStage == "Mid")
  y_train_prob_svm <- predict(rst_svm,Mid_train, probability = T)
  y_train_hat <- predict(rst_svm,Mid_train)
  y_test_prob_svm <- predict(rst_svm,Mid_test, probability = T)
  y_test_hat <- predict(rst_svm,Mid_test)
  ce_train <- mean(y_train_hat != Mid_train $GoStage)
  ce_test <- mean(y_test_hat != Mid_test$GoStage)
  # 4 column : all, open, mid, end
  rst$train[i,3] <- ce_train
  rst$test[i,3] <- ce_test
  # End
  End_train <- filter(trainData, GoStage == "End")
  End_test <- filter(testData, GoStage == "End")
  y_train_prob_svm <- predict(rst_svm,End_train, probability = T)
  y_train_hat <- predict(rst_svm,End_train)
  y_test_prob_svm <- predict(rst_svm,End_test, probability = T)
  y_test_hat <- predict(rst_svm,End_test)
  ce_train <- mean(y_train_hat != End_train$GoStage)
  ce_test <- mean(y_test_hat != End_test$GoStage)
  # 4 column : all, open, mid, end
  rst$train[i,4] <- ce_train
  rst$test[i,4] <- ce_test
}

# 4 column : all, open, mid, end
SVM_GoStage[1,2] <- mean(rst$test[,1]) # 0.2638889
SVM_GoStage[1,3] <- mean(rst$test[,2]) # 0.3333333
SVM_GoStage[1,4] <- mean(rst$test[,3]) # 0.2083333
SVM_GoStage[1,5] <- mean(rst$test[,4]) # 0.25


################ Dan ################
set.seed(1)
# equally sampling
dta_3s_dan_Open <- filter(dta_3s_dan, GoStage == "Open")
dta_3s_dan_Mid <- filter(dta_3s_dan, GoStage == "Mid")
dta_3s_dan_End <- filter(dta_3s_dan, GoStage == "End")
# random order
dta_3s_dan_Open <- dta_3s_dan_Open[sample(nrow(dta_3s_dan_Open)),]
dta_3s_dan_Mid <- dta_3s_dan_Mid[sample(nrow(dta_3s_dan_Mid)),]
dta_3s_dan_End <- dta_3s_dan_End[sample(nrow(dta_3s_dan_End)),]
# bind data
dta_3s_dan <- rbind(dta_3s_dan_Open,dta_3s_dan_Mid,dta_3s_dan_End)
folds <- rep(rep(1:8,2),3)

rst <- list()
rst$train <- matrix(NA,8,4) # 4 column : all, open, mid, end
rst$test <- matrix(NA,8,4)  # 4 column : all, open, mid, end

for(i in 1:8){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- dta_3s_dan[testIndexes, c(4:12)]
  trainData <- dta_3s_dan[-testIndexes, c(4:12)]
   rst_svm <- svm(GoStage~., data = trainData,
    type = "C-classification", cost = costvalue,
    gamma = gammavalue, kernel = kerneltype,
    probability = T)
  y_train_prob_svm <- predict(rst_svm,trainData, probability = T)
  y_train_hat <- predict(rst_svm,trainData)
  y_test_prob_svm <- predict(rst_svm,testData , probability = T)
  y_test_hat <- predict(rst_svm,testData)
  ce_train <- mean(y_train_hat!=trainData$GoStage, na.rm = T)
  ce_test <- mean(y_test_hat!=testData$GoStage, na.rm = T)
  rst$train[i,1] <- ce_train
  rst$test[i,1] <- ce_test
  # Open
  Open_train <- filter(trainData, GoStage == "Open")
  Open_test <- filter(testData, GoStage == "Open")
  y_train_prob_svm <- predict(rst_svm,Open_train, probability = T)
  y_train_hat <- predict(rst_svm,Open_train)
  y_test_prob_svm <- predict(rst_svm,Open_test , probability = T)
  y_test_hat <- predict(rst_svm,Open_test)
  ce_train <- mean(y_train_hat!=Open_train$GoStage)
  ce_test <- mean(y_test_hat!=Open_test$GoStage)
  rst$train[i,2] <- ce_train # 4 column : all, open, mid, end
  rst$test[i,2] <- ce_test   # 4 column : all, open, mid, end
  # Mid
  Mid_train <- filter(trainData, GoStage == "Mid")
  Mid_test <- filter(testData, GoStage == "Mid")
  y_train_prob_svm <- predict(rst_svm,Mid_train, probability = T)
  y_train_hat <- predict(rst_svm,Mid_train)
  y_test_prob_svm <- predict(rst_svm,Mid_test, probability = T)
  y_test_hat <- predict(rst_svm,Mid_test)
  ce_train <- mean(y_train_hat != Mid_train $GoStage)
  ce_test <- mean(y_test_hat != Mid_test$GoStage)
  rst$train[i,3] <- ce_train # 4 column : all, open, mid, end
  rst$test[i,3] <- ce_test   # 4 column : all, open, mid, end

  End_train <- filter(trainData, GoStage == "End")
  End_test <- filter(testData, GoStage == "End")
  y_train_prob_svm <- predict(rst_svm,End_train, probability = T)
  y_train_hat <- predict(rst_svm,End_train)
  y_test_prob_svm <- predict(rst_svm,End_test, probability = T)
  y_test_hat <- predict(rst_svm,End_test)
  ce_train <- mean(y_train_hat != End_train $GoStage)
  ce_test <- mean(y_test_hat != End_test$GoStage)
  rst$train[i,4] <- ce_train # 4 column : all, open, mid, end
  rst$test[i,4] <- ce_test   # 4 column : all, open, mid, end
}

# 4 column : all, open, mid, end
SVM_GoStage[2,2] <- mean(rst$test[,1])
SVM_GoStage[2,3] <- mean(rst$test[,2])
SVM_GoStage[2,4] <- mean(rst$test[,3])
SVM_GoStage[2,5] <- mean(rst$test[,4])


################ Kyu ################
set.seed(1)
# equally sampling
dta_3s_kyu_Open <- filter(dta_3s_kyu, GoStage == "Open")
dta_3s_kyu_Mid <- filter(dta_3s_kyu, GoStage == "Mid")
dta_3s_kyu_End <- filter(dta_3s_kyu, GoStage == "End")
# random order
dta_3s_kyu_Open <- dta_3s_kyu_Open[sample(nrow(dta_3s_kyu_Open)),]
dta_3s_kyu_Mid <- dta_3s_kyu_Mid[sample(nrow(dta_3s_kyu_Mid)),]
dta_3s_kyu_End <- dta_3s_kyu_End[sample(nrow(dta_3s_kyu_End)),]
# bind data
dta_3s_kyu <- rbind(dta_3s_kyu_Open,dta_3s_kyu_Mid,dta_3s_kyu_End)
folds <- rep(rep(1:8,1),3)

rst <- list()
rst$train <- matrix(NA,8,4) # 4 column : all, open, mid, end
rst$test <- matrix(NA,8,4)  # 4 column : all, open, mid, end

for(i in 1:8){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- dta_3s_kyu[testIndexes, c(4:12)]
  trainData <- dta_3s_kyu[-testIndexes, c(4:12)]
   rst_svm <- svm(GoStage~., data = trainData,
    type = "C-classification", cost = costvalue,
    gamma = gammavalue, kernel = kerneltype,
    probability = T)
  y_train_prob_svm <- predict(rst_svm,trainData, probability = T)
  y_train_hat <- predict(rst_svm,trainData)
  y_test_prob_svm <- predict(rst_svm,testData , probability = T)
  y_test_hat <- predict(rst_svm,testData)

  ce_train <- mean(y_train_hat!=trainData$GoStage, na.rm = T)
  ce_test <- mean(y_test_hat!=testData$GoStage, na.rm = T)
  rst$train[i,1] <- ce_train
  rst$test[i,1] <- ce_test
  # Open
  Open_train <- filter(trainData, GoStage == "Open")
  Open_test <- filter(testData, GoStage == "Open")
  y_train_prob_svm <- predict(rst_svm,Open_train, probability = T)
  y_train_hat <- predict(rst_svm,Open_train)
  y_test_prob_svm <- predict(rst_svm,Open_test , probability = T)
  y_test_hat <- predict(rst_svm,Open_test)
  ce_train <- mean(y_train_hat!=Open_train$GoStage)
  ce_test <- mean(y_test_hat!=Open_test$GoStage)
  rst$train[i,2] <- ce_train # 4 column : all, open, mid, end
  rst$test[i,2] <- ce_test   # 4 column : all, open, mid, end
  # Mid
  Mid_train <- filter(trainData, GoStage == "Mid")
  Mid_test <- filter(testData, GoStage == "Mid")
  y_train_prob_svm <- predict(rst_svm,Mid_train, probability = T)
  y_train_hat <- predict(rst_svm,Mid_train)
  y_test_prob_svm <- predict(rst_svm,Mid_test, probability = T)
  y_test_hat <- predict(rst_svm,Mid_test)
  ce_train <- mean(y_train_hat != Mid_train $GoStage)
  ce_test <- mean(y_test_hat != Mid_test$GoStage)
  rst$train[i,3] <- ce_train # 4 column : all, open, mid, end
  rst$test[i,3] <- ce_test   # 4 column : all, open, mid, end

  End_train <- filter(trainData, GoStage == "End")
  End_test <- filter(testData, GoStage == "End")
  y_train_prob_svm <- predict(rst_svm,End_train, probability = T)
  y_train_hat <- predict(rst_svm,End_train)
  y_test_prob_svm <- predict(rst_svm,End_test, probability = T)
  y_test_hat <- predict(rst_svm,End_test)
  ce_train <- mean(y_train_hat != End_train $GoStage)
  ce_test <- mean(y_test_hat != End_test$GoStage)
  rst$train[i,4] <- ce_train # 4 column : all, open, mid, end
  rst$test[i,4] <- ce_test   # 4 column : all, open, mid, end
}

# 4 column : all, open, mid, end
SVM_GoStage[3,2] <- mean(rst$test[,1])
SVM_GoStage[3,3] <- mean(rst$test[,2])
SVM_GoStage[3,4] <- mean(rst$test[,3])
SVM_GoStage[3,5] <- mean(rst$test[,4])

SVM_GoStage
saveRDS(SVM_GoStage,file = "Output/SVM/GoStage_bACCbRT_SVM_Rst.Rdata")

#head(dta_3s_rst)
#saveRDS(dta_3s_rst,file = "../GoCogdata/dta_3s_rst.Rdata")