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
RF_GoStage <- as.data.frame(matrix(NA,3,5))
colnames(RF_GoStage) <- c("Type","All","Open","Mid","End")
RF_GoStage$Type <- c("AllSubj","Dan","Kyu")

############### Parameter ###############
allmtry <- 7 
allnodesize <- 20
Danmtry <- 7 
Dannodesize <- 15
Kyumtry <- 7 
Kyunodesize <- 3

overallntree <-  1000

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
  rst_forests <- randomForest(GoStage~., data = trainData,
                              mtry = allmtry , 
                              ntree = overallntree, 
                              nodesize = allnodesize,
                              importance = T)
  # Overall
  y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
  y_train_hat <- predict(rst_forests,trainData,type="response")
  y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
  y_test_hat <- predict(rst_forests,testData,type="response")
  ce_train <- mean(y_train_hat!=trainData$GoStage, na.rm = T)
  ce_test <- mean(y_test_hat!=testData$GoStage, na.rm = T)
  # 4 column : all, open, mid, end
  rst$train[i,1] <- ce_train 
  rst$test[i,1] <- ce_test
  # Open
  Open_train <- filter(trainData, GoStage == "Open")
  Open_test <- filter(testData, GoStage == "Open")
  y_train_prob_tree <- predict(rst_forests,Open_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Open_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Open_test ,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Open_test,type="response")
  ce_train <- mean(y_train_hat!=Open_train$GoStage)
  ce_test <- mean(y_test_hat!=Open_test$GoStage)
  # 4 column : all, open, mid, end
  rst$train[i,2] <- ce_train
  rst$test[i,2] <- ce_test
  # Mid
  Mid_train <- filter(trainData, GoStage == "Mid")
  Mid_test <- filter(testData, GoStage == "Mid")
  y_train_prob_tree <- predict(rst_forests,Mid_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Mid_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Mid_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Mid_test,type="response")
  ce_train <- mean(y_train_hat != Mid_train $GoStage)
  ce_test <- mean(y_test_hat != Mid_test$GoStage)
  # 4 column : all, open, mid, end
  rst$train[i,3] <- ce_train
  rst$test[i,3] <- ce_test
  # End
  End_train <- filter(trainData, GoStage == "End")
  End_test <- filter(testData, GoStage == "End")
  y_train_prob_tree <- predict(rst_forests,End_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,End_train,type="response")
  y_test_prob_tree <- predict(rst_forests,End_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,End_test,type="response")
  ce_train <- mean(y_train_hat != End_train $GoStage)
  ce_test <- mean(y_test_hat != End_test$GoStage)
  # 4 column : all, open, mid, end
  rst$train[i,4] <- ce_train
  rst$test[i,4] <- ce_test
}

# 4 column : all, open, mid, end
RF_GoStage[1,2] <- mean(rst$test[,1]) # 0.2638889
RF_GoStage[1,3] <- mean(rst$test[,2]) # 0.3333333
RF_GoStage[1,4] <- mean(rst$test[,3]) # 0.2083333
RF_GoStage[1,5] <- mean(rst$test[,4]) # 0.25

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
  rst_forests <- randomForest(GoStage~., data = trainData,
                              mtry = Danmtry, 
                              ntree = overallntree, 
                              nodesize = Dannodesize,
                              importance = T)
  y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
  y_train_hat <- predict(rst_forests,trainData,type="response")
  y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
  y_test_hat <- predict(rst_forests,testData,type="response")
  ce_train <- mean(y_train_hat!=trainData$GoStage, na.rm = T)
  ce_test <- mean(y_test_hat!=testData$GoStage, na.rm = T)
  rst$train[i,1] <- ce_train 
  rst$test[i,1] <- ce_test
  # Open   
  Open_train <- filter(trainData, GoStage == "Open")
  Open_test <- filter(testData, GoStage == "Open")
  y_train_prob_tree <- predict(rst_forests,Open_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Open_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Open_test ,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Open_test,type="response")
  ce_train <- mean(y_train_hat!=Open_train$GoStage)
  ce_test <- mean(y_test_hat!=Open_test$GoStage)
  rst$train[i,2] <- ce_train # 4 column : all, open, mid, end
  rst$test[i,2] <- ce_test   # 4 column : all, open, mid, end
  # Mid  
  Mid_train <- filter(trainData, GoStage == "Mid")
  Mid_test <- filter(testData, GoStage == "Mid")
  y_train_prob_tree <- predict(rst_forests,Mid_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Mid_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Mid_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Mid_test,type="response")
  ce_train <- mean(y_train_hat != Mid_train $GoStage)
  ce_test <- mean(y_test_hat != Mid_test$GoStage)
  rst$train[i,3] <- ce_train # 4 column : all, open, mid, end
  rst$test[i,3] <- ce_test   # 4 column : all, open, mid, end
  
  End_train <- filter(trainData, GoStage == "End")
  End_test <- filter(testData, GoStage == "End")
  y_train_prob_tree <- predict(rst_forests,End_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,End_train,type="response")
  y_test_prob_tree <- predict(rst_forests,End_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,End_test,type="response")
  ce_train <- mean(y_train_hat != End_train $GoStage)
  ce_test <- mean(y_test_hat != End_test$GoStage)
  rst$train[i,4] <- ce_train # 4 column : all, open, mid, end
  rst$test[i,4] <- ce_test   # 4 column : all, open, mid, end
}

# 4 column : all, open, mid, end
RF_GoStage[2,2] <- mean(rst$test[,1])
RF_GoStage[2,3] <- mean(rst$test[,2])
RF_GoStage[2,4] <- mean(rst$test[,3])
RF_GoStage[2,5] <- mean(rst$test[,4])


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
  rst_forests <- randomForest(GoStage~., data = trainData,
                              mtry = Kyumtry, 
                              ntree = overallntree, 
                              nodesize = Kyunodesize,
                              importance = T)
  y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
  y_train_hat <- predict(rst_forests,trainData,type="response")
  y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
  y_test_hat <- predict(rst_forests,testData,type="response")
  ce_train <- mean(y_train_hat!=trainData$GoStage, na.rm = T)
  ce_test <- mean(y_test_hat!=testData$GoStage, na.rm = T)
  rst$train[i,1] <- ce_train 
  rst$test[i,1] <- ce_test
  # Open   
  Open_train <- filter(trainData, GoStage == "Open")
  Open_test <- filter(testData, GoStage == "Open")
  y_train_prob_tree <- predict(rst_forests,Open_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Open_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Open_test ,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Open_test,type="response")
  ce_train <- mean(y_train_hat!=Open_train$GoStage)
  ce_test <- mean(y_test_hat!=Open_test$GoStage)
  rst$train[i,2] <- ce_train # 4 column : all, open, mid, end
  rst$test[i,2] <- ce_test   # 4 column : all, open, mid, end
  # Mid  
  Mid_train <- filter(trainData, GoStage == "Mid")
  Mid_test <- filter(testData, GoStage == "Mid")
  y_train_prob_tree <- predict(rst_forests,Mid_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Mid_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Mid_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Mid_test,type="response")
  ce_train <- mean(y_train_hat != Mid_train $GoStage)
  ce_test <- mean(y_test_hat != Mid_test$GoStage)
  rst$train[i,3] <- ce_train # 4 column : all, open, mid, end
  rst$test[i,3] <- ce_test   # 4 column : all, open, mid, end
  
  End_train <- filter(trainData, GoStage == "End")
  End_test <- filter(testData, GoStage == "End")
  y_train_prob_tree <- predict(rst_forests,End_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,End_train,type="response")
  y_test_prob_tree <- predict(rst_forests,End_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,End_test,type="response")
  ce_train <- mean(y_train_hat != End_train $GoStage)
  ce_test <- mean(y_test_hat != End_test$GoStage)
  rst$train[i,4] <- ce_train # 4 column : all, open, mid, end
  rst$test[i,4] <- ce_test   # 4 column : all, open, mid, end
}

# 4 column : all, open, mid, end
RF_GoStage[3,2] <- mean(rst$test[,1])
RF_GoStage[3,3] <- mean(rst$test[,2])
RF_GoStage[3,4] <- mean(rst$test[,3])
RF_GoStage[3,5] <- mean(rst$test[,4])

RF_GoStage








  