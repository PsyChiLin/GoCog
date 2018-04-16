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
allmtry <- 2
allnodesize <- 20
danmtry <- 2
dannodesize <- 10
kyumtry <- 3
kyunodesize <- 5

overallntree <-  1000

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
# for 3d plot
dta_4c_rst <- rbind(dta_4c_Calc,dta_4c_Reas,dta_4c_Spat,dta_4c_None)
dta_4c_rst$pred_CogTask <- NA
dta_4c_rst$pred_CogTask <- as.factor(dta_4c_rs$pred_CogTask)
dta_4c_rst$pred_CogTask <- factor(dta_4c_rst$pred_CogTask,levels = c("Calc","None","Reas","Spat"))
row.names(dta_4c_rst) <- 1:dim(dta_4c_rst)[1]
# 8 folds
folds <- rep(rep(1:8,3),4)

rst <- list()
rst$train <- matrix(NA,8,5)
rst$test <- matrix(NA,8,5)
for(i in 1:8){
  # Use dta_4c_new
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- dta_4c_new[testIndexes, c(4:10)]
  trainData <- dta_4c_new[-testIndexes, c(4:10)]
  rst_forests <- randomForest(CogTask~., data = trainData,
                              mtry = allmtry , 
                              ntree = overallntree, 
                              nodesize = allnodesize,
                              importance = T)
  # Overall
  y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
  y_train_hat <- predict(rst_forests,trainData,type="response")
  y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
  y_test_hat <- predict(rst_forests,testData,type="response")
  ce_train <- mean(y_train_hat!=trainData$CogTask, na.rm = T)
  ce_test <- mean(y_test_hat!=testData$CogTask, na.rm = T)
  rst$train[i,1] <- ce_train 
  rst$test[i,1] <- ce_test
  # for 3d plot
  dta_4c_rst[testIndexes,]$pred_CogTask <- y_test_hat
  # Calc
  Calc_train <- filter(trainData, CogTask == "Calc")
  Calc_test <- filter(testData, CogTask == "Calc")
  y_train_prob_tree <- predict(rst_forests,Calc_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Calc_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Calc_test ,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Calc_test,type="response")
  ce_train <- mean(y_train_hat!=Calc_train$CogTask)
  ce_test <- mean(y_test_hat!=Calc_test$CogTask)
  # 5 column : all, Calc, Reas, Spat, None
  rst$train[i,2] <- ce_train
  rst$test[i,2] <- ce_test
  # Reas
  Reas_train <- filter(trainData, CogTask == "Reas")
  Reas_test <- filter(testData, CogTask == "Reas")
  y_train_prob_tree <- predict(rst_forests,Reas_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Reas_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Reas_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Reas_test,type="response")
  ce_train <- mean(y_train_hat != Reas_train $CogTask)
  ce_test <- mean(y_test_hat != Reas_test$CogTask)
  # 5 column : all, Calc, Reas, Spat, None
  rst$train[i,3] <- ce_train
  rst$test[i,3] <- ce_test
  # Spat
  Spat_train <- filter(trainData, CogTask == "Spat")
  Spat_test <- filter(testData, CogTask == "Spat")
  y_train_prob_tree <- predict(rst_forests,Spat_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Spat_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Spat_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Spat_test,type="response")
  ce_train <- mean(y_train_hat != Spat_train $CogTask)
  ce_test <- mean(y_test_hat != Spat_test$CogTask)
  # 5 column : all, Calc, Reas, Spat, None
  rst$train[i,4] <- ce_train
  rst$test[i,4] <- ce_test
  # None
  None_train <- filter(trainData, CogTask == "None")
  None_test <- filter(testData, CogTask == "None")
  y_train_prob_tree <- predict(rst_forests,None_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,None_train,type="response")
  y_test_prob_tree <- predict(rst_forests,None_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,None_test,type="response")
  ce_train <- mean(y_train_hat != None_train $CogTask)
  ce_test <- mean(y_test_hat != None_test$CogTask)
  # 5 column : all, Calc, Reas, Spat, None
  rst$train[i,5] <- ce_train 
  rst$test[i,5] <- ce_test
}
# 5 column : all, Calc, Reas, Spat, None
RF_CogTask[1,2] <- mean(rst$test[,1])
RF_CogTask[1,3] <- mean(rst$test[,2])
RF_CogTask[1,4] <- mean(rst$test[,3])
RF_CogTask[1,5] <- mean(rst$test[,4])
RF_CogTask[1,6] <- mean(rst$test[,5])

################ Dan ################ 
set.seed(1)
# equally sampling
dta_4c_dan_Calc <- filter(dta_4c_dan, CogTask == "Calc")
dta_4c_dan_Reas <- filter(dta_4c_dan, CogTask == "Reas")
dta_4c_dan_Spat <- filter(dta_4c_dan, CogTask == "Spat")
dta_4c_dan_None <- filter(dta_4c_dan, CogTask == "None")
# random order
dta_4c_dan_Calc <- dta_4c_dan_Calc[sample(nrow(dta_4c_dan_Calc)),]
dta_4c_dan_Reas  <- dta_4c_dan_Reas[sample(nrow(dta_4c_dan_Reas)),]
dta_4c_dan_Spat <- dta_4c_dan_Spat[sample(nrow(dta_4c_dan_Spat)),]
dta_4c_dan_None <- dta_4c_dan_None[sample(nrow(dta_4c_dan_None)),]
# bind data
dta_4c_dan_new <- rbind(dta_4c_dan_Calc,dta_4c_dan_Reas,dta_4c_dan_Spat,dta_4c_dan_None)
# 8 folds
folds <- rep(rep(1:8,2),4)

rst <- list()
rst$train <- matrix(NA,8,5)
rst$test <- matrix(NA,8,5)

for(i in 1:8){
  # Use dta_4c_new
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- dta_4c_dan_new[testIndexes, c(4:10)]
  trainData <- dta_4c_dan_new[-testIndexes, c(4:10)]
  rst_forests <- randomForest(CogTask~., data = trainData,
                              mtry = danmtry , 
                              ntree = overallntree, 
                              nodesize = dannodesize,
                              importance = T)
  # Overall
  y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
  y_train_hat <- predict(rst_forests,trainData,type="response")
  y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
  y_test_hat <- predict(rst_forests,testData,type="response")
  ce_train <- mean(y_train_hat!=trainData$CogTask, na.rm = T)
  ce_test <- mean(y_test_hat!=testData$CogTask, na.rm = T)
  rst$train[i,1] <- ce_train 
  rst$test[i,1] <- ce_test
  # Calc
  Calc_train <- filter(trainData, CogTask == "Calc")
  Calc_test <- filter(testData, CogTask == "Calc")
  y_train_prob_tree <- predict(rst_forests,Calc_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Calc_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Calc_test ,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Calc_test,type="response")
  ce_train <- mean(y_train_hat!=Calc_train$CogTask)
  ce_test <- mean(y_test_hat!=Calc_test$CogTask)
  # 5 column : all, Calc, Reas, Spat, None
  rst$train[i,2] <- ce_train
  rst$test[i,2] <- ce_test
  # Reas
  Reas_train <- filter(trainData, CogTask == "Reas")
  Reas_test <- filter(testData, CogTask == "Reas")
  y_train_prob_tree <- predict(rst_forests,Reas_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Reas_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Reas_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Reas_test,type="response")
  ce_train <- mean(y_train_hat != Reas_train $CogTask)
  ce_test <- mean(y_test_hat != Reas_test$CogTask)
  # 5 column : all, Calc, Reas, Spat, None
  rst$train[i,3] <- ce_train
  rst$test[i,3] <- ce_test
  # Spat
  Spat_train <- filter(trainData, CogTask == "Spat")
  Spat_test <- filter(testData, CogTask == "Spat")
  y_train_prob_tree <- predict(rst_forests,Spat_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Spat_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Spat_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Spat_test,type="response")
  ce_train <- mean(y_train_hat != Spat_train $CogTask)
  ce_test <- mean(y_test_hat != Spat_test$CogTask)
  # 5 column : all, Calc, Reas, Spat, None
  rst$train[i,4] <- ce_train
  rst$test[i,4] <- ce_test
  # None
  None_train <- filter(trainData, CogTask == "None")
  None_test <- filter(testData, CogTask == "None")
  y_train_prob_tree <- predict(rst_forests,None_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,None_train,type="response")
  y_test_prob_tree <- predict(rst_forests,None_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,None_test,type="response")
  ce_train <- mean(y_train_hat != None_train $CogTask)
  ce_test <- mean(y_test_hat != None_test$CogTask)
  # 5 column : all, Calc, Reas, Spat, None
  rst$train[i,5] <- ce_train 
  rst$test[i,5] <- ce_test
}

# 5 column : all, Calc, Reas, Spat, None
RF_CogTask[2,2] <- mean(rst$test[,1])
RF_CogTask[2,3] <- mean(rst$test[,2])
RF_CogTask[2,4] <- mean(rst$test[,3])
RF_CogTask[2,5] <- mean(rst$test[,4])
RF_CogTask[2,6] <- mean(rst$test[,5])

################ Kyu ################ 
set.seed(1)
# equally sampling
dta_4c_kyu_Calc <- filter(dta_4c_kyu, CogTask == "Calc")
dta_4c_kyu_Reas <- filter(dta_4c_kyu, CogTask == "Reas")
dta_4c_kyu_Spat <- filter(dta_4c_kyu, CogTask == "Spat")
dta_4c_kyu_None <- filter(dta_4c_kyu, CogTask == "None")
# random order
dta_4c_kyu_Calc <- dta_4c_kyu_Calc[sample(nrow(dta_4c_kyu_Calc)),]
dta_4c_kyu_Reas  <- dta_4c_kyu_Reas[sample(nrow(dta_4c_kyu_Reas)),]
dta_4c_kyu_Spat <- dta_4c_kyu_Spat[sample(nrow(dta_4c_kyu_Spat)),]
dta_4c_kyu_None <- dta_4c_kyu_None[sample(nrow(dta_4c_kyu_None)),]
# bind data
dta_4c_kyu_new <- rbind(dta_4c_kyu_Calc,dta_4c_kyu_Reas,dta_4c_kyu_Spat,dta_4c_kyu_None)
# 8 folds
folds <- rep(rep(1:8,1),4)

rst <- list()
rst$train <- matrix(NA,8,5)
rst$test <- matrix(NA,8,5)
for(i in 1:8){
  # Use dta_4c_new
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- dta_4c_kyu_new[testIndexes, c(4:10)]
  trainData <- dta_4c_kyu_new[-testIndexes, c(4:10)]
  rst_forests <- randomForest(CogTask~., data = trainData,
                              mtry = kyumtry , 
                              ntree = overallntree, 
                              nodesize = kyunodesize,
                              importance = T)
  # Overall
  y_train_prob_tree <- predict(rst_forests,trainData,type="prob")[,2]
  y_train_hat <- predict(rst_forests,trainData,type="response")
  y_test_prob_tree <- predict(rst_forests,testData ,type="prob")[,2]
  y_test_hat <- predict(rst_forests,testData,type="response")
  ce_train <- mean(y_train_hat!=trainData$CogTask, na.rm = T)
  ce_test <- mean(y_test_hat!=testData$CogTask, na.rm = T)
  rst$train[i,1] <- ce_train 
  rst$test[i,1] <- ce_test
  # Calc
  Calc_train <- filter(trainData, CogTask == "Calc")
  Calc_test <- filter(testData, CogTask == "Calc")
  y_train_prob_tree <- predict(rst_forests,Calc_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Calc_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Calc_test ,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Calc_test,type="response")
  ce_train <- mean(y_train_hat!=Calc_train$CogTask)
  ce_test <- mean(y_test_hat!=Calc_test$CogTask)
  # 5 column : all, Calc, Reas, Spat, None
  rst$train[i,2] <- ce_train
  rst$test[i,2] <- ce_test
  # Reas
  Reas_train <- filter(trainData, CogTask == "Reas")
  Reas_test <- filter(testData, CogTask == "Reas")
  y_train_prob_tree <- predict(rst_forests,Reas_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Reas_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Reas_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Reas_test,type="response")
  ce_train <- mean(y_train_hat != Reas_train $CogTask)
  ce_test <- mean(y_test_hat != Reas_test$CogTask)
  # 5 column : all, Calc, Reas, Spat, None
  rst$train[i,3] <- ce_train
  rst$test[i,3] <- ce_test
  # Spat
  Spat_train <- filter(trainData, CogTask == "Spat")
  Spat_test <- filter(testData, CogTask == "Spat")
  y_train_prob_tree <- predict(rst_forests,Spat_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,Spat_train,type="response")
  y_test_prob_tree <- predict(rst_forests,Spat_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,Spat_test,type="response")
  ce_train <- mean(y_train_hat != Spat_train $CogTask)
  ce_test <- mean(y_test_hat != Spat_test$CogTask)
  # 5 column : all, Calc, Reas, Spat, None
  rst$train[i,4] <- ce_train
  rst$test[i,4] <- ce_test
  # None
  None_train <- filter(trainData, CogTask == "None")
  None_test <- filter(testData, CogTask == "None")
  y_train_prob_tree <- predict(rst_forests,None_train,type="prob")[,2]
  y_train_hat <- predict(rst_forests,None_train,type="response")
  y_test_prob_tree <- predict(rst_forests,None_test,type="prob")[,2]
  y_test_hat <- predict(rst_forests,None_test,type="response")
  ce_train <- mean(y_train_hat != None_train $CogTask)
  ce_test <- mean(y_test_hat != None_test$CogTask)
  # 5 column : all, Calc, Reas, Spat, None
  rst$train[i,5] <- ce_train 
  rst$test[i,5] <- ce_test
}

# 5 column : all, Calc, Reas, Spat, None
RF_CogTask[3,2] <- mean(rst$test[,1])
RF_CogTask[3,3] <- mean(rst$test[,2])
RF_CogTask[3,4] <- mean(rst$test[,3])
RF_CogTask[3,5] <- mean(rst$test[,4])
RF_CogTask[3,6] <- mean(rst$test[,5])

RF_CogTask
saveRDS(RF_CogTask,file = "Output/CogTask_bACCbRT_RF_Rst.Rdata")
head(dta_4c_rst)
saveRDS(dta_4c_rst,file = "../GoCogdata/dta_4c_rst.Rdata")

