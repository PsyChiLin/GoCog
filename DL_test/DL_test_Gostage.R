######### GoStage Result #########
library(randomForest)
library(dplyr)
library(reshape2)
library(e1071)
library(pROC)
require(neuralnet) # for neuralnet(), nn model
require(nnet)      # for class.ind()
require(caret) 

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
#allmtry <- 7 
#allnodesize <- 20
#Danmtry <- 7 
#Dannodesize <- 15
#Kyumtry <- 7 
#Kyunodesize <- 3

#overallntree <-  1000

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


head(class.ind(dta_3s_rst$GoStage))
dta_3s_rst <- cbind(dta_3s_rst, class.ind(dta_3s_rst$GoStage))
formula.bpn <- End+Mid+Open~Calc_Both_ACC+None_Both_ACC+Reas_Both_ACC+Spat_Both_ACC+Calc_Both_RT+None_Both_RT+Reas_Both_RT+Spat_Both_RT



bpn <- neuralnet(formula = formula.bpn, 
                 data = dta_3s_rst,
                 hidden = c(1,2),       
                 learningrate = 0.001, # learning rate
                 threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                 stepmax = 5e5)

model <- train(form=formula.bpn,
               data=dta_3s_rst,
               method="neuralnet",
               
               tuneGrid = expand.grid(.layer1=c(1:10), .layer2=c(0:10), .layer3=c(0)),               
               
               learningrate = 0.001,
               threshold = 0.01,    
               stepmax = 5e5        
)

model


plot(bpn)
pred <- compute(bpn,  dta_3s_rst[,5:12])  
pred$net.result



ir1 <- nnet(GoStage~., data = dta_3s_rst[,4:12], size = 3, rang = 0.5,
            decay = 5e-4, maxit = 200)
test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  table(true, cres)
}
table(as.character(dta_3s_rst$GoStage), predict(ir1, dta_3s_rst[,4:12],type = "class"))


ird <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                  species = factor(c(rep("s",50), rep("c", 50), rep("v", 50))))
samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
ir.nn2 <- nnet(species ~ ., data = ird, subset = samp, size = 2, rang = 0.1,
               decay = 5e-4, maxit = 200)
test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  table(true, cres)
}
table(ird$species[-samp], predict(ir.nn2, ird[-samp,], type = "class"))







#x_train <- as.matrix(dta_3s_rst[,5:12])
#y_train <-  as.array(as.integer(ifelse(dta_3s_rst[,4] == "Open",0,
#                             ifelse(dta_3s_rst[,4] == "Mid",1,2))))
#
#y_train <- to_categorical(y_train, 3)
#y_test <- to_categorical(y_test, 10)



model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(8)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  #layer_dense(units = 64, activation = 'relu') %>%
  #layer_dropout(rate = 0.2) %>%
  #layer_dense(units = 32, activation = 'relu') %>%
  #layer_dropout(rate = 0.1) %>%
  layer_dense(units = 3, activation = "sigmoid")


model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_sgd(lr = 0.01, decay = 1e-6, momentum = 0.9, nesterov = TRUE),
  metrics = c('accuracy')     
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 10, batch_size = 72, 
  validation_split = 0.2
)


model %>% evaluate(x_train, y_train)












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
  # for 3d plot
  dta_3s_rst[testIndexes,]$pred_GoStage <- y_test_hat
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
#saveRDS(RF_GoStage,file = "Output/GoStage_bACCbRT_RF_Rst.Rdata")

head(dta_3s_rst) 
#saveRDS(dta_3s_rst,file = "../GoCogdata/dta_3s_rst.Rdata")







