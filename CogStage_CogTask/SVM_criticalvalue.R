alphavalue = 0.0005

perm_all  <- readRDS("Output/SVM/GoStage_bACCbRT_SVM_All_Perm10000.Rdata")
perm_all_test <- as.data.frame(1-perm_all$test)
colnames(perm_all_test) <- c("All","Open","Mid","End")
perm_all_test <- perm_all_test*100
head(perm_all_test)
tail(perm_all_test)
quantile(perm_all_test[,1],c(1-alphavalue))[1] # 54.16736 
                                   

perm_all2 <- readRDS("Output/SVM/CogTask_bACCbRT_SVM_All_Perm10000.Rdata")
perm_all_test2 <- as.data.frame(1-perm_all2$test)*100
colnames(perm_all_test2) <- c("All","Calc","Reas","Spat","None")
head(perm_all_test2)
tail(perm_all_test2)
quantile(perm_all_test2[,1],c(1-alphavalue))[1] # 42.70833
