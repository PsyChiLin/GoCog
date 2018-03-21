library(randomForest)
library(dplyr)
library(reshape2)
library(e1071)
library(pROC)
library(ggplot2)
library(grid)
library(gridExtra)

rm(list = ls())
theme_default <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme( strip.background = element_blank()
    )
}
perm_all <- readRDS("Output/CogTask_bACCbRT_RF_All_Perm10000.Rdata")
perm_dan <- readRDS("Output/CogTask_bACCbRT_RF_Dan_Perm10000.Rdata")
perm_kyu <- readRDS("Output/CogTask_bACCbRT_RF_Kyu_Perm10000.Rdata")
rstce <- readRDS("Output/CogTask_bACCbRT_RF_Rst.Rdata")
rst <- rstce
rst[,2:6] = 1-rstce[,2:6]

perm_all_test <- as.data.frame(1-perm_all$test)
colnames(perm_all_test) <- c("All","Calc","Reas","Spat","None")
#head(perm_all_test)
perm_dan_test <- as.data.frame(1-perm_dan$test)
colnames(perm_dan_test) <-  c("All","Calc","Reas","Spat","None")
#head(perm_dan_test)
perm_kyu_test <- as.data.frame(1-perm_kyu$test)
colnames(perm_kyu_test) <-  c("All","Calc","Reas","Spat","None")
#head(perm_dan_test)
#perm_all_test_long <- melt(perm_all_test)
#colnames(perm_all_test_long) <- c("CogTask","accuracy")
perm_dan_test$Level <- "Dan"
perm_kyu_test$Level <- "Kyu"
perm_dankyu <- rbind(perm_dan_test,perm_kyu_test)


CogTask_all_ACC <- ggplot(data = perm_all_test, aes(x = All))+  
  #facet_grid(~CogTask)+
  geom_density(alpha=.5, col = "#000000", fill = "#000000" )+
  geom_vline(aes(xintercept=quantile(perm_all_test[,1],c(.95))[1]),
             #color="firebrick",
             size=1)+
  geom_vline(aes(xintercept=rst$All[1]),
             #color="#FF6699",
             linetype="twodash",size=1)+
  # geom_vline(aes(xintercept=rst$Calc[1]),
  #            color="#FF66CC",linetype="longdash",size=1)+
  # geom_vline(aes(xintercept=rst$Reas[1]),
  #            color="#FF33CC",linetype="dashed",size=1)+
  # geom_vline(aes(xintercept=rst$Spat[1]),
  #            color="#CC0099",linetype="dotdash",size=1)+
  # geom_vline(aes(xintercept=rst$None[1]),
  #            color="663366",linetype="dotted",size=1)+
  ggtitle(paste0("(A) CogTask: Overall Performance"))+
  ylab("Density")+
  xlab("Accuracy")+
  xlim(0,1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10))

CogTask_dan_ACC <- ggplot(data = perm_dan_test, aes(x = All))+  
  #facet_grid(~CogTask)+
  geom_density(alpha=.5, col = "#000000", fill = "#000000")+
  geom_vline(aes(xintercept=quantile(perm_dan_test[,1],c(.95))[1]),
             #color="firebrick",
             size=1)+
  geom_vline(aes(xintercept=rst$All[2]), # All
             #color="#FF6699",
             linetype="twodash",size=1)+
  geom_vline(aes(xintercept=rst$All[2]),
             #color="#FF6699",
             linetype="twodash",size=1)+
  geom_vline(aes(xintercept=rst$Calc[2]),
             #color="#FF66CC",
             linetype="longdash",size=1)+
  geom_vline(aes(xintercept=rst$Reas[2]),
             #color="#FF33CC",
             linetype="dashed",size=1)+
  geom_vline(aes(xintercept=rst$Spat[2]),
             #color="#CC0099",
             linetype="dotdash",size=1)+
  geom_vline(aes(xintercept=rst$None[2]),
             #color="663366",
             linetype="dotted",size=1)+
  ggtitle(paste0("Dan Performance: Mean ACC = ",
                 xintercept=round(rst$All[2],2)))+
  ylab("Density")+
  xlab("Accuracy")+
  xlim(0,1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10))


CogTask_kyu_ACC <- ggplot(data = perm_kyu_test, aes(x = All))+  
  #facet_grid(~CogTask)+
  geom_density(alpha=.5, col = "#000000", fill = "#000000" )+
  geom_vline(aes(xintercept=quantile(perm_kyu_test[,1],c(.95))[1]),
             #color="firebrick",
             size=1)+
  geom_vline(aes(xintercept=rst$All[3]), # All
             #color="#FF6699",
             linetype="twodash",size=1)+
  geom_vline(aes(xintercept=rst$All[3]),
             #color="#FF6699",
             linetype="twodash",size=1)+
  geom_vline(aes(xintercept=rst$Calc[3]),
             #color="#FF66CC",
             linetype="longdash",size=1)+
  geom_vline(aes(xintercept=rst$Reas[3]),
             #color="#FF33CC",
             linetype="dashed",size=1)+
  geom_vline(aes(xintercept=rst$Spat[3]),
             #color="#CC0099",
             linetype="dotdash",size=1)+
  geom_vline(aes(xintercept=rst$None[3]),
             #color="663366",
             linetype="dotted",size=1)+
  ggtitle(paste0("Kyu Performance: Mean ACC = ",
                 xintercept=round(rst$All[3],2)))+
  ylab("Density")+
  xlab("Accuracy")+
  xlim(0,1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10))

CogTask_dankyu_ACC <- ggplot(data = perm_dankyu, aes(x = All, 
                                                     group= Level, fill = Level))+  
  facet_grid(~Level)+
  geom_density(alpha=.5)+
  geom_vline(data=filter(perm_dankyu, Level == "Dan"), 
             aes(xintercept = quantile(filter(perm_dankyu, Level == "Dan")[,1],.95)[1]), 
             #col = "firebrick",
             size=1)+
  geom_vline(data=filter(perm_dankyu, Level == "Kyu"), 
             aes(xintercept = quantile(filter(perm_dankyu, Level == "Kyu")[,1],.95)[1]), 
             #col = "firebrick",
             size=1) + 
  geom_vline(data=filter(perm_dankyu, Level == "Dan"), 
             aes(xintercept = mean(rst$All[2])), 
             #col = "#FF6699",
             linetype="twodash",size=1)+
  geom_vline(data=filter(perm_dankyu, Level == "Kyu"), 
             aes(xintercept = mean(rst$All[3])), 
             #col = "#FF6699",
             linetype="twodash",size=1) +
  ggtitle(paste0("(B) CogTask: Dan & Kyu Performance"))+
  scale_fill_grey()+
  ylab("Density")+
  xlab("Accuracy")+
  xlim(0,1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        legend.position = c(0.9,0.8))
  #scale_fill_brewer(palette="Set1")

pdf("CogTask.pdf", width = 7,height = 7)
grid.arrange(CogTask_all_ACC,CogTask_dankyu_ACC,ncol=1)
dev.off()

