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
perm_all <- readRDS("Output/GoStage_bACCbRT_RF_All_Perm10000.Rdata")
perm_dan <- readRDS("Output/GoStage_bACCbRT_RF_Dan_Perm10000.Rdata")
perm_kyu <- readRDS("Output/GoStage_bACCbRT_RF_Kyu_Perm10000.Rdata")
rstce <- readRDS("Output/GoStage_bACCbRT_RF_Rst.Rdata")
rst <- rstce
rst[,2:5] = 1-rstce[,2:5]

perm_all_test <- as.data.frame(1-perm_all$test)
colnames(perm_all_test) <- c("All","Open","Mid","End")
#head(perm_all_test)
perm_dan_test <- as.data.frame(1-perm_dan$test)
colnames(perm_dan_test) <- c("All","Open","Mid","End")
#head(perm_dan_test)
perm_kyu_test <- as.data.frame(1-perm_kyu$test)
colnames(perm_kyu_test) <- c("All","Open","Mid","End")
#head(perm_dan_test)
#perm_all_test_long <- melt(perm_all_test)
#colnames(perm_all_test_long) <- c("GoStage","CE")
perm_dan_test$Level <- "Dan"
perm_kyu_test$Level <- "Kyu"
perm_dankyu <- rbind(perm_dan_test,perm_kyu_test)


Gostage_all_ACC <- ggplot(data = perm_all_test, aes(x = All))+  
  #facet_grid(~GoStage)+
  geom_density(alpha=.35, col = "chartreuse4", fill = "chartreuse4" )+
  geom_vline(aes(xintercept=quantile(perm_all_test[,1],c(.95))[1]),
             color="firebrick", size=1)+
  geom_vline(aes(xintercept=rst$All[1]), # All
             color="#FF6699",linetype="F1",size=1)+
  #geom_vline(aes(xintercept=rst$Open[1]), # Open
  #           color="#FF66CC",linetype="longdash",size=1)+
  #geom_vline(aes(xintercept=rst$Mid[1]), # Mid
  #           color="#FF33CC",linetype="dashed",size=1)+
  #geom_vline(aes(xintercept=rst$End[1]), # End
  #           color="#CC0099",linetype="dotted",size=1)+
  ggtitle(paste0("Overall Performance: Mean ACC = ",
                 xintercept=round(rst$All[1],2)))+
  ylab("Density")+
  xlab("CE")+
  xlim(0,1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0.5))

Gostage_dan_ACC <- ggplot(data = perm_dan_test, aes(x = All))+  
  #facet_grid(~GoStage)+
  geom_density(alpha=.35, col = "chartreuse4", fill = "chartreuse4" )+
  geom_vline(aes(xintercept=quantile(perm_dan_test[,1],c(.95))[1]),
             color="firebrick", size=1)+
  geom_vline(aes(xintercept=rst$All[2]), # All
             color="#FF6699",linetype="F1",size=1)+
  #geom_vline(aes(xintercept=rst$Open[2]), # Open
  #           color="#FF66CC",linetype="longdash",size=1)+
  #geom_vline(aes(xintercept=rst$Mid[2]), # Mid
  #           color="#FF33CC",linetype="dashed",size=1)+
  #geom_vline(aes(xintercept=rst$End[2]), # End
  #           color="#CC0099",linetype="dotted",size=1)+
  ggtitle(paste0("Dan Performance: Mean ACC = ",
                 xintercept=round(rst$All[2],2)))+
  ylab("Density")+
  xlab("CE")+
  xlim(0,1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0.5))


Gostage_kyu_ACC <- ggplot(data = perm_kyu_test, aes(x = All))+  
  #facet_grid(~GoStage)+
  geom_density(alpha=.35, col = "chartreuse4", fill = "chartreuse4" )+
  geom_vline(aes(xintercept=quantile(perm_kyu_test[,1],c(.95))[1]),
             color="firebrick", size=1)+
  geom_vline(aes(xintercept=rst$All[3]), # All
             color="#FF6699",linetype="F1",size=1)+
  #geom_vline(aes(xintercept=rst$Open[3]), # Open
  #           color="#FF66CC",linetype="longdash",size=1)+
  #geom_vline(aes(xintercept=rst$Mid[3]), # Mid
  #           color="#FF33CC",linetype="dashed",size=1)+
  #geom_vline(aes(xintercept=rst$End[3]), # End
  #           color="#CC0099",linetype="dotted",size=1)+
  ggtitle(paste0("Kyu Performance: Mean ACC = ",
                 xintercept=round(rst$All[3],2)))+
  ylab("Density")+
  xlab("CE")+
  xlim(0,1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0.5))

Gostage_dankyu_ACC <- ggplot(data = perm_dankyu, aes(x = All, 
                                                     group= Level, fill = Level))+  
  facet_grid(~Level)+
  geom_density(alpha=.35)+
  #geom_vline(aes(xintercept=quantile(perm_kyu_test[,1],c(.95))[1]),
  #           color="firebrick", size=1)+
  #geom_vline(aes(xintercept=rst$All[3]), # All
  #           color="#FF6699",linetype="F1",size=1)+
  #geom_vline(aes(xintercept=rst$Open[3]), # Open
  #           color="#FF66CC",linetype="longdash",size=1)+
  #geom_vline(aes(xintercept=rst$Mid[3]), # Mid
  #           color="#FF33CC",linetype="dashed",size=1)+
  #geom_vline(aes(xintercept=rst$End[3]), # End
  #           color="#CC0099",linetype="dotted",size=1)+
  #ggtitle(paste0("Kyu Performance: Mean ACC = ",
  #               xintercept=round(rst$All[3],2)))+
  ylab("Density")+
  xlab("CE")+
  xlim(0,1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(Gostage_all_ACC,Gostage_dan_ACC,Gostage_kyu_ACC,ncol=3)

