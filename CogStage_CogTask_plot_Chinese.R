library(ggthemes)
library(scales)
library(reshape2)
library(randomForest)
library(dplyr)
library(reshape2)
library(e1071)
library(pROC)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggsignif)

rm(list = ls())
theme_default <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.y  = element_text(angle = 0, vjust = 0.5),
          axis.text.x  = element_text(angle = 0, vjust = 0.5),
          axis.text.y =  element_blank()
    )
}

perm_all <- readRDS("Output/GoStage_bACCbRT_RF_All_Perm10000.Rdata")
#perm_dan <- readRDS("Output/GoStage_bACCbRT_RF_Dan_Perm10000.Rdata")
#perm_kyu <- readRDS("Output/GoStage_bACCbRT_RF_Kyu_Perm10000.Rdata")
rstce <- readRDS("Output/GoStage_bACCbRT_RF_Rst.Rdata")
rst <- rstce
rst[,2:5] = (1-rstce[,2:5])*100

perm_all_test <- as.data.frame(1-perm_all$test)
colnames(perm_all_test) <- c("All","Open","Mid","End")
perm_all_test <- perm_all_test*100
head(perm_all_test)


Gostage_all_ACC <- ggplot(data = perm_all_test, aes(x = All))+  
  #facet_grid(~GoStage)+
  geom_density(alpha=.3,
               col = "#000000", 
               fill = "#000000"
  )+
  geom_vline(aes(xintercept=quantile(perm_all_test[,1],c(.95))[1]),
             #color="firebrick", 
             size=1)+
  geom_vline(aes(xintercept=rst$All[1]), # All
             #color="#FF6699",
             linetype="dashed",size=1)+
  #geom_vline(aes(xintercept=rst$Open[1]), # Open
  #           color="#FF66CC",linetype="longdash",size=1)+
  #geom_vline(aes(xintercept=rst$Mid[1]), # Mid
  #           color="#FF33CC",linetype="dashed",size=1)+
  #geom_vline(aes(xintercept=rst$End[1]), # End
  #           color="#CC0099",linetype="dotted",size=1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        text = element_text(family = 'BiauKai'),
        )+
  ggtitle(paste0("(A) 三階段圍棋"))+
  ylab("機\n率\n密\n度")+
  xlab("預測正確性(%)")+
  xlim(0,100)
#Gostage_all_ACC

perm_all2 <- readRDS("Output/CogTask_bACCbRT_RF_All_Perm10000.Rdata")
rstce2 <- readRDS("Output/CogTask_bACCbRT_RF_Rst.Rdata")
rst2 <- rstce2
rst2[,2:6] = (1-rstce2[,2:6])*100
perm_all_test2 <- as.data.frame(1-perm_all2$test)*100
colnames(perm_all_test2) <- c("All","Calc","Reas","Spat","None")

CogTask_all_ACC <- ggplot(data = perm_all_test2, aes(x = All))+  
  #facet_grid(~CogTask)+
  geom_density(alpha=.5, col = "#000000", fill = "#000000" )+
  geom_vline(aes(xintercept=quantile(perm_all_test2[,1],c(.95))[1]),
             #color="firebrick",
             size=1)+
  geom_vline(aes(xintercept=rst2$All[1]),
             #color="#FF6699",
             linetype="dashed",size=1)+
  # geom_vline(aes(xintercept=rst$Calc[1]),
  #            color="#FF66CC",linetype="longdash",size=1)+
  # geom_vline(aes(xintercept=rst$Reas[1]),
  #            color="#FF33CC",linetype="dashed",size=1)+
  # geom_vline(aes(xintercept=rst$Spat[1]),
  #            color="#CC0099",linetype="dotdash",size=1)+
  # geom_vline(aes(xintercept=rst$None[1]),
  #            color="663366",linetype="dotted",size=1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        text = element_text(family = 'BiauKai'),
  )+
  ggtitle(paste0("(B) 四種認知干擾"))+
  ylab("機\n率\n密\n度")+
  xlab("預測正確性(%)")+
  xlim(0,100)


#tiff(file = "../GoCog_Manuscript/FigureTable/圖9.tiff",height=6, width=6, units="in", res = 300,compression = "lzw")
grid.arrange(Gostage_all_ACC,CogTask_all_ACC,ncol=1)
#dev.off()