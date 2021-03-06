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
          #axis.title.y  = element_text(angle = 0, vjust = 0.5),
          axis.text.x  = element_text(angle = 0, vjust = 0.5),
          axis.text.y =  element_blank()
    )
}

perm_all <- readRDS("Output/GoStage_bACCbRT_RF_All_Perm10000.Rdata")
rstce <- readRDS("Output/GoStage_bACCbRT_RF_Rst.Rdata")
rst <- rstce
rst[,2:5] = (1-rstce[,2:5])*100

perm_all_test <- as.data.frame(1-perm_all$test)
colnames(perm_all_test) <- c("All","Open","Mid","End")
perm_all_test <- perm_all_test*100
head(perm_all_test)
alphavalue = 0.0005

Gostage_all_ACC <- ggplot(data = perm_all_test, aes(x = All))+  
  #facet_grid(~GoStage)+
  geom_density(alpha=.3,
               col = "#000000", 
               fill = "#000000"
  )+
  geom_vline(aes(xintercept=33.3333333),
             linetype="dotted",
             #color="firebrick",
             size=1)+
  geom_vline(aes(xintercept=quantile(perm_all_test[,1],c(1-alphavalue))[1]),
             #color="firebrick", 
             linetype="dashed",
             size=1)+
  geom_vline(aes(xintercept=rst$All[1]), # All
             #color="#FF6699",
             col = "black",
             size=1)+
  #geom_vline(aes(xintercept=rst$Open[1]), # Open
  #           color="#FF66CC",linetype="longdash",size=1)+
  #geom_vline(aes(xintercept=rst$Mid[1]), # Mid
  #           color="#FF33CC",linetype="dashed",size=1)+
  #geom_vline(aes(xintercept=rst$End[1]), # End
  #           color="#CC0099",linetype="dotted",size=1)+
  theme_default()+
  #theme(plot.title = element_text(hjust = 0,size = 10)#,
  #      #text = element_text(family = 'BiauKai')
  #      )+
  #ggtitle(paste0("(A) Three Go Stages"))+
  ylab("Probability Density")+
  xlab("Classification accuracy  (%)")+
  xlim(0,100)
#Gostage_all_ACC

png(file = "../GoCog_Manuscript/FigureTable/Abstract_Figure3.png",height=3, width=6, units="in", res = 300)
Gostage_all_ACC
dev.off()
