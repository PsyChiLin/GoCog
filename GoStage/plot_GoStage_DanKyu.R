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
meanrst_boot <- readRDS("../GoCogdata/GoStage_BothACC_RF_10000_Ex_Dan.Rdata")
dattest <- na.omit(as.data.frame(meanrst_boot$test))
colnames(dattest) <- c("All","Open","Mid","End")
head(dattest)
long <- melt(dattest)
colnames(long) <- c("GoStage","CE")

Gostage_dan <- ggplot(data = long, aes(x = CE, group = GoStage, fill = GoStage))+  
  #facet_grid(~GoStage)+
  geom_density(alpha=.35)+
  geom_vline(aes(xintercept=mean(dattest$All)),
             color="#FF6699", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(dattest[,1],c(.025,.975))[1]),
             color="#FF6699", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(dattest[,1],c(.025,.975))[2]),
             color="#FF6699", linetype="dashed", size=1)+
  ggtitle(paste0("Dan - All Stages : Mean CE = ",round(mean(dattest$All),3),
                 " (",
                 round(quantile(dattest[,1],c(.025,.975))[1],3),
                 " ~ ",
                 round(quantile(dattest[,1],c(.025,.975))[2],3),
                 ")"))+
  ylab("Density")+
  xlab("CE")+
  theme_default()+
  theme(plot.title = element_text(hjust = 0.5))

meanrst_boot2 <- readRDS("../GoCogdata/GoStage_BothACC_RF_10000_Ex_Kyu.Rdata")
dattest2 <- na.omit(as.data.frame(meanrst_boot2$test))
colnames(dattest2) <- c("All","Open","Mid","End")
head(dattest2)
long2 <- melt(dattest2)
colnames(long2) <- c("GoStage","CE")

Gostage_kyu <- ggplot(data = long2, aes(x = CE, group = GoStage, fill = GoStage))+  
  #facet_grid(~GoStage)+
  geom_density(alpha=.35)+
  geom_vline(aes(xintercept=mean(dattest2$All)),
             color="#FF6699", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(dattest2[,1],c(.025,.975))[1]),
             color="#FF6699", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(dattest2[,1],c(.025,.975))[2]),
             color="#FF6699", linetype="dashed", size=1)+
  ggtitle(paste0("Kyu - All Stages : Mean CE = ",round(mean(dattest2$All),3),
                 " (",
                 round(quantile(dattest2[,1],c(.025,.975))[1],3),
                 " ~ ",
                 round(quantile(dattest2[,1],c(.025,.975))[2],3),
                 ")"))+
  ylab("Density")+
  xlab("CE")+
  theme_default()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(Gostage_dan,Gostage_kyu, ncol = 2)
