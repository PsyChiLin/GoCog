library(randomForest)
library(dplyr)
library(reshape2)
library(e1071)
library(pROC)
library(ggplot2)

rm(list = ls())
theme_default <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme( strip.background = element_blank()
    )
}
meanrst_boot <- readRDS("../GoCogdata/CogTask_RF_Boot_10000_Ex.Rdata")
dattest <- na.omit(as.data.frame(meanrst_boot$test))
colnames(dattest) <- c("All","Calc","Reas","Spat","None")
head(dattest)
long <- melt(dattest)
colnames(long) <- c("GogTask","CE")

ggplot(data = long, aes(x = CE, group = GogTask, fill = GogTask))+  
  geom_density(alpha=.35)+
  geom_vline(aes(xintercept=mean(dattest$All)),
             color="#FF6699", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(dattest[,1],c(.025,.975))[1]),
             color="#FF6699", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(dattest[,1],c(.025,.975))[2]),
             color="#FF6699", linetype="dashed", size=1)+
  ggtitle(paste0("All Stages : Mean CE = ",round(mean(dattest$All),3),
                 " (",
                 round(quantile(dattest[,1],c(.025,.975))[1],3),
                 " ~ ",
                 round(quantile(dattest[,1],c(.025,.975))[2],3),
                 ")"))+
  ylab("Density")+
  xlab("CE")+
  theme_default()+
  theme(plot.title = element_text(hjust = 0.5))