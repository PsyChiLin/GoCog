library(randomForest)
library(dplyr)
library(reshape2)
library(e1071)
library(pROC)
library(ggplot2)
theme_default <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme( strip.background = element_blank()
    )
}
#meanrst_boot <- readRDS("../GoCogdata/DanKyu_BothACC_RF_10000_EX.Rdata")
#meanrst_boot <- readRDS("../GoCogdata/DanKyu_BothACC_RF_10000_US.Rdata")
meanrst_boot <- readRDS("../GoCogdata/DanKyu_BothACC_RF_10000_OS.Rdata")

dattest <- as.data.frame(meanrst_boot$test)
colnames(dattest) <- c("CE","AUC")
head(dattest)

ggplot(data = dattest,aes(x = CE))+  
  #geom_histogram(aes(y = ..density..),
  #               binwidth = 0.05,
  #               #center = 0.01,
  #               color="darkblue", 
  #               fill="#6699FF",alpha = 0.5)+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(CE)),
             color="#FF6699", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(meanrst_boot$test[,1],c(.025,.975))[1]),
             color="#FF6699", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(meanrst_boot$test[,1],c(.025,.975))[2]),
             color="#FF6699", linetype="dashed", size=1)+
  ggtitle(paste0("Mean CE = ",round(mean(dattest$CE),3),
                 " (",
                 round(quantile(meanrst_boot$test[,1],c(.025,.975))[1],3),
                 " ~ ",
                 round(quantile(meanrst_boot$test[,1],c(.025,.975))[2],3),
                 ")"))+
  ylab("Density")+
  xlab("CE")+
  theme_default()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dattest,aes(x = AUC))+  
  #geom_histogram(aes(y = ..density..),
  #               binwidth = 0.01,
  #               center = 5,
  #               color="darkblue", 
  #               fill="#6699FF",alpha = 0.5)+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(AUC)),
             color="#FF6699", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(meanrst_boot$test[,2],c(.025,.975))[1]),
             color="#FF6699", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=quantile(meanrst_boot$test[,2],c(.025,.975))[2]),
             color="#FF6699", linetype="dashed", size=1)+
  ggtitle(paste0("Mean AUC = ",round(mean(dattest$AUC),3),
                 " (",
                 round(quantile(meanrst_boot$test[,2],c(.025,.975))[1],3),
                 " ~ ",
                 round(quantile(meanrst_boot$test[,2],c(.025,.975))[2],3),
                 ")"))+
  ylab("Density")+
  xlab("AUC")+
  theme_default()+
  theme(plot.title = element_text(hjust = 0.5))
