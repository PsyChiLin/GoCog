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
meanrst_boot <- readRDS("../GoCogdata/GoStage_BothACC_RF_10000_Ex.Rdata")
dattest <- na.omit(as.data.frame(meanrst_boot$test))
colnames(dattest) <- c("All","Open","Mid","End")
head(dattest)
long <- melt(dattest)
colnames(long) <- c("GoStage","CE")

ggplot(data = long, aes(x = CE, group = GoStage, fill = GoStage))+  
  #facet_grid(~GoStage)+
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















# ##############################################################
# ggplot(data = dattest,aes(x = CE))+  
#   facet_grid(~GoStage)+
#   geom_density(alpha=.2, fill="#FF6666")+
#   geom_vline(aes(xintercept=mean(All)),
#              color="#FF6699", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=quantile(dattest[,1],c(.025,.975))[1]),
#              color="#FF6699", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=quantile(dattest[,1],c(.025,.975))[2]),
#              color="#FF6699", linetype="dashed", size=1)+
#   ggtitle(paste0("All Stages : Mean CE = ",round(mean(dattest$All),3),
#                  " (",
#                  round(quantile(dattest[,1],c(.025,.975))[1],3),
#                  " ~ ",
#                  round(quantile(dattest[,1],c(.025,.975))[2],3),
#                  ")"))+
#   ylab("Density")+
#   xlab("CE")+
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0.5))
# 
# Open <- ggplot(data = na.omit(dattest[,1:2]),aes(x = Open))+  
#   #facet_grid(~GoStage)+
#   geom_density(alpha=.2, fill="#FF6666")+
#   geom_vline(aes(xintercept=mean(Open)),
#              color="#FF6699", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=quantile(meanrst_boot$test[,1],c(.025,.975))[1]),
#              color="#FF6699", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=quantile(meanrst_boot$test[,1],c(.025,.975))[2]),
#              color="#FF6699", linetype="dashed", size=1)+
#   ggtitle(paste0("Open : Mean CE = ",round(mean(na.omit(dattest[,1:2])$Open),3),
#                  " (",
#                  round(quantile(na.omit(dattest[,1:2])[,2],c(.025,.975))[1],3),
#                  " ~ ",
#                  round(quantile(na.omit(dattest[,1:2])[,2],c(.025,.975))[2],3),
#                  ")"))+
#   ylab("Density")+
#   xlab("CE")+
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0.5))
# 
# 
