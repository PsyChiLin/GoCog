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
rm(list = ls())
theme_default <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme( strip.background = element_blank()
    )
}
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Read Data
RST<-read.csv("../GoCogdata/NoneRST_All.csv", h=T)

#RST_bACCbRT <- melt(RST[,c(1:4,7,16:18)], 
#                    id.vars = c("Subj","Age","SubjGroup","Both_ACC","Both_RT"))
#colnames(RST_bACCbRT)[6:7] <- c("RST_Type","Score")
#head(RST_bACCbRT)

RSTz_bACCbRT <- melt(RST[,c(1:4,7,25:27)], 
                    id.vars = c("Subj","Age","SubjGroup","Both_ACC","Both_RT"))
colnames(RSTz_bACCbRT)[6:7] <- c("RST_Type","Score")
RSTz_bACCbRT$Both_RT <- RSTz_bACCbRT$Both_RT/1000
head(RSTz_bACCbRT)
  
  
bACC <- ggplot(data= RSTz_bACCbRT, aes(x = Both_ACC, y = Score,
                           color=SubjGroup, shape=SubjGroup)) +
  facet_grid(~RST_Type)+
  theme_default() + 
  theme(plot.title = element_text(hjust = 0,size = 10),
        legend.position = "none")+
  scale_colour_grey(start = 0.5, end = 0)+
  geom_point(data = RSTz_bACCbRT, aes(color = SubjGroup), size=2) +
  stat_smooth(data = RSTz_bACCbRT, aes(color = SubjGroup), method = "lm", se = F)+
  scale_shape_manual(values=c(1,19))+
  xlab(" ")+
  ggtitle(paste0("(A) Both ACC"))

bRT <- ggplot(data= RSTz_bACCbRT, aes(x = Both_RT, y = Score,
                                       color=SubjGroup, shape=SubjGroup)) +
  facet_grid(~RST_Type)+
  theme_default() + 
  theme(plot.title = element_text(hjust = 0,size = 10),
        legend.position = "none")+
  scale_colour_grey(start = 0.5, end = 0)+
  geom_point(data = RSTz_bACCbRT, aes(color = SubjGroup), size=2) +
  stat_smooth(data = RSTz_bACCbRT, aes(color = SubjGroup), method = "lm", se = F)+
  scale_shape_manual(values=c(1,19))+
  xlab(" ")+
  ggtitle(paste0("(B) Both RT"))

dk <- ggplot(data= RSTz_bACCbRT, aes(x = Both_RT, y = Score,
                                      color=SubjGroup, shape=SubjGroup)) +
  facet_grid(~RST_Type)+
  theme_default() + 
  scale_colour_grey(start = 0.5, end = 0)+
  geom_point(data = RSTz_bACCbRT, aes(color = SubjGroup), size=2) +
  stat_smooth(data = RSTz_bACCbRT, aes(color = SubjGroup), method = "lm", se = F)+
  scale_shape_manual(values=c(1,19))+
  theme(plot.title = element_text(hjust = 0,size = 10),
        legend.position = "top")

dk <- g_legend(dk)

pdf("Output/RST_DanKyu.pdf", width = 7,height = 7)
grid.arrange(arrangeGrob(bACC,
                         bRT,
                         nrow=2),
             dk, nrow=2,heights=c(10, 1))
dev.off()