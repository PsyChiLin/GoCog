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
    theme(strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0,size = 10),
          axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
          axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8)
          #text = element_text(family = 'BiauKai')
    )
}
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Read Data
RST<-read.csv("../GoCogdata/NoneRST_All.csv", h=T)[,c(1:3,25:27)]
GoCog<-read.csv("../GoCogdata/GoCog_20180404.csv", h=T)[,1:7]
RSTdata <- full_join(GoCog,RST)
RSTdata$Both_Acc <- RSTdata$Both_Acc*100
RSTdata$Both_RT <- RSTdata$Both_RT/1000
RSTdata$GoStage <- factor(RSTdata$GoStage, levels=c("Open", "Mid", "End"), 
                        labels = c("佈局","中盤","官子"))
RSTdata$CogTask<- factor(RSTdata$CogTask, levels=c("None", "Spat", "Reas", "Calc"),
                       labels = c("無\n干\n擾","空\n間\n干\n擾","推\n理\n干\n擾","計\n算\n干\n擾"))
RSTdata$SubjGroup <- factor(RSTdata$SubjGroup, levels=c("Dan", "Kyu"),
                          labels = c("段位","級位"))
Reas <- filter(RSTdata, CogTask == "推\n理\n干\n擾")
head(Reas)

# ACC <- ggplot(data= Reas, aes(x = Both_Acc, y = Total.z, 
#                               shape = GoStage)) +
#   facet_grid(~GoStage)+
#   #scale_colour_grey(start = 0.5, end = 0)+
#   theme_default()+
#   theme(text = element_text(family = 'BiauKai'),
#         legend.position = "none")+
#   #scale_colour_grey()+
#   geom_point(size=2) +
#   stat_smooth(method = "lm", se = T, col = "grey")+
#   scale_shape_manual(values=c(1,13,19))+
#   ggtitle(paste0("(A) 各階段圍棋題目正確率分析"))+
#   xlab("正確率(%)")+
#   ylab("推\n理\n思\n考\n測\n驗\n之\n標\n準\n化\n得\n分")+
#   xlim(0,100)
CogTask <- aggregate(data = RSTdata, cbind(Both_Acc,Both_RT,Verbal.z,Nonverbal.z,Total.z)~Subj+CogTask,FUN = mean)
Reas2 <- filter(CogTask, CogTask == "推\n理\n干\n擾")
overallACC <- ggplot(data= Reas2, aes(x = Both_Acc, y = Total.z)) +
  theme_default()+
  theme(text = element_text(family = 'BiauKai'))+
  #scale_colour_grey(start = 0.5, end = 0)+
  geom_point(size=2) +
  stat_smooth(method = "lm", se = T, col = "grey")+
  #ggtitle(paste0("(B) 整體正確率分析"))+
  xlab("正確率(%)")+
  ylab("推\n理\n思\n考\n測\n驗\n之\n標\n準\n化\n得\n分")#+
  #xlim(0,100)
#overallACC

# tiff(file = "../GoCog_Manuscript/FigureTable/圖8_RST.tiff",height=6, width=6, units="in", res = 300,compression = "lzw")
# grid.arrange(ACC,overallACC,ncol = 1, heights = c(4,6))
# dev.off()


tiff(file = "../GoCog_Manuscript/FigureTable/圖7_RST.tiff",height=3, width=3, units="in", res = 300,compression = "lzw")
overallACC
dev.off()


#ACC
# TotalACC <- ggplot(data= RSTdata, aes(x = Both_Acc, y = Total.z)) +
#   facet_grid(GoStage~CogTask)+
#   theme_default() + 
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         legend.position = "none")+
#   scale_colour_grey(start = 0.5, end = 0)+
#   geom_point(size=2) +
#   stat_smooth(method = "lm", se = T)+
#   #scale_shape_manual(values=c(1,19))+
#   #ggtitle(paste0("(A) 正確率"))+
#   xlab(" ")

# Verbal <- ggplot(data= RSTdata, aes(x = Both_Acc, y = Verbal.z)) +
#   facet_grid(GoStage~CogTask)+
#   theme_default() + 
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         legend.position = "none")+
#   scale_colour_grey(start = 0.5, end = 0)+
#   geom_point(size=2) +
#   stat_smooth(method = "lm", se = F)+
#   #scale_shape_manual(values=c(1,19))+
#   xlab(" ")+
#   ggtitle(paste0("(B) Verbal"))
# 
# NonVerbal <- ggplot(data= RSTdata, aes(x = Both_Acc, y = Nonverbal.z)) +
#   facet_grid(GoStage~CogTask)+
#   theme_default() + 
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         legend.position = "none")+
#   scale_colour_grey(start = 0.5, end = 0)+
#   geom_point(size=2) +
#   stat_smooth(method = "lm", se = F)+
#   #scale_shape_manual(values=c(1,19))+
#   xlab(" ")+
#   ggtitle(paste0("(C) NonVerbal"))


#RST_bACCbRT <- melt(RST[,c(1:4,7,16:18)], 
#                    id.vars = c("Subj","Age","SubjGroup","Both_ACC","Both_RT"))
#colnames(RST_bACCbRT)[6:7] <- c("RST_Type","Score")
#head(RST_bACCbRT)

# RSTz_bACCbRT_6 <- melt(RST[,c(1:4,7,10:15)], 
#                      id.vars = c("Subj","Age","SubjGroup","Both_ACC","Both_RT"))
# colnames(RSTz_bACCbRT_6)[6:7] <- c("RST_Type","Score")
# 
# ggplot(data= RSTz_bACCbRT_6, aes(x = Both_ACC, y = Score,
#                                color=SubjGroup, shape=SubjGroup)) +
#   facet_grid(~RST_Type)+
#   theme_default() + 
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         legend.position = "none")+
#   scale_colour_grey(start = 0.5, end = 0)+
#   geom_point(data = RSTz_bACCbRT_6, aes(color = SubjGroup), size=2) +
#   stat_smooth(data = RSTz_bACCbRT_6, aes(color = SubjGroup), method = "lm", se = F)+
#   scale_shape_manual(values=c(1,19))+
#   xlab(" ")+
#   ggtitle(paste0("(A) Both ACC"))
# 
# RSTz_bACCbRT_6 <- melt(RST[,c(1:4,9,10:15)], 
#                        id.vars = c("Subj","Age","SubjGroup","Both_ACC","None_RT"))
# colnames(RSTz_bACCbRT_6)[6:7] <- c("RST_Type","Score")
# 
# ggplot(data= RSTz_bACCbRT_6, aes(x = None_RT, y = Score,
#                                  color=SubjGroup, shape=SubjGroup)) +
#   facet_grid(~RST_Type)+
#   theme_default() + 
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         legend.position = "none")+
#   scale_colour_grey(start = 0.5, end = 0)+
#   geom_point(data = RSTz_bACCbRT_6, aes(color = SubjGroup), size=2) +
#   stat_smooth(data = RSTz_bACCbRT_6, aes(color = SubjGroup), method = "lm", se = F)+
#   scale_shape_manual(values=c(1,19))+
#   xlab(" ")+
#   ggtitle(paste0("(A) None RT"))


# RSTz_bACCbRT <- melt(RST[,c(1:4,7,25:27)], 
#                      id.vars = c("Subj","Age","SubjGroup","Both_ACC","Both_RT"))
# colnames(RSTz_bACCbRT)[6:7] <- c("RST_Type","Score")
# RSTz_bACCbRT$Both_RT <- RSTz_bACCbRT$Both_RT/1000
# head(RSTz_bACCbRT)
# 
# 
# bACC <- ggplot(data= RSTz_bACCbRT, aes(x = Both_ACC, y = Score,
#                                        color=SubjGroup, shape=SubjGroup)) +
#   facet_grid(~RST_Type)+
#   theme_default() + 
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         legend.position = "none")+
#   scale_colour_grey(start = 0.5, end = 0)+
#   geom_point(data = RSTz_bACCbRT, aes(color = SubjGroup), size=2) +
#   stat_smooth(data = RSTz_bACCbRT, aes(color = SubjGroup), method = "lm", se = F)+
#   scale_shape_manual(values=c(1,19))+
#   xlab(" ")+
#   ggtitle(paste0("(A) Both ACC"))
# 
# bRT <- ggplot(data= RSTz_bACCbRT, aes(x = Both_RT, y = Score,
#                                       color=SubjGroup, shape=SubjGroup)) +
#   facet_grid(~RST_Type)+
#   theme_default() + 
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         legend.position = "none")+
#   scale_colour_grey(start = 0.5, end = 0)+
#   geom_point(data = RSTz_bACCbRT, aes(color = SubjGroup), size=2) +
#   stat_smooth(data = RSTz_bACCbRT, aes(color = SubjGroup), method = "lm", se = F)+
#   scale_shape_manual(values=c(1,19))+
#   xlab(" ")+
#   ggtitle(paste0("(B) Both RT"))
# 
# dk <- ggplot(data= RSTz_bACCbRT, aes(x = Both_RT, y = Score,
#                                      color=SubjGroup, shape=SubjGroup)) +
#   facet_grid(~RST_Type)+
#   theme_default() + 
#   scale_colour_grey(start = 0.5, end = 0)+
#   geom_point(data = RSTz_bACCbRT, aes(color = SubjGroup), size=2) +
#   stat_smooth(data = RSTz_bACCbRT, aes(color = SubjGroup), method = "lm", se = F)+
#   scale_shape_manual(values=c(1,19))+
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         legend.position = "top")
# 
# dk <- g_legend(dk)
# 
# pdf("Output/RST_DanKyu.pdf", width = 7,height = 7)
# grid.arrange(arrangeGrob(bACC,
#                          bRT,
#                          nrow=2),
#              dk, nrow=2,heights=c(10, 1))
# dev.off()