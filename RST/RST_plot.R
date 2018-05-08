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

# R square
# p value

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
RST<-read.csv("../GoCogdata/NoneRST_All.csv", h=T)[,c(1:3,18)]
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

Overall <- aggregate(data = RSTdata, cbind(Both_Acc,Both_RT,Total)~Subj,FUN = mean)
summary(lm.beta(lm(Both_Acc~Total,data = Overall)))
#lb1 <- paste("r ==", 0.371)
OACC <- ggplot(data= Overall, aes(x = Both_Acc, y = Total)) +
  theme_default()+
  theme(text = element_text(family = 'BiauKai'))+
  #scale_colour_grey(start = 0.5, end = 0)+
  geom_point(size=2) +
  stat_smooth(method = "lm", se = T, col = "grey")+
  annotate("text", x = 21, y = 90, label = "r", hjust = 0,fontface = "italic")+
  annotate("text", x = 25, y = 90, label = "=", hjust = 0,size = 3)+
  annotate("text", x = 29, y = 90, label = "0.371",hjust = 0)+
  annotate("text", x = 21, y = 85, label = "p",fontface = "italic", hjust = 0)+
  annotate("text", x = 25, y = 85, label = "=",hjust = 0,size = 3)+
  annotate("text", x = 29, y = 85, label = "0.07", hjust = 0)+
  #ggtitle(paste0("(A) 整體正確率"))+
  xlab("整體正確率(%)")+
  xlim(15,90)+
  ylim(30,105)+
  ylab("推\n理\n思\n考\n測\n驗\n總\n分")#+
OACC_yx <- ggplot(data= Overall, aes(x = Total, y = Both_Acc)) +
  theme_default()+
  theme(text = element_text(family = 'BiauKai'))+
  #scale_colour_grey(start = 0.5, end = 0)+
  geom_point(size=2) +
  stat_smooth(method = "lm", se = T, col = "grey")+
  annotate("text", x = 30, y = 90, label = "r", hjust = 0,fontface = "italic")+
  annotate("text", x = 34, y = 90, label = "=", hjust = 0,size = 3)+
  annotate("text", x = 38, y = 90, label = "0.371",hjust = 0)+
  annotate("text", x = 30, y = 85, label = "p",fontface = "italic", hjust = 0)+
  annotate("text", x = 34, y = 85, label = "=",hjust = 0,size = 3)+
  annotate("text", x = 38, y = 85, label = "0.07", hjust = 0)+
  #ggtitle(paste0("(A) 整體正確率"))+
  ylab("整\n體\n正\n確\n率\n(%)")+
  xlim(30,105)+
  ylim(15,90)+
  xlab("推理思考測驗總分")#+



# ACC <- ggplot(data= Reas, aes(x = Both_Acc, y = Total, 
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
CogTask <- aggregate(data = RSTdata, cbind(Both_Acc,Both_RT,Total)~Subj+CogTask,FUN = mean)
Reas2 <- filter(CogTask, CogTask == "推\n理\n干\n擾")
#lb2 <- paste("r == ", 0.661)
#plb2 <- paste("p", 0.2874)
overallACC <- ggplot(data= Reas2, aes(x = Both_Acc, y = Total)) +
  theme_default()+
  theme(text = element_text(family = 'BiauKai'))+
  #scale_colour_grey(start = 0.5, end = 0)+
  geom_point(size=2) +
  stat_smooth(method = "lm", se = T, col = "grey")+
  #ggtitle(paste0("(B) 推理干擾情境平均正確率"))+
  xlim(15,90)+
  ylim(30,105)+
  xlab("推理干擾情況下正確率(%)")+
  annotate("text", x = 21, y = 90, label = "r", hjust = 0,fontface = "italic")+
  annotate("text", x = 25, y = 90, label = "=", hjust = 0,size = 3)+
  annotate("text", x = 29, y = 90, label = "0.661",hjust = 0)+
  annotate("text", x = 21, y = 85, label = "p",fontface = "italic", hjust = 0)+
  annotate("text", x = 25, y = 85, label = "<",hjust = 0,size = 3)+
  annotate("text", x = 29, y = 85, label = "0.001", hjust = 0)+
  ylab(" ")

overallACC_yx <- ggplot(data= Reas2, aes(x = Total,y = Both_Acc)) +
  theme_default()+
  theme(text = element_text(family = 'BiauKai'))+
  #scale_colour_grey(start = 0.5, end = 0)+
  geom_point(size=2) +
  stat_smooth(method = "lm", se = T, col = "grey")+
  #ggtitle(paste0("(B) 推理干擾情境平均正確率"))+
  xlim(30,105)+
  ylim(15,90)+  
  xlab("推理思考測驗總分")+
  ylab("推\n理\n干\n擾\n情\n況\n下\n正\n確\n率\n(%)")+
  annotate("text", x = 30, y = 90, label = "r", hjust = 0,fontface = "italic")+
  annotate("text", x = 34, y = 90, label = "=", hjust = 0,size = 3)+
  annotate("text", x = 38, y = 90, label = "0.661",hjust = 0)+
  annotate("text", x = 30, y = 85, label = "p",fontface = "italic", hjust = 0)+
  annotate("text", x = 34, y = 85, label = "<",hjust = 0,size = 3)+
  annotate("text", x = 38, y = 85, label = "0.001", hjust = 0)

  #ylab("推\n理\n思\n考\n測\n驗\n總\n分")#+
#xlim(0,100)
#overallACC

# tiff(file = "../GoCog_Manuscript/FigureTable/圖8_RST.tiff",height=6, width=6, units="in", res = 300,compression = "lzw")
# grid.arrange(ACC,overallACC,ncol = 1, heights = c(4,6))
# dev.off()


png(file = "../GoCog_Manuscript/FigureTable/圖7_RST.png",height=3, width=5, units="in", res = 300)
grid.arrange(OACC,overallACC,ncol = 2)
dev.off()

png(file = "../GoCog_Manuscript/FigureTable/圖7_RST_v2.png",height=3, width=5, units="in", res = 300)
grid.arrange(OACC_yx,overallACC_yx,ncol = 2)
dev.off()


# tiff(file = "../GoCog_Manuscript/FigureTable/圖7_RST.tiff",height=3, width=5, units="in", res = 300,compression = "lzw")
# grid.arrange(OACC,overallACC,ncol = 2)
# dev.off()


#ACC
# TotalACC <- ggplot(data= RSTdata, aes(x = Both_Acc, y = Total)) +
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

# Verbal <- ggplot(data= RSTdata, aes(x = Both_Acc, y = Verbal)) +
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
# NonVerbal <- ggplot(data= RSTdata, aes(x = Both_Acc, y = Nonverbal)) +
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