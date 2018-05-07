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
          axis.line = element_line(colour = "black")
    )
}
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
## Read Data
GoCog<-read.csv("../GoCogdata/GoCog_20180404.csv", h=T)
colnames(GoCog)[6:9] <- c("Both_ACC","Both_RT","Go_RT","Cog_RT")

##
GoCog$Subj<-as.factor(GoCog$Subj)
head(GoCog)
str(GoCog)
GoCog$Both_RT <- GoCog$Both_RT/1000
GoCog$Go_RT <- GoCog$Go_RT/1000
GoCog$Cog_RT <- GoCog$Cog_RT/1000
GoCog$Both_ACC <- GoCog$Both_ACC*100
GoCog$GoStage <- factor(GoCog$GoStage, levels=c("Open", "Mid", "End"), 
                        labels = c("佈\n局","中\n盤","官\n子"))
GoCog$CogTask<- factor(GoCog$CogTask, levels=c("None", "Spat", "Reas", "Calc"),
                       labels = c("無\n干\n擾","空\n間\n干\n擾","推\n理\n干\n擾","計\n算\n干\n擾"))
GoCog$SubjGroup <- factor(GoCog$SubjGroup, levels=c("Dan", "Kyu"),
                          labels = c("段位","級位"))
########################  Overall ########################  
## MainEffect Plot
# GoStagebACC <- ggplot(data = GoCog, aes(x = GoStage, y =  Both_ACC))+
#   #scale_colour_grey(start = 0.5, end = 0)+
#   stat_summary(fun.y = mean, geom ="point", size = 2, shape = 19) +
#   stat_summary(fun.data = mean_se, geom = "errorbar", linetype = "solid", width = .1)+
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         text = element_text(family = 'BiauKai'),
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8)
#         )+
#   ylab("正\n確\n率\n(%)")+
#   xlab(" ")+
#   coord_cartesian(ylim=c(35,90))+
#   ggtitle(paste0("(A) 三階段圍棋題目: 正確率"))

# GoStageGRT <- ggplot(data = GoCog, aes(x = GoStage, y =  Go_RT))+
#   #scale_colour_grey(start = 0.5, end = 0)+
#   stat_summary(fun.y = mean, geom ="point", size = 2, shape = 1, col = "#666666") +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .1,col = "#666666")+
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
#         text = element_text(family = 'BiauKai'))+
#   ylab("反\n應\n時\n間\n(秒)")+
#   xlab(" ")+
#   coord_cartesian(ylim=c(5,15))+
#   ggtitle(paste0("(B) 三階段圍棋題目: 圍棋作業反應時間"))

# GoStageCRT <- ggplot(data = GoCog, aes(x = GoStage, y =  Cog_RT))+
#   #scale_colour_grey(start = 0.5, end = 0)+
#   stat_summary(fun.y = mean, geom ="point", size = 2, shape = 19) +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .1)+
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
#         text = element_text(family = 'BiauKai'))+
#   ylab("反\n應\n時\n間\n(秒)")+
#   xlab(" ")+
#   coord_cartesian(ylim=c(5,15))+
#   ggtitle(paste0("(C) 三階段圍棋題目: 認知干擾反應時間"))

GoStagebRT <- ggplot(data = GoCog, aes(x = GoStage, y =  Both_RT))+
  #scale_colour_grey(start = 0.5, end = 0)+
  stat_summary(fun.y = mean, geom ="point", size = 2, shape = 19) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
        axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
        text = element_text(family = 'BiauKai'))+
  ylab("反\n應\n時\n間\n(秒)")+
  xlab(" ")+
  coord_cartesian(ylim=c(10,25))+
  #ggtitle(paste0("(D) 三階段圍棋題目: 總反應時間"))+
  #ggtitle(paste0("(B) 三階段圍棋題目: 反應時間"))+
  ggtitle(paste0("(A) 三階段圍棋題目: 反應時間"))
annotation_df <- data.frame(start=c("佈\n局","佈\n局","中\n盤"),
                            #start=c("無\n干\n擾","無\n干\n擾","無\n干\n擾"),
                            #end=c("空\n間\n干\n擾","空\n間\n干\n擾","空\n間\n干\n擾"),
                            end=c("中\n盤","官\n子","官\n子"),
                            y=c(21,23,25),
                            label=c("*", "NS.", "*"))
GoStagebRT <- GoStagebRT+geom_signif(data=annotation_df,aes(xmin=start, xmax=end, annotations=label, y_position=y),
                         textsize = 3, vjust = 0,manual=TRUE)

# CogTaskbACC <- ggplot(data = GoCog, aes(x = CogTask, y =  Both_ACC))+
#   #scale_colour_grey(start = 0.5, end = 0)+
#   stat_summary(fun.y = mean, geom ="point", size = 2, shape = 19) +
#   stat_summary(fun.data = mean_se, geom = "errorbar", 
#                linetype = "solid", width = .1)+
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
#         text = element_text(family = 'BiauKai'))+
#   ylab("正\n確\n率\n(%)")+
#   xlab(" ")+
#   coord_cartesian(ylim=c(35,90))+
#   #ggtitle(paste0("(E) 四種認知干擾: 正確率"))+
#   ggtitle(paste0("(C) 四種認知干擾: 正確率"))

# CogTaskGRT <- ggplot(data = GoCog, aes(x = CogTask, y =  Go_RT))+
#   #scale_colour_grey(start = 0.5, end = 0)+
#   stat_summary(fun.y = mean, geom ="point", size = 2, shape = 1,col = "#666666") +
#   stat_summary(fun.data = mean_se, geom = "errorbar", 
#                linetype = "solid", width = .1,col = "#666666")+
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
#         text = element_text(family = 'BiauKai'))+
#   ylab("反\n應\n時\n間\n(秒)")+
#   xlab(" ")+
#   coord_cartesian(ylim=c(5,15))+
#   ggtitle(paste0("(F) 四種認知干擾: 圍棋作業反應時間"))

# CogTaskCRT <- ggplot(data = GoCog, aes(x = CogTask, y =  Cog_RT))+
#   #scale_colour_grey(start = 0.5, end = 0)+
#   stat_summary(fun.y = mean, geom ="point", size = 2, shape = 19) +
#   stat_summary(fun.data = mean_se, geom = "errorbar", 
#                linetype = "solid", width = .1)+
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
#         text = element_text(family = 'BiauKai'))+
#   ylab("反\n應\n時\n間\n(秒)")+
#   xlab(" ")+
#   coord_cartesian(ylim=c(5,15))+
#   ggtitle(paste0("(G) 四種認知干擾: 認知干擾反應時間"))

CogTaskbRT <- ggplot(data = GoCog, aes(x = CogTask, y =  Both_RT))+
  #scale_colour_grey(start = 0.5, end = 0)+
  stat_summary(fun.y = mean, geom ="point", size = 2, shape = 1,col = "#666666") +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               linetype = "solid", width = .1,col = "#666666")+
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
        axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
        text = element_text(family = 'BiauKai'))+
  ylab("反\n應\n時\n間\n(秒)")+
  xlab(" ")+
  coord_cartesian(ylim=c(10,30))+
  #ggtitle(paste0("(H) 四種認知干擾: 總反應時間"))+
  #ggtitle(paste0("(D) 四種認知干擾: 反應時間"))+
  ggtitle(paste0("(B) 四種認知干擾: 反應時間"))

# annotation_df <- data.frame(start=c("無\n干\n擾","無\n干\n擾","無\n干\n擾"), 
#                             end=c("空\n間\n干\n擾","推\n理\n干\n擾","計\n算\n干\n擾"),
#                             y=c(21,25,29),
#                             label=c("*", "*", "*"))
# CogTaskbRT <- CogTaskbRT+geom_signif(data=annotation_df,aes(xmin=start, xmax=end, annotations=label, y_position=y),
#                         textsize = 3, vjust = 0,manual=TRUE)
ann_text1 <- data.frame(CogTask = c(2,3,4),Both_RT = c(18,25,18),label = c("*","*","*"))
CogTaskbRT<- CogTaskbRT+geom_text(aes(x = CogTask, y =  Both_RT, label =  label), data = ann_text1)

# tiff(file = "../GoCog_Manuscript/FigureTable/圖3.tiff",height=6, width=12, units="in", res = 300,compression = "lzw")
# grid.arrange(GoStagebACC,GoStageGRT,GoStageCRT,GoStagebRT,
#              CogTaskbACC,CogTaskGRT,CogTaskCRT,CogTaskbRT,ncol =4)
# dev.off()

# tiff(file = "../GoCog_Manuscript/FigureTable/圖3.tiff",height=6, width=6, units="in", res = 300,compression = "lzw")
# grid.arrange(GoStagebACC,GoStagebRT,
#              CogTaskbACC,CogTaskbRT,ncol = 2)
# dev.off()
# 
# tiff(file = "../GoCog_Manuscript/FigureTable/圖4_反應時間.tiff",height=4, width=6, units="in", res = 300,compression = "lzw")
# grid.arrange(GoStagebRT,
#              CogTaskbRT,ncol = 2)
# dev.off()

png(file = "../GoCog_Manuscript/FigureTable/圖4_反應時間.png",height=4, width=6, units="in", res = 300)
grid.arrange(GoStagebRT,CogTaskbRT,ncol = 2)
dev.off()


## Interaction Plot: Type1
GoCog$GoStage <- factor(GoCog$GoStage, labels = c("佈局","中盤","官子"))
bACC <- ggplot(data = GoCog, aes(x = CogTask, y = Both_ACC, group = GoStage)) +
  scale_colour_grey(start = 0.5, end = 0)+
  facet_grid(.~GoStage) +
  stat_summary(fun.y = mean, geom = "point", size = 2, shape = 19) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .2) +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
        axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
        text = element_text(family = 'BiauKai'))+
  ylab("正\n確\n率\n(%)")+
  xlab(" ")+
  coord_cartesian(ylim=c(35,100))+
  ggtitle(paste0("(A) 給定一圍棋階段探討四種認知干擾之差異分析"))
# annotation_df <- data.frame(GoStage=c("佈局","中盤","官子"), 
#                            start=c("無\n干\n擾","無\n干\n擾","無\n干\n擾"), 
#                            end=c("空\n間\n干\n擾","空\n間\n干\n擾","空\n間\n干\n擾"),
#                            y=c(85,85,85),
#                            label=c("*", "NS.", "NS."))
# annotation_df2 <- data.frame(GoStage=c("佈局","中盤","官子"), 
#                             start=c("無\n干\n擾","無\n干\n擾","無\n干\n擾"), 
#                             end=c("推\n理\n干\n擾","推\n理\n干\n擾","推\n理\n干\n擾"),
#                             y=c(90,90,90),
#                             label=c("NS.", "*", "*"))
# annotation_df3 <- data.frame(GoStage=c("佈局","中盤","官子"), 
#                              start=c("無\n干\n擾","無\n干\n擾","無\n干\n擾"), 
#                              end=c("計\n算\n干\n擾","計\n算\n干\n擾","計\n算\n干\n擾"),
#                              y=c(95,95,95),
#                              label=c("NS.", "NS.", "NS."))
# bACC <- bACC+geom_signif(data=annotation_df,aes(xmin=start, xmax=end, annotations=label, y_position=y),
#                  textsize = 3, vjust = 0,manual=TRUE)+
#   geom_signif(data=annotation_df2,aes(xmin=start, xmax=end, annotations=label, y_position=y),
#               textsize = 3, vjust = 0,manual=TRUE)+
#   geom_signif(data=annotation_df3,aes(xmin=start, xmax=end, annotations=label, y_position=y),
#               textsize = 3, vjust = 0,manual=TRUE)

ann_text <- data.frame(CogTask = c(2,3,3),Both_ACC = c(60,60,60),label = c("*","*","*"),
                       GoStage = factor(c("佈局","中盤","官子"),levels = c("佈局","中盤","官子")))
bACC <- bACC+geom_text(aes(x = CogTask, y = Both_ACC, label =  label), data = ann_text)






# #BothRT(by Stage)
# GRT <- ggplot(data = GoCog, aes(x = CogTask, y = Go_RT, group = GoStage)) +
#   scale_colour_grey(start = 0.5, end = 0)+
#   facet_grid(.~GoStage) +
#   stat_summary(fun.y = mean, geom = "point", size = 2,shape = 1, col = "#666666") +
#   stat_summary(fun.y = mean, geom = "line", col = "#666666") +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2, col = "#666666") +
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
#         text = element_text(family = 'BiauKai'))+
#   ylab("反\n應\n時\n間\n(秒)")+
#   xlab(" ")+
#   coord_cartesian(ylim=c(5,15))+
#   ggtitle(paste0("(B) 圍棋題目反應時間"))

# CRT <- ggplot(data = GoCog, aes(x = CogTask, y = Cog_RT, group = GoStage)) +
#   scale_colour_grey(start = 0.5, end = 0)+
#   facet_grid(.~GoStage) +
#   stat_summary(fun.y = mean, geom = "point", size = 2,shape = 19) +
#   stat_summary(fun.y = mean, geom = "line") +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2) +
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
#         text = element_text(family = 'BiauKai'))+
#   ylab("反\n應\n時\n間\n(秒)")+
#   xlab(" ")+
#   coord_cartesian(ylim=c(5,15))+
#   ggtitle(paste0("(C) 認知干擾反應時間"))

bRT <- ggplot(data = GoCog, aes(x = CogTask, y = Both_RT, group = GoStage)) +
  scale_colour_grey(start = 0.5, end = 0)+
  facet_grid(.~GoStage) +
  # stat_summary(fun.y = mean, geom = "point", size = 2,shape = 1, col = "#666666") +
  # stat_summary(fun.y = mean, geom = "line", col = "#666666") +
  # stat_summary(fun.data = mean_se, geom = "errorbar",
  #              linetype = "solid", width = .2, col = "#666666") +
  stat_summary(fun.y = mean, geom = "point", size = 2, shape = 19) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .2) +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
        axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
        text = element_text(family = 'BiauKai'))+
  ylab("反\n應\n時\n間\n(秒)")+
  xlab(" ")+
  #ggtitle(paste0("(D) 總反應時間"))+
  ggtitle(paste0("(A) 給定一圍棋階段探討四種認知干擾之差異分析"))

# tiff(file = "../GoCog_Manuscript/FigureTable/圖4.tiff",height=12, width=6, units="in", res = 300,compression = "lzw")
# grid.arrange(bACC,GRT,CRT,bRT,ncol =1)
# dev.off()

## Interaction Plot: Type2
GoCog$CogTask <- factor(GoCog$CogTask,labels = c("無干擾","空間干擾","推理干擾","計算干擾"))
GoCog$GoStage <- factor(GoCog$GoStage,labels = c("佈\n局","中\n盤","官\n子"))

bACC2 <- ggplot(data = GoCog, aes(x = GoStage, y = Both_ACC, group = CogTask)) +
  scale_colour_grey(start = 0.5, end = 0)+
  facet_grid(.~CogTask) +
  # stat_summary(fun.y = mean, geom = "point", size = 2, shape = 19) +
  # stat_summary(fun.y = mean, geom = "line") +
  # stat_summary(fun.data = mean_se, geom = "errorbar",
  #              linetype = "solid", width = .2) +
  stat_summary(fun.y = mean, geom = "point", size = 2,shape = 1, col = "#666666") +
  stat_summary(fun.y = mean, geom = "line", col = "#666666") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .2, col = "#666666") +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
        axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
        text = element_text(family = 'BiauKai'))+
  ylab("正\n確\n率\n(%)")+
  xlab(" ")+
  coord_cartesian(ylim=c(35,90))+
  ggtitle(paste0("(B) 給定一認知干擾作業探討三階段圍棋題目之差異分析"))
annotation_df_2 <- data.frame(CogTask=c("空間干擾"), start=c("佈\n局"), end=c("中\n盤"),
                              y=c(75),label=c("*"))
annotation_df_22 <- data.frame(CogTask=c("空間干擾"), start=c("佈\n局"), end=c("官\n子"),
                              y=c(85),label=c("*"))
annotation_df_23 <- data.frame(CogTask=c("空間干擾"), start=c("中\n盤"), end=c("官\n子"),
                              y=c(80),label=c("NS."))

bACC2 <- bACC2+geom_signif(data=annotation_df_2,aes(xmin=start, xmax=end, annotations=label, y_position=y),
                         textsize = 3, vjust = 0,manual=TRUE)+
  geom_signif(data=annotation_df_22,aes(xmin=start, xmax=end, annotations=label, y_position=y),
                                                                          textsize = 3, vjust = 0,manual=TRUE)+
  geom_signif(data=annotation_df_23,aes(xmin=start, xmax=end, annotations=label, y_position=y),
              textsize = 3, vjust = 0,manual=TRUE)



bRT2 <- ggplot(data = GoCog, aes(x = GoStage, y = Both_RT, group = CogTask)) +
  scale_colour_grey(start = 0.5, end = 0)+
  facet_grid(.~CogTask) +
  stat_summary(fun.y = mean, geom = "point", size = 2,shape = 1, col = "#666666") +
  stat_summary(fun.y = mean, geom = "line", col = "#666666") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .2, col = "#666666") +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
        axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
        text = element_text(family = 'BiauKai'))+
  ylab("反\n應\n時\n間\n(秒)")+
  xlab(" ")+
  #ggtitle(paste0("(D) 總反應時間"))+
  ggtitle(paste0("(B) 給定一認知干擾作業探討三階段圍棋題目之差異分析"))

# tiff(file = "../GoCog_Manuscript/FigureTable/圖4.tiff",height=12, width=6, units="in", res = 300,compression = "lzw")
# grid.arrange(bACC,GRT,CRT,bRT,ncol =1)
# dev.off()

# tiff(file = "../GoCog_Manuscript/FigureTable/圖3_正確率.tiff",height=6, width=6, units="in", res = 300,compression = "lzw")
# grid.arrange(bACC,bACC2,ncol =1)
# dev.off()

png(file = "../GoCog_Manuscript/FigureTable/圖3_正確率.png",height=6, width=6, units="in", res = 300)
grid.arrange(bACC,bACC2,ncol =1)
dev.off()

# tiff(file = "../GoCog_Manuscript/FigureTable/圖5_反應時間.tiff",height=6, width=6, units="in", res = 300,compression = "lzw")
# grid.arrange(bRT,bRT2,ncol =1)
# dev.off()





# ############ Dan Kyu: Main and Interaction ############
# 
# ## MainEffect Plot
# GoStagebACC <- ggplot(data = GoCog, aes(x = GoStage, y =  Both_ACC,
#                                         shape = SubjGroup,
#                                         color = SubjGroup)) +
#   scale_colour_grey(start = 0.5, end = 0)+
#   stat_summary(fun.y = mean, geom ="point", size = 2, position=position_dodge(width=0.3)) +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .1,position=position_dodge(width=0.3)) +
#   scale_shape_manual(values=c(1,19)) +
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),legend.position = "none",
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 60, vjust = 0.5, size = 8),
#         text = element_text(family = 'BiauKai'))+
#   ylab("正\n確\n率\n(%)")+
#   xlab(" ")+
#   coord_cartesian(ylim=c(35,90))+
#   ggtitle(paste0("(A) 三階段圍棋題目: 正確率"))
# 
# GoStagebRT <- ggplot(data = GoCog,  aes(x = GoStage, y =  Both_RT,
#                                         shape = SubjGroup,
#                                         color = SubjGroup)) +
#   scale_colour_grey(start = 0.5, end = 0)+
#   stat_summary(fun.y = mean, geom ="point", size = 2, position=position_dodge(width=0.3)) +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .1,position=position_dodge(width=0.3)) +
#   scale_shape_manual(values=c(1,19)) +
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),
#       ######
#       legend.position = c(0.83,0.83),
#       #######
#       legend.title=element_blank(),
#       axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#       axis.text.x  = element_text(angle = 60, vjust = 0.5, size = 8),
#       text = element_text(family = 'BiauKai'))+ 
# ylab("反\n應\n時\n間\n(秒)")+
#   xlab(" ")+
#   coord_cartesian(ylim=c(10,25))+
#   ggtitle(paste0("(B) 三階段圍棋題目: 反應時間"))
# 
# CogTaskbACC <- ggplot(data = GoCog,  aes(x = CogTask, y =  Both_ACC,
#                                          shape = SubjGroup,
#                                          color = SubjGroup)) +
#   scale_colour_grey(start = 0.5, end = 0)+
#   stat_summary(fun.y = mean, geom ="point", size = 2, position=position_dodge(width=0.3)) +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .1,position=position_dodge(width=0.3)) +
#   scale_shape_manual(values=c(1,19)) +
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),legend.position = "none",
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 60, vjust = 0.5, size = 8),
#         text = element_text(family = 'BiauKai'))+
#   ylab("正\n確\n率\n(%)")+
#   xlab(" ")+
#   coord_cartesian(ylim=c(35,90))+
#   ggtitle(paste0("(C) 四種認知干擾: 正確率"))
# 
# CogTaskbRT <- ggplot(data = GoCog,  aes(x = CogTask, y =  Both_RT,
#                                         shape = SubjGroup,
#                                         color = SubjGroup)) +
#   scale_colour_grey(start = 0.5, end = 0)+
#   stat_summary(fun.y = mean, geom ="point", size = 2, position=position_dodge(width=0.3)) +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .1,position=position_dodge(width=0.3)) +
#   scale_shape_manual(values=c(1,19)) +
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),legend.position = "none",
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 60, vjust = 0.5, size = 8),
#         text = element_text(family = 'BiauKai'))+
#   ylab("反\n應\n時\n間\n(秒)")+
#   xlab(" ")+
#   coord_cartesian(ylim=c(10,25))+
#   ggtitle(paste0("(D) 四種認知干擾: 反應時間"))
# 
# tiff(file = "../GoCog_Manuscript/FigureTable/圖5.tiff",height=6, width=6, units="in", res = 300,compression = "lzw")
# grid.arrange(GoStagebACC,GoStagebRT,CogTaskbACC,CogTaskbRT,ncol =2)
# dev.off()
# ## Interaction Plot
# 
# #BothACC(by Stage)
# bACC <- ggplot(data = GoCog, aes(x = CogTask, y = Both_ACC, group = SubjGroup, 
#                                  color = SubjGroup, shape=SubjGroup)) +
#   scale_colour_grey(start = 0.5, end = 0)+
#   facet_grid(.~GoStage) +
#   stat_summary(fun.y = mean, geom = "point", size = 2,
#                position=position_dodge(width=0.3)) +
#   stat_summary(fun.y = mean, geom = "line",
#                position=position_dodge(width=0.3)) +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2,
#                position=position_dodge(width=0.3)) +
#   scale_shape_manual(values=c(1,19)) +
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         legend.position = "none",
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 60, vjust = 0.5, size = 8),
#         text = element_text(family = 'BiauKai'))+
#   ylab("正\n確\n率\n(%)")+
#   xlab(" ")+
#   ggtitle(paste0("(A) 正確率"))
# 
# #BothRT(by Stage)
# bRT <- ggplot(data = GoCog, aes(x = CogTask, y = Both_RT, group = SubjGroup, 
#                                 color = SubjGroup, shape=SubjGroup)) +
#   scale_colour_grey(start = 0.5, end = 0)+
#   facet_grid(.~GoStage) +
#   stat_summary(fun.y = mean, geom = "point", size = 2,
#                position=position_dodge(width=0.3)) +
#   stat_summary(fun.y = mean, geom = "line",
#                position=position_dodge(width=0.3)) +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2,
#                position=position_dodge(width=0.3)) +
#   scale_shape_manual(values=c(1,19)) +
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         legend.position = "none",
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 60, vjust = 0.5, size = 8),
#         text = element_text(family = 'BiauKai'))+
#   ylab("反\n應\n時\n間\n(秒)")+
#   xlab(" ")+
#   ggtitle(paste0("(B) 反應時間"))
# 
# dk <- ggplot(data = GoCog, aes(x = CogTask, y = Both_RT, group = SubjGroup, 
#                                color = SubjGroup, shape=SubjGroup)) +
#   scale_colour_grey(start = 0.5, end = 0)+
#   facet_grid(.~GoStage) +
#   stat_summary(fun.y = mean, geom = "point", size = 2,
#                position=position_dodge(width=0.3)) +
#   stat_summary(fun.y = mean, geom = "line",
#                position=position_dodge(width=0.3)) +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2,
#                position=position_dodge(width=0.3)) +
#   scale_shape_manual(values=c(1,19)) +
#   theme_default()+
#   theme(plot.title = element_text(hjust = 0,size = 10),
#         legend.position = "right",
#         legend.title=element_blank(),
#         axis.title.y  = element_text(angle = 0, vjust = 0.5, size = 8),
#         axis.text.x  = element_text(angle = 60, vjust = 0.5, size = 8),
#         text = element_text(family = 'BiauKai'))
# 
# dk <- g_legend(dk)
# 
# tiff(file = "../GoCog_Manuscript/FigureTable/圖6.tiff",height=6, width=7, units="in", res = 300,compression = "lzw")
# grid.arrange(arrangeGrob(bACC,
#                          bRT,
#                          nrow=2),
#              dk, nrow=1,widths=c(10, 1))
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# # 
# # 
# # #########################################################################################
# # #Interaction_BothACC
# # png( filename = "In_BothACC.png", width = 1200, height = 600)
# # ggplot(data = GoCog, aes(x = CogTask, y = Both_ACC, group = GoStage, 
# #                          color = GoStage, shape=GoStage)) +
# #   facet_grid(.~SubjGroup) +
# #   stat_summary(fun.y = mean, geom = "point", size = 4) +
# #   stat_summary(fun.y = mean, geom = "line") +
# #   stat_summary(fun.data = mean_se, geom = "errorbar",
# #                linetype = "solid", width = .2) +
# #   labs(x = "CogTask", y = "ACC")
# # dev.off()
# # 
# # #Interaction_BothRT
# # png( filename = "In_BothRT.png", width = 1200, height = 600)
# # ggplot(data = GoCog, aes(x = CogTask, y = Both_RT, group = GoStage, 
# #                          color = GoStage, shape=GoStage)) +
# #   facet_grid(.~SubjGroup) +
# #   stat_summary(fun.y = mean, geom = "point", size = 4) +
# #   stat_summary(fun.y = mean, geom = "line") +
# #   stat_summary(fun.data = mean_se, geom = "errorbar",
# #                linetype = "solid", width = .2) +
# #   labs(x = "CogTask", y = "RT")
# # dev.off()
# # 
# # 
# # ####################################################################################
# # 
# #   
# #   #GoStage_GoACC
# #   png( filename = "P_GoACC.png", width = 600, height = 600)
# # ggplot(data = GoCog, aes(x = GoStage, y = Go_ACC, color = SubjGroup)) +
# #   stat_summary(fun.y = mean, geom = "point", size = 4) +
# #   stat_summary(fun.data = mean_se, geom = "errorbar",
# #                linetype = "solid", width = .2) +
# #   labs(x = "GoStage", y = "ACC", title="Go_ACC")
# # 
# # dev.off()
# # 
# # #GoStage_GoRT
# # png( filename = "P_GoRT.png", width = 600, height = 600)
# # ggplot(data = GoCog, aes(x = GoStage, y = Go_RT, color = SubjGroup)) +
# #   stat_summary(fun.y = mean, geom = "point", size = 4) +
# #   stat_summary(fun.data = mean_se, geom = "errorbar",
# #                linetype = "solid", width = .2) +
# #   labs(x = "GoStage", y = "RT", title="Go_ACC")
# # dev.off()
# # 
# # #Interaction_GoACC
# # png( filename = "In_GoACC.png", width = 1200, height = 600)
# # ggplot(data = GoCog, aes(x = CogTask, y = Go_ACC, group = GoStage, 
# #                          color = GoStage, shape=GoStage)) +
# #   facet_grid(.~SubjGroup) +
# #   stat_summary(fun.y = mean, geom = "point", size = 4) +
# #   stat_summary(fun.y = mean, geom = "line") +
# #   stat_summary(fun.data = mean_se, geom = "errorbar",
# #                linetype = "solid", width = .2) +
# #   labs(x = "CogTask", y = "ACC", title="Go_ACC")
# # dev.off()
# # 
# # #Interaction_GoRT
# # png( filename = "In_GoRT.png", width = 1200, height = 600)
# # ggplot(data = GoCog, aes(x = CogTask, y = Go_RT, group = GoStage, 
# #                          color = GoStage, shape=GoStage)) +
# #   facet_grid(.~SubjGroup) +
# #   stat_summary(fun.y = mean, geom = "point", size = 4) +
# #   stat_summary(fun.y = mean, geom = "line") +
# #   stat_summary(fun.data = mean_se, geom = "errorbar",
# #                linetype = "solid", width = .2) +
# #   labs(x = "CogTask", y = "RT", title="Go_ACC")
# # dev.off()
# # 
# # #Interaction_GoACC(by Stage)
# # png( filename = "Stage_GoACC.png", width = 1200, height = 600)
# # ggplot(data = GoCog, aes(x = CogTask, y = Go_ACC, group = SubjGroup, 
# #                          color = SubjGroup, shape=SubjGroup)) +
# #   facet_grid(.~GoStage) +
# #   stat_summary(fun.y = mean, geom = "point", size = 4) +
# #   stat_summary(fun.y = mean, geom = "line") +
# #   stat_summary(fun.data = mean_se, geom = "errorbar",
# #                linetype = "solid", width = .2) +
# #   labs(x = "CogTask", y = "ACC", title="Go_ACC")
# # dev.off()
# # 
# # #Interaction_GoACC(by Stage)
# # png( filename = "Stage_GoRT.png", width = 1200, height = 600)
# # ggplot(data = GoCog, aes(x = CogTask, y = Go_RT, group = SubjGroup, 
# #                          color = SubjGroup, shape=SubjGroup)) +
# #   facet_grid(.~GoStage) +
# #   stat_summary(fun.y = mean, geom = "point", size = 4) +
# #   stat_summary(fun.y = mean, geom = "line") +
# #   stat_summary(fun.data = mean_se, geom = "errorbar",
# #                linetype = "solid", width = .2) +
# #   labs(x = "CogTask", y = "RT", title="Go_ACC")
# # dev.off()
# # 
# # #Interaction_BothACC_overall
# # png( filename = "In_BothACC_overall.png", width = 600, height = 600)
# # ggplot(data = GoCog, aes(x = CogTask, y = Both_ACC, group = GoStage, 
# #                          color = GoStage, shape=GoStage)) +
# #   stat_summary(fun.y = mean, geom = "point", size = 4) +
# #   stat_summary(fun.y = mean, geom = "line") +
# #   stat_summary(fun.data = mean_se, geom = "errorbar",
# #                linetype = "solid", width = .2) +
# #   labs(x = "CogTask", y = "ACC")
# # dev.off()
# # 
# # #################################################################################################
# # GoCog<-read.csv("GoCog.csv", h=T)
# # GoCog<-GoCog[-which(GoCog$CogTask=="None"),]
# # 
# # 
# # GoCog$GoStage <- factor(GoCog$GoStage, levels=c("Open", "Mid", "End"))
# # GoCog$CogTask<- factor(GoCog$CogTask, levels=c( "Calc", "Spat", "Reas"))
# # GoCog$SubjGroup<- factor(GoCog$SubjGroup, levels=c("?~?l?Ŧ?(Kyu)", "?~?l?q??(Dan)"))
# # GoCog$SubjRank <- factor(GoCog$SubjRank, levels = c("?~?l????", "?~?l?A??", "?~?l?Ҳ?", "?~?l?@?q", 
# #                                                     "?~?l?G?q", "?~?l?T?q", "?~?l?|?q", "?~?l???q", "?~?l???q"))
# # 
# # #Interaction_BothACC(by Stage)
# # png( filename = "Stage_CogACC.png", width = 1200, height = 600)
# # ggplot(data = GoCog, aes(x = GoStage, y = Cog_ACC, group = SubjGroup, 
# #                          color = SubjGroup, shape=SubjGroup)) +
# #   facet_grid(.~CogTask) +
# #   stat_summary(fun.y = mean, geom = "point", size = 4) +
# #   stat_summary(fun.y = mean, geom = "line") +
# #   stat_summary(fun.data = mean_se, geom = "errorbar",
# #                linetype = "solid", width = .2) +
# #   labs(x = "GoStage", y = "CogACC")
# # dev.off()
# # 
# # 
# # #Interaction_BothACC(by Stage)
# # png( filename = "Stage_CogRT.png", width = 1200, height = 600)
# # ggplot(data = GoCog, aes(x = GoStage, y = Cog_RT, group = SubjGroup, 
# #                          color = SubjGroup, shape=SubjGroup)) +
# #   facet_grid(.~CogTask) +
# #   stat_summary(fun.y = mean, geom = "point", size = 4) +
# #   stat_summary(fun.y = mean, geom = "line") +
# #   stat_summary(fun.data = mean_se, geom = "errorbar",
# #                linetype = "solid", width = .2) +
# #   labs(x = "GoStage", y = "CogRT")
# # dev.off()
