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
## Read Data
GoCog<-read.csv("../GoCogdata/GoCog.csv", h=T)
GoCog$Subj<-as.factor(GoCog$Subj)
head(GoCog)
str(GoCog)
GoCog$Both_RT <- GoCog$Both_RT/1000
GoCog$GoStage <- factor(GoCog$GoStage, levels=c("Open", "Mid", "End"))
GoCog$CogTask<- factor(GoCog$CogTask, levels=c("None", "Spat", "Reas", "Calc"))

############ Overall ############ 

## MainEffect Plot
GoStagebACC <- ggplot(data = GoCog, aes(x = GoStage, y =  Both_ACC))+
  #scale_colour_grey(start = 0.5, end = 0)+
  stat_summary(fun.y = mean, geom ="point", size = 2, shape = 19) +
  stat_summary(fun.data = mean_se, geom = "errorbar", linetype = "solid", width = .1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10))+
  ylab("Accuracy (%)")+
  xlab(" ")+
  coord_cartesian(ylim=c(0.35,0.9))+
  ggtitle(paste0("(A) GoStage: Both ACC"))

GoStagebRT <- ggplot(data = GoCog, aes(x = GoStage, y =  Both_RT))+
  #scale_colour_grey(start = 0.5, end = 0)+
  stat_summary(fun.y = mean, geom ="point", size = 2, shape = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", linetype = "solid", width = .1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10))+
  ylab("RT (sec)")+
  xlab(" ")+
  coord_cartesian(ylim=c(10,24))+
  ggtitle(paste0("(B) GoStage: Both RT"))

CogTaskbACC <- ggplot(data = GoCog, aes(x = CogTask, y =  Both_ACC))+
  #scale_colour_grey(start = 0.5, end = 0)+
  stat_summary(fun.y = mean, geom ="point", size = 2, shape = 19) +
  stat_summary(fun.data = mean_se, geom = "errorbar", linetype = "solid", width = .1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10))+
  ylab("Accuracy (%)")+
  xlab(" ")+
  coord_cartesian(ylim=c(0.35,0.9))+
  ggtitle(paste0("(C) CogTask: Both ACC"))

CogTaskbRT <- ggplot(data = GoCog, aes(x = CogTask, y =  Both_RT))+
  #scale_colour_grey(start = 0.5, end = 0)+
  stat_summary(fun.y = mean, geom ="point", size = 2, shape = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", linetype = "solid", width = .1)+
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10))+
  ylab("RT (sec)")+
  xlab(" ")+
  coord_cartesian(ylim=c(10,24))+
  ggtitle(paste0("(D) CogTask: Both RT"))

pdf("Output/Aov3_Overall_MainEffect.pdf", width = 7,height = 7)
grid.arrange(GoStagebACC,GoStagebRT,CogTaskbACC,CogTaskbRT,ncol =2)
dev.off()



## Interaction Plot

#BothACC(by Stage)
bACC <- ggplot(data = GoCog, aes(x = CogTask, y = Both_ACC, group = SubjGroup, 
                                 color = SubjGroup, shape=SubjGroup)) +
  scale_colour_grey(start = 0.5, end = 0)+
  facet_grid(.~GoStage) +
  stat_summary(fun.y = mean, geom = "point", size = 2,
               position=position_dodge(width=0.3)) +
  stat_summary(fun.y = mean, geom = "line",
               position=position_dodge(width=0.3)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .2,
               position=position_dodge(width=0.3)) +
  scale_shape_manual(values=c(1,19)) +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        legend.position = "none")+
  ylab("Accuracy (%)")+
  #coord_cartesian(ylim=c(0.3,0.9))+
  ggtitle(paste0("(A) Both ACC"))

#BothRT(by Stage)
bRT <- ggplot(data = GoCog, aes(x = CogTask, y = Both_RT, group = SubjGroup, 
                                color = SubjGroup, shape=SubjGroup)) +
  scale_colour_grey(start = 0.5, end = 0)+
  facet_grid(.~GoStage) +
  stat_summary(fun.y = mean, geom = "point", size = 2,
               position=position_dodge(width=0.3)) +
  stat_summary(fun.y = mean, geom = "line",
               position=position_dodge(width=0.3)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .2,
               position=position_dodge(width=0.3)) +
  scale_shape_manual(values=c(1,19)) +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        legend.position = "none")+
  ylab("RT (sec)")+
  #coord_cartesian(ylim=c(0.3,0.9))+
  ggtitle(paste0("(B) Both RT"))

dk <- ggplot(data = GoCog, aes(x = CogTask, y = Both_RT, group = SubjGroup, 
                               color = SubjGroup, shape=SubjGroup)) +
  scale_colour_grey(start = 0.5, end = 0)+
  facet_grid(.~GoStage) +
  stat_summary(fun.y = mean, geom = "point", size = 2,
               position=position_dodge(width=0.3)) +
  stat_summary(fun.y = mean, geom = "line",
               position=position_dodge(width=0.3)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .2,
               position=position_dodge(width=0.3)) +
  scale_shape_manual(values=c(1,19)) +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        legend.position = "top")

dk <- g_legend(dk)

pdf("Output/Aov3_Interaction.pdf", width = 7,height = 7)
grid.arrange(arrangeGrob(bACC,
                         bRT,
                         nrow=2),
             dk, nrow=2,heights=c(10, 1))
dev.off()

















############ Dan Kyu: Main and Interaction ############

## MainEffect Plot
GoStagebACC <- ggplot(data = GoCog, aes(x = GoStage, y =  Both_ACC,
                                        shape = SubjGroup,
                                        color = SubjGroup)) +
  scale_colour_grey(start = 0.5, end = 0)+
  stat_summary(fun.y = mean, geom ="point", size = 2, position=position_dodge(width=0.3)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .1,position=position_dodge(width=0.3)) +
  scale_shape_manual(values=c(1,19)) +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),legend.position = "none")+
  ylab("Accuracy (%)")+
  xlab(" ")+
  coord_cartesian(ylim=c(0.35,0.9))+
  ggtitle(paste0("(A) GoStage: Both ACC"))
  
GoStagebRT <- ggplot(data = GoCog,  aes(x = GoStage, y =  Both_RT,
                                        shape = SubjGroup,
                                        color = SubjGroup)) +
  scale_colour_grey(start = 0.5, end = 0)+
  stat_summary(fun.y = mean, geom ="point", size = 2, position=position_dodge(width=0.3)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .1,position=position_dodge(width=0.3)) +
  scale_shape_manual(values=c(1,19)) +
  theme_default()+
  ######
  theme(plot.title = element_text(hjust = 0,size = 10),
        legend.position = c(0.82,0.82))+ 
  #######
  ylab("RT (sec)")+
  xlab(" ")+
  coord_cartesian(ylim=c(10,24))+
  ggtitle(paste0("(B) GoStage: Both RT"))

CogTaskbACC <- ggplot(data = GoCog,  aes(x = CogTask, y =  Both_ACC,
                                         shape = SubjGroup,
                                         color = SubjGroup)) +
  scale_colour_grey(start = 0.5, end = 0)+
  stat_summary(fun.y = mean, geom ="point", size = 2, position=position_dodge(width=0.3)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .1,position=position_dodge(width=0.3)) +
  scale_shape_manual(values=c(1,19)) +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),legend.position = "none")+
  ylab("Accuracy (%)")+
  xlab(" ")+
  coord_cartesian(ylim=c(0.35,0.9))+
  ggtitle(paste0("(C) CogTask: Both ACC"))

CogTaskbRT <- ggplot(data = GoCog,  aes(x = CogTask, y =  Both_RT,
                                        shape = SubjGroup,
                                        color = SubjGroup)) +
  scale_colour_grey(start = 0.5, end = 0)+
  stat_summary(fun.y = mean, geom ="point", size = 2, position=position_dodge(width=0.3)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .1,position=position_dodge(width=0.3)) +
  scale_shape_manual(values=c(1,19)) +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),legend.position = "none")+
  ylab("RT (sec)")+
  xlab(" ")+
  coord_cartesian(ylim=c(10,24))+
  ggtitle(paste0("(D) CogTask: Both RT"))

pdf("Output/Aov3_MainEffect.pdf", width = 7,height = 7)
grid.arrange(GoStagebACC,GoStagebRT,CogTaskbACC,CogTaskbRT,ncol =2)
dev.off()
## Interaction Plot

#BothACC(by Stage)
bACC <- ggplot(data = GoCog, aes(x = CogTask, y = Both_ACC, group = SubjGroup, 
                         color = SubjGroup, shape=SubjGroup)) +
  scale_colour_grey(start = 0.5, end = 0)+
  facet_grid(.~GoStage) +
  stat_summary(fun.y = mean, geom = "point", size = 2,
               position=position_dodge(width=0.3)) +
  stat_summary(fun.y = mean, geom = "line",
               position=position_dodge(width=0.3)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .2,
               position=position_dodge(width=0.3)) +
  scale_shape_manual(values=c(1,19)) +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        legend.position = "none")+
  ylab("Accuracy (%)")+
  #coord_cartesian(ylim=c(0.3,0.9))+
  ggtitle(paste0("(A) Both ACC"))

#BothRT(by Stage)
bRT <- ggplot(data = GoCog, aes(x = CogTask, y = Both_RT, group = SubjGroup, 
                                 color = SubjGroup, shape=SubjGroup)) +
  scale_colour_grey(start = 0.5, end = 0)+
  facet_grid(.~GoStage) +
  stat_summary(fun.y = mean, geom = "point", size = 2,
               position=position_dodge(width=0.3)) +
  stat_summary(fun.y = mean, geom = "line",
               position=position_dodge(width=0.3)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .2,
               position=position_dodge(width=0.3)) +
  scale_shape_manual(values=c(1,19)) +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        legend.position = "none")+
  ylab("RT (sec)")+
  #coord_cartesian(ylim=c(0.3,0.9))+
  ggtitle(paste0("(B) Both RT"))

dk <- ggplot(data = GoCog, aes(x = CogTask, y = Both_RT, group = SubjGroup, 
                         color = SubjGroup, shape=SubjGroup)) +
  scale_colour_grey(start = 0.5, end = 0)+
  facet_grid(.~GoStage) +
  stat_summary(fun.y = mean, geom = "point", size = 2,
               position=position_dodge(width=0.3)) +
  stat_summary(fun.y = mean, geom = "line",
               position=position_dodge(width=0.3)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .2,
               position=position_dodge(width=0.3)) +
  scale_shape_manual(values=c(1,19)) +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        legend.position = "top")

dk <- g_legend(dk)

pdf("Output/Aov3_Interaction.pdf", width = 7,height = 7)
grid.arrange(arrangeGrob(bACC,
                         bRT,
                         nrow=2),
             dk, nrow=2,heights=c(10, 1))
dev.off()














# 
# 
# 
# #########################################################################################
# #Interaction_BothACC
# png( filename = "In_BothACC.png", width = 1200, height = 600)
# ggplot(data = GoCog, aes(x = CogTask, y = Both_ACC, group = GoStage, 
#                          color = GoStage, shape=GoStage)) +
#   facet_grid(.~SubjGroup) +
#   stat_summary(fun.y = mean, geom = "point", size = 4) +
#   stat_summary(fun.y = mean, geom = "line") +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2) +
#   labs(x = "CogTask", y = "ACC")
# dev.off()
# 
# #Interaction_BothRT
# png( filename = "In_BothRT.png", width = 1200, height = 600)
# ggplot(data = GoCog, aes(x = CogTask, y = Both_RT, group = GoStage, 
#                          color = GoStage, shape=GoStage)) +
#   facet_grid(.~SubjGroup) +
#   stat_summary(fun.y = mean, geom = "point", size = 4) +
#   stat_summary(fun.y = mean, geom = "line") +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2) +
#   labs(x = "CogTask", y = "RT")
# dev.off()
# 
# 
# ####################################################################################
# 
#   
#   #GoStage_GoACC
#   png( filename = "P_GoACC.png", width = 600, height = 600)
# ggplot(data = GoCog, aes(x = GoStage, y = Go_ACC, color = SubjGroup)) +
#   stat_summary(fun.y = mean, geom = "point", size = 4) +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2) +
#   labs(x = "GoStage", y = "ACC", title="Go_ACC")
# 
# dev.off()
# 
# #GoStage_GoRT
# png( filename = "P_GoRT.png", width = 600, height = 600)
# ggplot(data = GoCog, aes(x = GoStage, y = Go_RT, color = SubjGroup)) +
#   stat_summary(fun.y = mean, geom = "point", size = 4) +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2) +
#   labs(x = "GoStage", y = "RT", title="Go_ACC")
# dev.off()
# 
# #Interaction_GoACC
# png( filename = "In_GoACC.png", width = 1200, height = 600)
# ggplot(data = GoCog, aes(x = CogTask, y = Go_ACC, group = GoStage, 
#                          color = GoStage, shape=GoStage)) +
#   facet_grid(.~SubjGroup) +
#   stat_summary(fun.y = mean, geom = "point", size = 4) +
#   stat_summary(fun.y = mean, geom = "line") +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2) +
#   labs(x = "CogTask", y = "ACC", title="Go_ACC")
# dev.off()
# 
# #Interaction_GoRT
# png( filename = "In_GoRT.png", width = 1200, height = 600)
# ggplot(data = GoCog, aes(x = CogTask, y = Go_RT, group = GoStage, 
#                          color = GoStage, shape=GoStage)) +
#   facet_grid(.~SubjGroup) +
#   stat_summary(fun.y = mean, geom = "point", size = 4) +
#   stat_summary(fun.y = mean, geom = "line") +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2) +
#   labs(x = "CogTask", y = "RT", title="Go_ACC")
# dev.off()
# 
# #Interaction_GoACC(by Stage)
# png( filename = "Stage_GoACC.png", width = 1200, height = 600)
# ggplot(data = GoCog, aes(x = CogTask, y = Go_ACC, group = SubjGroup, 
#                          color = SubjGroup, shape=SubjGroup)) +
#   facet_grid(.~GoStage) +
#   stat_summary(fun.y = mean, geom = "point", size = 4) +
#   stat_summary(fun.y = mean, geom = "line") +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2) +
#   labs(x = "CogTask", y = "ACC", title="Go_ACC")
# dev.off()
# 
# #Interaction_GoACC(by Stage)
# png( filename = "Stage_GoRT.png", width = 1200, height = 600)
# ggplot(data = GoCog, aes(x = CogTask, y = Go_RT, group = SubjGroup, 
#                          color = SubjGroup, shape=SubjGroup)) +
#   facet_grid(.~GoStage) +
#   stat_summary(fun.y = mean, geom = "point", size = 4) +
#   stat_summary(fun.y = mean, geom = "line") +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2) +
#   labs(x = "CogTask", y = "RT", title="Go_ACC")
# dev.off()
# 
# #Interaction_BothACC_overall
# png( filename = "In_BothACC_overall.png", width = 600, height = 600)
# ggplot(data = GoCog, aes(x = CogTask, y = Both_ACC, group = GoStage, 
#                          color = GoStage, shape=GoStage)) +
#   stat_summary(fun.y = mean, geom = "point", size = 4) +
#   stat_summary(fun.y = mean, geom = "line") +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2) +
#   labs(x = "CogTask", y = "ACC")
# dev.off()
# 
# #################################################################################################
# GoCog<-read.csv("GoCog.csv", h=T)
# GoCog<-GoCog[-which(GoCog$CogTask=="None"),]
# 
# 
# GoCog$GoStage <- factor(GoCog$GoStage, levels=c("Open", "Mid", "End"))
# GoCog$CogTask<- factor(GoCog$CogTask, levels=c( "Calc", "Spat", "Reas"))
# GoCog$SubjGroup<- factor(GoCog$SubjGroup, levels=c("?~?l?Ŧ?(Kyu)", "?~?l?q??(Dan)"))
# GoCog$SubjRank <- factor(GoCog$SubjRank, levels = c("?~?l????", "?~?l?A??", "?~?l?Ҳ?", "?~?l?@?q", 
#                                                     "?~?l?G?q", "?~?l?T?q", "?~?l?|?q", "?~?l???q", "?~?l???q"))
# 
# #Interaction_BothACC(by Stage)
# png( filename = "Stage_CogACC.png", width = 1200, height = 600)
# ggplot(data = GoCog, aes(x = GoStage, y = Cog_ACC, group = SubjGroup, 
#                          color = SubjGroup, shape=SubjGroup)) +
#   facet_grid(.~CogTask) +
#   stat_summary(fun.y = mean, geom = "point", size = 4) +
#   stat_summary(fun.y = mean, geom = "line") +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2) +
#   labs(x = "GoStage", y = "CogACC")
# dev.off()
# 
# 
# #Interaction_BothACC(by Stage)
# png( filename = "Stage_CogRT.png", width = 1200, height = 600)
# ggplot(data = GoCog, aes(x = GoStage, y = Cog_RT, group = SubjGroup, 
#                          color = SubjGroup, shape=SubjGroup)) +
#   facet_grid(.~CogTask) +
#   stat_summary(fun.y = mean, geom = "point", size = 4) +
#   stat_summary(fun.y = mean, geom = "line") +
#   stat_summary(fun.data = mean_se, geom = "errorbar",
#                linetype = "solid", width = .2) +
#   labs(x = "GoStage", y = "CogRT")
# dev.off()
