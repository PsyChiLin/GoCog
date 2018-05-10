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
                        labels = c("Opening","Midgame","Endgame"))
GoCog$CogTask<- factor(GoCog$CogTask, levels=c("None", "Spat", "Reas", "Calc"),
                       labels = c("None","Spatial","Reasoning","Calculation"))
GoCog$SubjGroup <- factor(GoCog$SubjGroup, levels=c("Dan", "Kyu"))

## Interaction Plot: Type1
bACC <- ggplot(data = GoCog, aes(x = CogTask, y = Both_ACC, group = GoStage)) +
  scale_colour_grey(start = 0.5, end = 0)+
  facet_grid(.~GoStage) +
  stat_summary(fun.y = mean, geom = "point", size = 2, shape = 19) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               linetype = "solid", width = .2) +
  theme_default()+
  theme(plot.title = element_text(hjust = 0,size = 10),
        #axis.title.y  = element_text(angle = 60, vjust = 0.5, size = 8),
        axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 8),
        text = element_text(family = 'Times'))+
  ylab("Accuracy(%)")+
  xlab(" ")+
  coord_cartesian(ylim=c(35,100))#+
  #ggtitle(paste0("The difference between four interference tasks under each Go stage"))

ann_text <- data.frame(CogTask = c(2,3,3),Both_ACC = c(60,60,60),label = c("*","*","*"),
                       GoStage = factor(c("Opening","Midgame","Endgame"),levels = c("Opening","Midgame","Endgame")))
bACC <- bACC+geom_text(aes(x = CogTask, y = Both_ACC, label =  label), data = ann_text)


png(file = "../GoCog_Manuscript/FigureTable/Abstract_Figure2.png",height=4, width=8, units="in", res = 300)
bACC
dev.off()
