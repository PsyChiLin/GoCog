rm(list = ls())
library(psych)
library(plotly)
library("scatterplot3d")
library(dplyr)
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
ang = -45

dta_3s_rst <- readRDS("../GoCogdata/dta_3s_rst.Rdata")
# PCA
rst_pca <- prcomp(x = dta_3s_rst[5:12],center = TRUE,scale. = TRUE)
#summary(rst_pca)
# 3d plot
dta_3s_rst[,14:16] <- predict(rst_pca, newdata=dta_3s_rst)
#head(dta_3s_rst)
colnames(dta_3s_rst)[14:16] <- c("PC1","PC2","PC3")
dta_3s_rst$TF <- ifelse(dta_3s_rst$GoStage == dta_3s_rst$pred_GoStage,
                        as.character(dta_3s_rst$GoStage),"Error")
dta_3s_rst$TF <- factor(dta_3s_rst$TF, levels = c("Open","Mid","End","Error"))

shapes = c(19,1, 2, 4) 
shapes <- shapes[as.numeric(dta_3s_rst$TF)]
max(dta_3s_rst[,14])
#pdf(file = "../GoCog_Manuscript/FigureTable/圖5_GoStage_3D.pdf", width = 7, height = 7)
gostage <- scatterplot3d(dta_3s_rst[,14:16], pch = "", grid=TRUE, box=FALSE,label.tick.marks=F,
                         xlim=c(-3.29,3.93), ylim=c(-3.35,3.99), zlim=c(-2.43,3.25),
                         #angle = ang,
                         xlab = " ", ylab = " ",zlab = " ")
addgrids3d(dta_3s_rst[,14:16], grid = c("xy", "xz", "yz"),
           #angle = ang,
           xlim=c(-3.29,3.93), ylim=c(-3.35,3.99), zlim=c(-2.43,3.25))
gostage$points3d(dta_3s_rst[,14:16], pch = shapes)
#dev.off()
# gostage <- scatterplot3d(dta_3s_rst[,14:16], pch = "", grid=TRUE, box=FALSE,label.tick.marks=F,
#                           xlim=c(-4,4), ylim=c(-4,4), zlim=c(-4,4),
#                           xlab = " ", ylab = " ",zlab = " ")
# addgrids3d(dta_3s_rst[,14:16], grid = c("xy", "xz", "yz"),xlim = c(-4,4), ylim = c(-4,4), zlim = c(-4,4))
# gostage$points3d(dta_3s_rst[,14:16], pch = shapes)
# legend("right", legend = levels(dta_3s_rst$TF), pch = c(19,1, 2, 4))


dta_4c_rst <- readRDS("../GoCogdata/dta_4c_rst.Rdata")
aver2 <- aggregate(cbind(End_Both_ACC,Mid_Both_ACC,Open_Both_ACC,
                         End_Both_RT,Mid_Both_RT,Open_Both_RT)~pred_CogTask,
                   data = dta_4c_rst, FUN = mean)
# PCA
rst_pca <- prcomp(x = dta_4c_rst[5:10],center = TRUE,scale. = TRUE)
#summary(rst_pca)
# 3d plot
dta_4c_rst[,12:14] <- predict(rst_pca, newdata=dta_4c_rst)
#head(dta_4c_rst)
colnames(dta_4c_rst)[12:14] <- c("PC1","PC2","PC3")
dta_4c_rst$TF <- ifelse(dta_4c_rst$CogTask == dta_4c_rst$pred_CogTask,
                        as.character(dta_4c_rst$CogTask),"Error")

dta_4c_rst$TF <- factor(dta_4c_rst$TF,levels = c("None","Spat","Reas","Calc","Error"))

shapes2 = c(16, 2, 17, 0, 4) 
shapes2 <- shapes2[as.numeric(dta_4c_rst$TF)]
pdf(file = "../GoCog_Manuscript/FigureTable/圖5_CogTask_3D.pdf", width = 7, height = 7)
cogtask <- scatterplot3d(dta_4c_rst[,12:14], pch = "", grid=TRUE, box=FALSE,label.tick.marks=F,
                         xlim = c(-4,4), ylim = c(-4,4), zlim = c(-4,4),
                         angle = ang,
                         xlab = " ", ylab = " ",zlab = " ")
addgrids3d(dta_4c_rst[,12:14],
           xlim = c(-4,4), ylim = c(-4,4), zlim = c(-4,4),
           angle = ang,
           grid = c("xy", "xz", "yz"))
cogtask$points3d(dta_4c_rst[,12:14], pch = shapes2)
dev.off()
cogtask <- scatterplot3d(dta_4c_rst[,12:14], pch = "", grid=TRUE, box=FALSE,label.tick.marks=F,
                         xlim = c(-4,4), ylim = c(-4,4), zlim = c(-4,4),
                         xlab = " ", ylab = " ",zlab = " ")
addgrids3d(dta_4c_rst[,12:14],
           xlim = c(-4,4), ylim = c(-4,4), zlim = c(-4,4),
           grid = c("xy", "xz", "yz"))
cogtask$points3d(dta_4c_rst[,12:14], pch = shapes2)
legend("bottom", legend = levels(dta_4c_rst$TF), pch = c(16, 2, 17, 0, 4))
