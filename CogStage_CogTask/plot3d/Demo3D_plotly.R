library(psych)
library(plotly)
dta_3s_rst <- readRDS("../GoCogdata/dta_3s_rst.Rdata")
# aver <- aggregate(cbind(Reas_Both_ACC,Spat_Both_ACC,
#                         Calc_Both_ACC,None_Both_ACC,
#                         Reas_Both_RT,Spat_Both_RT,
#                         Calc_Both_RT,None_Both_RT)~pred_GoStage,
#           data = dta_3s_rst, FUN = mean)
# PCA
rst_pca <- prcomp(x = dta_3s_rst[5:12],center = TRUE,scale. = TRUE)
#summary(rst_pca)
# 3d plot
dta_3s_rst[,14:16] <- predict(rst_pca, newdata=dta_3s_rst)


#head(dta_3s_rst)
colnames(dta_3s_rst)[14:16] <- c("PC1","PC2","PC3")
dta_3s_rst$TF <- ifelse(dta_3s_rst$GoStage == dta_3s_rst$pred_GoStage,
                        as.character(dta_3s_rst$GoStage),"Error")

# aver data predict
# dta <- as.data.frame(predict(rst_pca, newdata=aver))
# dta$Subj <- NA
# dta$Age <- NA
# dta$SubjGroup <- NA
# dta$GoStage <- NA
# dta$pred_GoStage <- aver$pred_GoStage
# dta$TF <- c("End(mean)", "Mid(mean)", "Open(mean)")
# dta_3s_rst <- rbind(dta_3s_rst[,c(14:17)],dta[,c(1:3,14)])

# dta_3s_rst$TF <- factor(dta_3s_rst$TF, 
#                         levels = c("Open","Open(mean)",
#                                    "Mid","Mid(mean)",
#                                    "End","End(mean)",
#                                    "Error"))

dta_3s_rst$TF <- factor(dta_3s_rst$TF, 
                        levels = c("Open",
                                   "Mid",
                                   "End",
                                   "Error"))

# plotly (dynamic)
p <- plot_ly(dta_3s_rst, x = ~PC1, y = ~PC2, z = ~PC3, symbol = ~TF,color = ~TF,#I('black'),
             symbols = c("circle","circle-open","square","cross"),
             marker = list(size = c(5))
             ,
             colors = c("black","black","grey","black")
) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = ' ',showticklabels  = F),
                      yaxis = list(title = ' ',showticklabels  = F),
                      zaxis = list(title = ' ',showticklabels  = F)))
p

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

# # aver data predict
# dta <- as.data.frame(predict(rst_pca, newdata=aver2))
# dta$Subj <- NA
# dta$Age <- NA
# dta$SubjGroup <- NA
# dta$CogTask <- NA
# dta$pred_CogTask <- aver$pred_CogTask
# dta$TF <- c("Calc(mean)", "None(mean)", "Reas(mean)","Spat(mean)")
# dta_4c_rst <- rbind(dta_4c_rst[,c(12:15)],dta[,c(1:3,11)])
# dta_4c_rst$TF <- factor(dta_4c_rst$TF, 
#                         levels = c("None","None(mean)",
#                                    "Spat", "Spat(mean)",
#                                    "Reas","Reas(mean)",
#                                    "Calc","Calc(mean)",
#                                    "Error"))
dta_4c_rst$TF <- factor(dta_4c_rst$TF,levels = c("None","Spat","Reas","Calc","Error"))
# plotly (dynamic)
p <- plot_ly(dta_4c_rst, x = ~PC1, y = ~PC2, z = ~PC3, symbol = ~TF,color = ~TF,# I('black'),
             symbols = c("circle-open","circle","square-open","diamond","cross"),
             marker = list(size = c(5)),
             colors = c("black","black","gray45","gray85","black")
             ) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = ' ',showticklabels  = F),
                      yaxis = list(title = ' ',showticklabels  = F),
                      zaxis = list(title = ' ',showticklabels  = F)))
p

