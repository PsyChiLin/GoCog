library(psych)
library(plotly)
dta_4c_rst <- readRDS("../GoCogdata/dta_4c_rst.Rdata")
# PCA
rst_pca <- prcomp(x = dta_4c_rst[5:10],center = TRUE,scale. = TRUE)
summary(rst_pca)
# 3d plot
dta_4c_rst[,12:14] <- predict(rst_pca, newdata=dta_4c_rst)
head(dta_4c_rst)
colnames(dta_4c_rst)[12:14] <- c("PC1","PC2","PC3")
dta_4c_rst$TF <- ifelse(dta_4c_rst$CogTask == dta_4c_rst$pred_CogTask,
                        as.character(dta_4c_rst$CogTask),"Error")
dta_4c_rst$TF <- factor(dta_4c_rst$TF, levels = c("None","Spat","Reas","Calc","Error"))

# plotly (dynamic)
p <- plot_ly(dta_4c_rst, x = ~PC1, y = ~PC2, z = ~PC3, color = ~TF,
             colors = c("yellow",'red', 'green',"blue","grey")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = ' ',showticklabels  = F),
                      yaxis = list(title = ' ',showticklabels  = F),
                      zaxis = list(title = ' ',showticklabels  = F)))
p
