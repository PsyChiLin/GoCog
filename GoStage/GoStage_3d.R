
library(psych)
library(plotly)
dta_3s_rst <- readRDS("../GoCogdata/dta_3s_rst.Rdata")
# PCA
rst_pca <- prcomp(x = dta_3s_rst[5:12],center = TRUE,scale. = TRUE)
summary(rst_pca)
# 3d plot
dta_3s_rst[,14:16] <- predict(rst_pca, newdata=dta_3s_rst)
head(dta_3s_rst)
colnames(dta_3s_rst)[14:16] <- c("PC1","PC2","PC3")
dta_3s_rst$TF <- ifelse(dta_3s_rst$GoStage == dta_3s_rst$pred_GoStage,
                        as.character(dta_3s_rst$GoStage),"Error")
dta_3s_rst$TF <- factor(dta_3s_rst$TF, levels = c("Open","Mid","End","Error"))

# plotly (dynamic)
p <- plot_ly(dta_3s_rst, x = ~PC1, y = ~PC2, z = ~PC3, color = ~TF,
             colors = c('red', 'green',"blue","grey")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = ' ',showticklabels  = F),
                      yaxis = list(title = ' ',showticklabels  = F),
                      zaxis = list(title = ' ',showticklabels  = F)))
p
