dta_3s_rst <- readRDS("../GoCogdata/dta_3s_rst.Rdata")
scatter <- dta_3s_rst
head(scatter)

scatter$ACC <- as.data.frame(rowMeans(scatter[,5:8]))
scatter$RT <- as.data.frame(rowMeans(scatter[,9:12]))

colnames(scatter)[14:15] <- c("ACC","RT")

ggplot(scatter, aes(x = ACC, y = RT, color = pred_GoStage))+
  geom_point()
