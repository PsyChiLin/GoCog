library(afex)
library(dplyr)
library(reshape2)
library(readxl)

rm(list = ls())
## Read Data
GoCog<-read.csv("../GoCogdata/GoCog.csv", h=T)
GoCog_20180401 <- list()
sub <- GoCog$Subj[!duplicated(GoCog$Subj)]
for (i in 1:length(sub)){
  name <- ifelse(sub[i] %in% 1:9, paste0("0",sub[i]),sub[i])
  dta <- as.data.frame(read_excel(paste0("../GoCogdata/Response/Response",name,".xlsx")))
  dta$GoStage <- factor(dta$GoStage, levels = c("Open","Mid","End"))
  dta$CogTask <- factor(dta$CogTask, levels = c("None","Spat","Reas","Calc"))
  # both ACC
  bACC <- aggregate(Both_Acc~ GoStage+CogTask,data = dta, FUN = mean)
  bACC <- bACC[order(bACC$GoStage),]
  # both ACC == 1 will be included.
  dta_bACC_1 <- filter(dta,Both_Acc == 1)
  # Both RT
  bRT <- aggregate(Both_RT~ GoStage+CogTask,data = dta_bACC_1, FUN = mean)
  bRT <- bRT[order(bRT$GoStage),]
  # Go RT
  GRT <- aggregate(Go_RT~ GoStage+CogTask,data = dta_bACC_1, FUN = mean)
  GRT <- GRT[order(GRT $GoStage),]
  # Cog RT
  CRT <- aggregate(Cog_RT~ GoStage+CogTask,data = dta_bACC_1, FUN = mean)
  CRT <- CRT[order(CRT$GoStage),]
  
  nData <- full_join(full_join(full_join(bACC,bRT),GRT),CRT)
  nData$Subj = sub[i]
  GoCog_20180401[[i]] <- nData[c(7,1:6)]
}
GoCog_20180401_new <- do.call(rbind,GoCog_20180401)
write.csv(GoCog_20180401_new,file = "../GoCogdata/GoCog_20180404.csv",row.names = F)
