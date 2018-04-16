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
  dta$Go_RTx <- ifelse(dta$CogTask == "None",dta$Go_RT,
                       ifelse(dta$Go_RT < dta$Both_RT,dta$Go_RT,dta$Both_RT - dta$Cog_RT))
  dta$Cog_RTx <- ifelse(dta$CogTask == "None",dta$Cog_RT,
                       ifelse(dta$Cog_RT < dta$Both_RT,dta$Cog_RT,dta$Both_RT - dta$Go_RT))
  
  # both ACC
  bACC <- aggregate(Both_Acc~ GoStage+CogTask,data = dta, FUN = mean)
  bACC <- bACC[order(bACC$GoStage),]
  # both ACC == 1 will be included.
  dta_bACC_1 <- filter(dta,Both_Acc == 1)
  # Both RT
  bRT <- aggregate(Both_RT~ GoStage+CogTask,data = dta_bACC_1, FUN = mean)
  bRT <- bRT[order(bRT$GoStage),]
  # Go RTx
  GRT <- aggregate(Go_RTx~ GoStage+CogTask,data = dta_bACC_1, FUN = mean)
  GRT <- GRT[order(GRT$GoStage),]
  # Cog RTx
  CRT <- aggregate(Cog_RTx~ GoStage+CogTask,data = dta_bACC_1, FUN = mean)
  CRT <- CRT[order(CRT$GoStage),]
  
  nData <- full_join(full_join(full_join(bACC,bRT),GRT),CRT)
  nData$Subj = sub[i]
  GoCog_20180401[[i]] <- nData[c(7,1:6)]
}
GoCog_20180401_new <- do.call(rbind,GoCog_20180401)
GoCog_20180401_new <- full_join(GoCog[,1:5],GoCog_20180401_new)

head(GoCog_20180401_new)
is.na(GoCog_20180401_new)

write.csv(GoCog_20180401_new,file = "../GoCogdata/GoCog_20180404.csv",row.names = F)

# 2 subjects have zero values 
# 23	10	Dan	Open	Reas	0	NA	NA	NA
# 14	11	Kyu	Mid	Reas	0	NA	NA	NA

#round(GoCog_20180401_new$Both_Acc,2) == round(GoCog$Both_ACC,2)
#round(GoCog_20180401_new$Both_RT,2)[111] == round(GoCog$Both_RT,2)[111]
