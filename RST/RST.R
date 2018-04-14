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
library(lm.beta)
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

# Read Data
RST<-read.csv("../GoCogdata/NoneRST_All.csv", h=T)[,c(1:3,25:27)]
GoCog<-read.csv("../GoCogdata/GoCog_20180404.csv", h=T)[,1:7]
RSTdata <- full_join(GoCog,RST)
RSTdata$Both_Acc <- RSTdata$Both_Acc*100
RSTdata$Both_RT <- RSTdata$Both_RT/1000
head(RSTdata)

Overall <- aggregate(data = RSTdata, cbind(Both_Acc,Both_RT,Verbal.z,Nonverbal.z,Total.z)~Subj,FUN = mean)
summary(lm(Both_Acc~Verbal.z,data = Overall))
summary(lm(Both_Acc~Nonverbal.z,data = Overall))
summary(lm(Both_Acc~Total.z,data = Overall))
summary(lm(Both_RT~Verbal.z,data = Overall))
summary(lm(Both_RT~Nonverbal.z,data = Overall))
summary(lm(Both_RT~Total.z,data = Overall))

GoStage <- aggregate(data = RSTdata, cbind(Both_Acc,Both_RT,Verbal.z,Nonverbal.z,Total.z)~Subj+GoStage,FUN = mean)
Opne <- filter(GoStage, GoStage == "Open")
Mid <- filter(GoStage, GoStage == "Mid")
End <- filter(GoStage, GoStage == "End")

summary(lm(Both_Acc~Verbal.z,data =Opne))
summary(lm(Both_Acc~Nonverbal.z,data =Opne)) # Sig p = 0.0476
summary(lm(Both_Acc~Total.z,data =Opne))
summary(lm(Both_RT~Verbal.z,data =Opne))
summary(lm(Both_RT~Nonverbal.z,data =Opne))
summary(lm(Both_RT~Total.z,data =Opne))

summary(lm(Both_Acc~Verbal.z,data =Mid))
summary(lm(Both_Acc~Nonverbal.z,data =Mid)) 
summary(lm(Both_Acc~Total.z,data =Mid))
summary(lm(Both_RT~Verbal.z,data =Mid))# Sig p = 0.0479
summary(lm(Both_RT~Nonverbal.z,data =Mid))
summary(lm(Both_RT~Total.z,data =Mid))

summary(lm(Both_Acc~Verbal.z,data =End))
summary(lm(Both_Acc~Nonverbal.z,data =End)) 
summary(lm(Both_Acc~Total.z,data =End))
summary(lm(Both_RT~Verbal.z,data =End))
summary(lm(Both_RT~Nonverbal.z,data =End))
summary(lm(Both_RT~Total.z,data =End))

CogTask <- aggregate(data = RSTdata, cbind(Both_Acc,Both_RT,Verbal.z,Nonverbal.z,Total.z)~Subj+CogTask,FUN = mean)
None <- filter(CogTask, CogTask == "None")
Spat <- filter(CogTask, CogTask == "Spat")
Reas <- filter(CogTask, CogTask == "Reas")
Calc <- filter(CogTask, CogTask == "Calc")

# summary(lm(Both_Acc~Verbal.z,data =None))
# summary(lm(Both_Acc~Nonverbal.z,data =None))
# summary(lm(Both_Acc~Total.z,data =None))
# summary(lm(Both_RT~Verbal.z,data =None))
# summary(lm(Both_RT~Nonverbal.z,data =None))
# summary(lm(Both_RT~Total.z,data =None))
# 
# summary(lm(Both_Acc~Verbal.z,data =Spat))
# summary(lm(Both_Acc~Nonverbal.z,data =Spat))
# summary(lm(Both_Acc~Total.z,data =Spat))
# summary(lm(Both_RT~Verbal.z,data =Spat))
# summary(lm(Both_RT~Nonverbal.z,data =Spat))
# summary(lm(Both_RT~Total.z,data =Spat))
# 
# summary(lm(Both_Acc~Verbal.z,data =Reas)) #  0.0106 * 
# summary(lm(Both_Acc~Nonverbal.z,data =Reas)) # 3e-04 ***
summary(lm.beta(lm(Total.z~Both_Acc,data =Reas))) # 0.000438 ***
# summary(lm(Both_RT~Verbal.z,data =Reas))
# summary(lm(Both_RT~Nonverbal.z,data =Reas))
# summary(lm(Both_RT~Total.z,data =Reas))
# 
# summary(lm(Both_Acc~Verbal.z,data = Calc))
# summary(lm(Both_Acc~Nonverbal.z,data = Calc))
# summary(lm(Both_Acc~Total.z,data = Calc))
# summary(lm(Both_RT~Verbal.z,data = Calc))# 0.00194 **
# summary(lm(Both_RT~Nonverbal.z,data = Calc))  #0.0539 . 
# summary(lm(Both_RT~Total.z,data = Calc))#0.00931 ** 


NoneOpen <- filter(RSTdata, CogTask == "None" & GoStage == "Open")
SpatOpen <- filter(RSTdata, CogTask == "Spat" & GoStage == "Open")
ReasOpen <- filter(RSTdata, CogTask == "Reas" & GoStage == "Open")
CalcOpen <- filter(RSTdata, CogTask == "Calc" & GoStage == "Open")

NoneMid <- filter(RSTdata, CogTask == "None" & GoStage == "Mid")
SpatMid <- filter(RSTdata, CogTask == "Spat" & GoStage == "Mid")
ReasMid <- filter(RSTdata, CogTask == "Reas" & GoStage == "Mid")
CalcMid <- filter(RSTdata, CogTask == "Calc" & GoStage == "Mid")

NoneEnd <- filter(RSTdata, CogTask == "None" & GoStage == "End")
SpatEnd <- filter(RSTdata, CogTask == "Spat" & GoStage == "End")
ReasEnd <- filter(RSTdata, CogTask == "Reas" & GoStage == "End")
CalcEnd <- filter(RSTdata, CogTask == "Calc" & GoStage == "End")

#summary(lm(Both_Acc~Verbal.z,data = NoneOpen))
#summary(lm(Both_Acc~Nonverbal.z,data = NoneOpen))
summary(lm(Both_Acc~Total.z,data = NoneOpen))
#summary(lm(Both_RT~Verbal.z,data = NoneOpen))
#summary(lm(Both_RT~Nonverbal.z,data = NoneOpen))
summary(lm(Both_RT~Total.z,data = NoneOpen))

#summary(lm(Both_Acc~Verbal.z,data = SpatOpen))
#summary(lm(Both_Acc~Nonverbal.z,data = SpatOpen))
summary(lm(Both_Acc~Total.z,data = SpatOpen))
#summary(lm(Both_RT~Verbal.z,data = SpatOpen))
#summary(lm(Both_RT~Nonverbal.z,data = SpatOpen))
summary(lm(Both_RT~Total.z,data = SpatOpen))

#summary(lm(Both_Acc~Verbal.z,data = ReasOpen)) #  0.0575 . 
#summary(lm(Both_Acc~Nonverbal.z,data = ReasOpen)) # 0.000214 ***
#summary(lm(Both_Acc~Total.z,data = ReasOpen)) # 0.00131 ** 
##
summary(lm.beta(lm(Total.z~Both_Acc,data = ReasOpen)))
##
#summary(lm(Both_RT~Verbal.z,data = ReasOpen))
#summary(lm(Both_RT~Nonverbal.z,data = ReasOpen))
summary(lm(Both_RT~Total.z,data = ReasOpen))

#summary(lm(Both_Acc~Verbal.z,data = CalcOpen ))
#summary(lm(Both_Acc~Nonverbal.z,data = CalcOpen ))
summary(lm(Both_Acc~Total.z,data = CalcOpen))
#summary(lm(Both_RT~Verbal.z,data = CalcOpen ))
#summary(lm(Both_RT~Nonverbal.z,data = CalcOpen ))
summary(lm(Both_RT~Total.z,data = CalcOpen ))

# Mid
#summary(lm(Both_Acc~Verbal.z,data = NoneMid))
#summary(lm(Both_Acc~Nonverbal.z,data = NoneMid))
summary(lm(Both_Acc~Total.z,data = NoneMid))
#summary(lm(Both_RT~Verbal.z,data = NoneMid))
#summary(lm(Both_RT~Nonverbal.z,data = NoneMid))
summary(lm(Both_RT~Total.z,data = NoneMid))

#summary(lm(Both_Acc~Verbal.z,data = SpatMid))
#summary(lm(Both_Acc~Nonverbal.z,data = SpatMid))
summary(lm(Both_Acc~Total.z,data = SpatMid))
#summary(lm(Both_RT~Verbal.z,data = SpatMid)) #  0.0498 *  
#summary(lm(Both_RT~Nonverbal.z,data = SpatMid))
summary(lm(Total.z~Both_RT,data = SpatMid)) # 0.0379 * 

#summary(lm(Both_Acc~Verbal.z,data = ReasMid)) # 0.243  
#summary(lm(Both_Acc~Nonverbal.z,data = ReasMid)) #0.0176 * 
#
summary(lm.beta(lm(Total.z~Both_Acc,data = ReasMid))) #  0.041 *   
#
#summary(lm(Both_RT~Verbal.z,data = ReasMid))
#summary(lm(Both_RT~Nonverbal.z,data = ReasMid))
summary(lm(Both_RT~Total.z,data = ReasMid))

#summary(lm(Both_Acc~Verbal.z,data = CalcMid ))
#summary(lm(Both_Acc~Nonverbal.z,data = CalcMid ))
summary(lm(Both_Acc~Total.z,data = CalcMid ))
#summary(lm(Both_RT~Verbal.z,data = CalcMid )) # 0.0275 *  
#summary(lm(Both_RT~Nonverbal.z,data = CalcMid ))
summary(lm(Both_RT~Total.z,data = CalcMid ))

# End
#summary(lm(Both_Acc~Verbal.z,data = NoneEnd))
#summary(lm(Both_Acc~Nonverbal.z,data = NoneEnd))
summary(lm(Both_Acc~Total.z,data = NoneEnd))
#summary(lm(Both_RT~Verbal.z,data = NoneEnd))
#summary(lm(Both_RT~Nonverbal.z,data = NoneEnd))
summary(lm(Both_RT~Total.z,data = NoneEnd))

#summary(lm(Both_Acc~Verbal.z,data = SpatEnd))
#summary(lm(Both_Acc~Nonverbal.z,data = SpatEnd))
summary(lm(Both_Acc~Total.z,data = SpatEnd))
#summary(lm(Both_RT~Verbal.z,data = SpatEnd))  
#summary(lm(Both_RT~Nonverbal.z,data = SpatEnd))
summary(lm(Both_RT~Total.z,data = SpatEnd)) 

#summary(lm(Both_Acc~Verbal.z,data = ReasEnd)) # 0.00213 **  
#summary(lm(Both_Acc~Nonverbal.z,data = ReasEnd)) # 0.0135 * 
#
summary(lm.beta(lm(Total.z~Both_Acc,data = ReasEnd))) # 0.00287 **    
#
#summary(lm(Both_RT~Verbal.z,data = ReasEnd))
#summary(lm(Both_RT~Nonverbal.z,data = ReasEnd))
summary(lm(Both_RT~Total.z,data = ReasEnd))

#summary(lm(Both_Acc~Verbal.z,data = CalcEnd ))
#summary(lm(Both_Acc~Nonverbal.z,data = CalcEnd ))
summary(lm(Both_Acc~Total.z,data = CalcEnd ))
#summary(lm(Both_RT~Verbal.z,data = CalcEnd )) # 0.0229 *  
#summary(lm(Both_RT~Nonverbal.z,data = CalcEnd ))
summary(lm(Both_RT~Total.z,data = CalcEnd )) # 0.0319 *  






