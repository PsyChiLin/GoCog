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
RST<-read.csv("../GoCogdata/NoneRST_All.csv", h=T)[,c(1:3,18)]
GoCog<-read.csv("../GoCogdata/GoCog_20180404.csv", h=T)[,1:7]
RSTdata <- full_join(GoCog,RST)
RSTdata$Both_Acc <- RSTdata$Both_Acc*100
RSTdata$Both_RT <- RSTdata$Both_RT/1000
head(RSTdata)

Overall <- aggregate(data = RSTdata, cbind(Both_Acc,Both_RT,Total)~Subj,FUN = mean)
summary(lm.beta(lm(Both_Acc~Total,data = Overall)))
summary(lm.beta(lm(Both_RT~Total,data = Overall)))

GoStage <- aggregate(data = RSTdata, cbind(Both_Acc,Both_RT,Total)~Subj+GoStage,FUN = mean)
Opne <- filter(GoStage, GoStage == "Open")
Mid <- filter(GoStage, GoStage == "Mid")
End <- filter(GoStage, GoStage == "End")


summary(lm(Both_Acc~Total,data =Opne))
summary(lm(Both_RT~Total,data =Opne))

summary(lm(Both_Acc~Total,data =Mid))
summary(lm(Both_RT~Total,data =Mid))

summary(lm(Both_Acc~Total,data =End))
summary(lm(Both_RT~Total,data =End))

CogTask <- aggregate(data = RSTdata, cbind(Both_Acc,Both_RT,Total)~Subj+CogTask,FUN = mean)
None <- filter(CogTask, CogTask == "None")
Spat <- filter(CogTask, CogTask == "Spat")
Reas <- filter(CogTask, CogTask == "Reas")
Calc <- filter(CogTask, CogTask == "Calc")

# summary(lm(Both_Acc~Verbal,data =None))
# summary(lm(Both_Acc~Nonverbal,data =None))
# summary(lm(Both_Acc~Total,data =None))
# summary(lm(Both_RT~Verbal,data =None))
# summary(lm(Both_RT~Nonverbal,data =None))
# summary(lm(Both_RT~Total,data =None))
# 
# summary(lm(Both_Acc~Verbal,data =Spat))
# summary(lm(Both_Acc~Nonverbal,data =Spat))
# summary(lm(Both_Acc~Total,data =Spat))
# summary(lm(Both_RT~Verbal,data =Spat))
# summary(lm(Both_RT~Nonverbal,data =Spat))
# summary(lm(Both_RT~Total,data =Spat))
# 
# summary(lm(Both_Acc~Verbal,data =Reas)) #  0.0106 * 
# summary(lm(Both_Acc~Nonverbal,data =Reas)) # 3e-04 ***
summary(lm.beta(lm(Total~Both_Acc,data =Reas))) # 0.000438 ***
# summary(lm(Both_RT~Verbal,data =Reas))
# summary(lm(Both_RT~Nonverbal,data =Reas))
# summary(lm(Both_RT~Total,data =Reas))
# 
# summary(lm(Both_Acc~Verbal,data = Calc))
# summary(lm(Both_Acc~Nonverbal,data = Calc))
# summary(lm(Both_Acc~Total,data = Calc))
# summary(lm(Both_RT~Verbal,data = Calc))# 0.00194 **
# summary(lm(Both_RT~Nonverbal,data = Calc))  #0.0539 . 
# summary(lm(Both_RT~Total,data = Calc))#0.00931 ** 


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

#summary(lm(Both_Acc~Verbal,data = NoneOpen))
#summary(lm(Both_Acc~Nonverbal,data = NoneOpen))
summary(lm(Both_Acc~Total,data = NoneOpen))
#summary(lm(Both_RT~Verbal,data = NoneOpen))
#summary(lm(Both_RT~Nonverbal,data = NoneOpen))
summary(lm(Both_RT~Total,data = NoneOpen))

#summary(lm(Both_Acc~Verbal,data = SpatOpen))
#summary(lm(Both_Acc~Nonverbal,data = SpatOpen))
summary(lm(Both_Acc~Total,data = SpatOpen))
#summary(lm(Both_RT~Verbal,data = SpatOpen))
#summary(lm(Both_RT~Nonverbal,data = SpatOpen))
summary(lm(Both_RT~Total,data = SpatOpen))

#summary(lm(Both_Acc~Verbal,data = ReasOpen)) #  0.0575 . 
#summary(lm(Both_Acc~Nonverbal,data = ReasOpen)) # 0.000214 ***
#summary(lm(Both_Acc~Total,data = ReasOpen)) # 0.00131 ** 
##
summary(lm.beta(lm(Total~Both_Acc,data = ReasOpen)))
##
#summary(lm(Both_RT~Verbal,data = ReasOpen))
#summary(lm(Both_RT~Nonverbal,data = ReasOpen))
summary(lm(Both_RT~Total,data = ReasOpen))

#summary(lm(Both_Acc~Verbal,data = CalcOpen ))
#summary(lm(Both_Acc~Nonverbal,data = CalcOpen ))
summary(lm(Both_Acc~Total,data = CalcOpen))
#summary(lm(Both_RT~Verbal,data = CalcOpen ))
#summary(lm(Both_RT~Nonverbal,data = CalcOpen ))
summary(lm(Both_RT~Total,data = CalcOpen ))

# Mid
#summary(lm(Both_Acc~Verbal,data = NoneMid))
#summary(lm(Both_Acc~Nonverbal,data = NoneMid))
summary(lm(Both_Acc~Total,data = NoneMid))
#summary(lm(Both_RT~Verbal,data = NoneMid))
#summary(lm(Both_RT~Nonverbal,data = NoneMid))
summary(lm(Both_RT~Total,data = NoneMid))

#summary(lm(Both_Acc~Verbal,data = SpatMid))
#summary(lm(Both_Acc~Nonverbal,data = SpatMid))
summary(lm(Both_Acc~Total,data = SpatMid))
#summary(lm(Both_RT~Verbal,data = SpatMid)) #  0.0498 *  
#summary(lm(Both_RT~Nonverbal,data = SpatMid))
summary(lm(Total~Both_RT,data = SpatMid)) # 0.0379 * 

#summary(lm(Both_Acc~Verbal,data = ReasMid)) # 0.243  
#summary(lm(Both_Acc~Nonverbal,data = ReasMid)) #0.0176 * 
#
summary(lm.beta(lm(Total~Both_Acc,data = ReasMid))) #  0.041 *   
#
#summary(lm(Both_RT~Verbal,data = ReasMid))
#summary(lm(Both_RT~Nonverbal,data = ReasMid))
summary(lm(Both_RT~Total,data = ReasMid))

#summary(lm(Both_Acc~Verbal,data = CalcMid ))
#summary(lm(Both_Acc~Nonverbal,data = CalcMid ))
summary(lm(Both_Acc~Total,data = CalcMid ))
#summary(lm(Both_RT~Verbal,data = CalcMid )) # 0.0275 *  
#summary(lm(Both_RT~Nonverbal,data = CalcMid ))
summary(lm(Both_RT~Total,data = CalcMid ))

# End
#summary(lm(Both_Acc~Verbal,data = NoneEnd))
#summary(lm(Both_Acc~Nonverbal,data = NoneEnd))
summary(lm(Both_Acc~Total,data = NoneEnd))
#summary(lm(Both_RT~Verbal,data = NoneEnd))
#summary(lm(Both_RT~Nonverbal,data = NoneEnd))
summary(lm(Both_RT~Total,data = NoneEnd))

#summary(lm(Both_Acc~Verbal,data = SpatEnd))
#summary(lm(Both_Acc~Nonverbal,data = SpatEnd))
summary(lm(Both_Acc~Total,data = SpatEnd))
#summary(lm(Both_RT~Verbal,data = SpatEnd))  
#summary(lm(Both_RT~Nonverbal,data = SpatEnd))
summary(lm(Both_RT~Total,data = SpatEnd)) 

#summary(lm(Both_Acc~Verbal,data = ReasEnd)) # 0.00213 **  
#summary(lm(Both_Acc~Nonverbal,data = ReasEnd)) # 0.0135 * 
#
summary(lm.beta(lm(Total~Both_Acc,data = ReasEnd))) # 0.00287 **    
#
#summary(lm(Both_RT~Verbal,data = ReasEnd))
#summary(lm(Both_RT~Nonverbal,data = ReasEnd))
summary(lm(Both_RT~Total,data = ReasEnd))

#summary(lm(Both_Acc~Verbal,data = CalcEnd ))
#summary(lm(Both_Acc~Nonverbal,data = CalcEnd ))
summary(lm(Both_Acc~Total,data = CalcEnd ))
#summary(lm(Both_RT~Verbal,data = CalcEnd )) # 0.0229 *  
#summary(lm(Both_RT~Nonverbal,data = CalcEnd ))
summary(lm(Both_RT~Total,data = CalcEnd )) # 0.0319 *  






