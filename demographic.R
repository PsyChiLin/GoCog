dta = read.csv("../GoCogdata/GoCog.csv")
dta$Subj <- as.factor(dta$Subj)
dta$SubjGroup <- as.factor(dta$SubjGroup)
head(dta)
dim(dta)
str(dta)

##### Age #####

mean(dta$Age);sd(dta$Age)
min(dta$Age);max(dta$Age)
