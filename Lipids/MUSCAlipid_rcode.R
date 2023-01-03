library(readxl)
library(lme4)
library(lmerTest)
MUSCAlipidsforR <- read_excel("C:/Users/Lindsay/Dropbox/MUSCA/FattyAcids/MUSCAlipidsforR.xlsx")
View(MUSCAlipidsforR)

scalloplipids<-subset(MUSCAlipidsforR, species=="scallop")
mussellipids<-subset(MUSCAlipidsforR, species=="mussel")
View(scalloplipids)

aggregate(percentofDW ~ season + depth, data=scalloplipids, mean)
aggregate(percentofDW ~ season + depth, data=scalloplipids, sd)
aggregate(percentofDW ~ season + depth, data=scalloplipids, length)

aggregate(percentofDW ~ season + depth, data=mussellipids, mean)
aggregate(percentofDW ~ season + depth, data=mussellipids, sd)
aggregate(percentofDW ~ season + depth, data=mussellipids, length)



aov<-aov(percentofDW~season*depth,data=scalloplipids)
summary(aov)
TukeyHSD(aov)
summary(lm(percentofDW~depth*season, data=scalloplipids[which(scalloplipids$season!='T0'),]))
summary(aov)
aov<-aov(percentofDW~season*depth,data=mussellipids)
summary(lm(percentofDW~depth*season, data=mussellipids[which(mussellipids$season!='T0'),]))
summary(aov)
TukeyHSD(aov)
