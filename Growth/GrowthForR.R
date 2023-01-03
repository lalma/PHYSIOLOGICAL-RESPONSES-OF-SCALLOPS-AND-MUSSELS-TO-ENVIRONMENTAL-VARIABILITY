GrowthForR <- read.csv("C:/Users/Lindsay Alma/Dropbox/MUSCA/Growth/GrowthForR.csv")
View(GrowthForR)



mussel<-subset(GrowthForR, Species=="mussel")
scallop<-subset(GrowthForR, Species=="scallop")
View(scallop)
View(mussel)

aggregate(growth_month ~ season+Depth, data=scallop, mean)
aggregate(growth_month ~ season+Depth, data=scallop, sd)
aggregate(growth_month ~ season+Depth, data=scallop, length)

aggregate(growth_month ~ Depth, data=scallop, mean)
aggregate(growth_month ~ Depth, data=mussel, mean)


growth.aov<-aov(growth_month~Depth*season, data=scallop)
summary(growth.aov)
  TukeyHSD(growth.aov)
  
  growth.aov<-aov(growth_month~Depth, data=scallop)
  summary(growth.aov)
  TukeyHSD(growth.aov)

  
  
aggregate(growth_month ~ season+Depth, data=mussel, mean)
aggregate(growth_month ~ season+Depth, data=mussel, sd)
aggregate(growth_month ~ season+Depth, data=mussel, length)

growth.aov<-aov(growth_month~Depth*season, data=mussel)
summary(growth.aov)
TukeyHSD(growth.aov)



boxplot(growth_month~ season, data=scallop, col=c("purple1", "red"))

