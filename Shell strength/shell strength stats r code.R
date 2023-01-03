SSforR10.25.18 <- read.csv("C:/Users/dell4sba/Dropbox/scallop/shell strength/SSforR10-25-18.csv")
SSforR<-SSforR
aggregate(Mpa ~ ph+temp+Treatment, data=SSforR, mean)
aggregate(Mpa ~ shell, data=SSforR, mean)

library(lmerTest)
library(MASS)
library(lme4)
aov<-aov(Mpa~ph*temp*factor(tank), data=SSforR10.25.18)
summary(aov)
mod <- lmer(logMpa ~ factor(ph) * factor(temp)+(1|tank), data=SSforR10_25_18)
summary(mod)
anova(mod)

library()


TukeyHSD(mod)
aggregate(Mpa ~ ph+temp+Treatment, data=SSforR10.25.18, mean)
plot(aov,1)

plot(aov,2)

# Extract the residuals
aov_residuals <- residuals(object = aov)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
shapiro.test(SSforR$logMpa)
plot(SSforR$Mpa)
install.packages("nortest")
library(nortest)
ad.test(SSforR$logMpa)
bartlett.test(logMpa~ interaction(temp,ph), data=SSforR)
leveneTest(logMpa~ interaction(temp,ph), data=SSforR)
hist(SSforR$logMpa)


aov<-aov(logMpa~ph, data=SSforR) #switched ph and temp
summary(aov)
TukeyHSD(aov)

aov<-aov(logMpa~temp, data=SSforR)
summary(aov)
TukeyHSD(aov)

aov<-aov(logMpa~Treatment, data=SSforR)
summary(aov)
TukeyHSD(aov)


hist(HV.and.MPa.sqm$thickness..mm., xlab="Shell thickness (mm)", col="skyblue", main="")

shapiro.test(HV.and.MPa.sqm$thic)


thick.area<-lm(thickness..mm. ~ area, data=HV.and.MPa.sqm)
summary(thick.area)

