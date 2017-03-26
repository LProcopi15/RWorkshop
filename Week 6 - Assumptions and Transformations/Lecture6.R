fire <- read.csv("C:/Users/Student/Documents/UVA 2016-2017/RWorkshop/Week 5 - Introduction to Modeling/forestfires.csv")
lm1 <- lm(area~temp, data = xtm_fire)
lm3 <- lm(area~temp+FFMC+wind+(DC+DMC)^2+(ISI+FFMC)^2+(temp+FFMC)^2, data = xtm_fire)

######################
#
# Diagnostic plots
#
#####################
# 1. Residual vs. Fitted: constant variance, no pattern, even scatter about the mean
# 2. Normal Q-Q: dots are on the line
# 3. Scale-Location: constant variance, no pattern, even scatter about the mean
# 4. Residual vs. Leverage: Points outside of Cook's distance are influential points
par(mfrow=c(2,2))
plot(lm1, labels.id = NULL)
par(mfrow=c(1,1))
# Based on Cook's distance we see that 32 is an influential point
xtm_fire[32,]
hist(xtm_fire$area)
summary(xtm_fire$area)

par(mfrow=c(2,2))
plot(lm3, labels.id = NULL)
par(mfrow=c(1,1))

######################
#
# Model Comparison
#
#####################

# AIC - smaller is better
AIC(lm1) # 834.4967
AIC(lm3) # 848.3075

# BIC - smaller is better
AIC(lm1,k=log(nrow(xtm_fire))) # 840.9733
AIC(lm3,k=log(nrow(xtm_fire))) # 872.0552

# adj-R2 - closer to 1 is better
summary(lm1)$adj.r.squared
summary(lm3)$adj.r.squared


######################
#
# Transformations and Modeling
#
#####################

# Stepwise
lm.cas2.step<-step(lm.cas2)

# Box-cox
library(MASS)
boxcox(lm1) 
# Value near 0 thus we need to perform a log transform

#The best lambda
L<-boxcox(accdmg.lm1, plotit = F)$x[which.max(boxcox(accdmg.lm1, plotit = F)$y)] 
L

# Box-cox transform
xdmgnd.lm1.boxcox<-lm(ACCDMG^L ~TEMP + TRNSPD + TONS + CARS + HEADEND1,data=xdmgnd)
summary(xdmgnd.lm1.boxcox)