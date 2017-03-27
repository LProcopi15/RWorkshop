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
summary(lm1)$adj.r.squared # 0.04932201
summary(lm3)$adj.r.squared # -0.05481489


######################
#
# Transformations and Modeling
#
#####################
# Box-cox transformation: used to increase normality of a dataset
library(MASS)

#The best lambda
L<-boxcox(lm3, plotit = F)$x[which.max(boxcox(lm3, plotit = F)$y)] 
L

# Box-cox transform
fire.lm1.boxcox <- lm(area^L~., data = xtm_fire)
summary(fire.lm1.boxcox)

# Look at new diagnostics
par(mfrow=c(2,2))
plot(fire.lm1.boxcox, labels.id = NULL)
par(mfrow=c(1,1))

# Stepwise regression
lm1.step <-step(lm3)
summary(lm1.step)

# Again we can compare these models using multiple aproaches
AIC(fire.lm1.boxcox)
AIC(lm1)
AIC(lm1.step)

# Logistic regression
fert <- read.csv("C:/Users/Student/Documents/UVA 2016-2017/RWorkshop/Week 6 - Assumptions and Transformations/fertility_Diagnosis.txt", header = FALSE)
View(fert)

colnames(fert) <- c("season", "age", "childish.disease", "accident", "surgical", "fevers", "alcohol", "smoking", "hours.sitting", "output")

# Recode the response
class(fert$output)
fert$output <- as.character(fert$output)
fert$output[which(fert$output == "N")] <- 0
fert$output[which(fert$output == "O")] <- 1
fert$output <- as.integer(fert$output)

# Binary logistic regression (logistic regression) model: gives the log-odds on the event
fert.glm <- glm(output~., family = binomial(link = "logit"), data = fert)
summary(fert.glm)


# Prediction using logistic regression model
fert.pred <- predict(fert.glm, type = "response")
# Score table: gives the values of false positive, false negative, true positive and true negative
# Based on a specific cut off probability: 0.3 means that R should classify all probabilities of less than 0.3 as false and greater than 0.3 as true
score.table(fert.pred,fert$output,0.3)
# ROC plot: Ratio of true positives to false positives
# Upper right-hand corner is an ideal curve
plot.roc(fert.pred, fert$output, main = "ROC Curve - Fertility", col = "blue")

######################
#
# Practice Problems
#
#####################
# Use the fertility data for these

# 1. Split the data into a training set (70%) and testing set (30%)

# Use the training set to build your models
# 2. Create a general linear regression model with output as the response and all the other attributes as a predictor

# 3. Determine correlation between attributes

# 4. Add an interaction term with hours.sitting and age 

# 5. Perform a step-wise regression on the model from Q3

# 6. Create a new logistic regression model using the training data

# 7. Create diagnostic plots for all models 

# 8. Compare the models using AIC

# 9. Compare the models using BIC

# 10. Compare the models using ajd-R2 (won't work for logistic)

# 11. Use the first and second linear regression model to predict test values

# 12. Compare these two models using MSE

# 13. Using your logistic regression model construct a confusion matrix

# 14. Using your logistic regression model construct a ROC curve

# 15. Compare the logistic regression model you made with the one we constructed as a class using ROC and confusion matrix