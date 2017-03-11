fire <- read.csv("C:/Users/Student/Documents/forestfires.csv")
View(fire)

#####################
#
# Explore Data
#
####################

# Get summary information
summary(fire)
# No NA's, all numeric data is normalized

# Check class of each attribute
for (i in 1:ncol(fire)){
  print(paste(colnames(fire[i]), ": ", class(fire[,i]),sep = ""))
}
 
# Getting a subset of the data
summary(fire$area)
areabox <- boxplot(fire$area)
upperwhisk <- areabox$stats[5,]

xtm_fire <- subset(fire, area >= upperwhisk, select = c('FFMC', 'DMC', 'DC', 'ISI', 'temp', 'RH', 'wind', 'rain', 'area'))

####################
#
# Basic modeling - Linear Regression
#
####################

# Plot two variables
plot(xtm_fire$temp, xtm_fire$area) 
# Create linear regression model between the two
lm1 <- lm(area~temp, data = xtm_fire)
# Add regression line to plot
abline(lm1, col = "orange")
# Statistical information about this lm
summary(lm1)

# Can add many more factors to this lm
lm2 <- lm(area~temp+FFMC+wind, data = xtm_fire)
summary(lm2)

# Could interactions between variables help us?
pairs(xtm_fire[c('FFMC', 'DMC', 'DC', 'ISI', 'temp', 'RH', 'wind', 'area')])
symnum(cor(xtm_fire[c('FFMC', 'DMC', 'DC', 'ISI', 'temp', 'RH', 'wind', 'area')]))
# Highly correlated: DC and DMC, ISI and FFMC, temp and FFMC
# Add these correlated attributes to a model
lm3 <- lm(area~temp+FFMC+wind+(DC+DMC)^2+(ISI+FFMC)^2+(temp+FFMC)^2, data = xtm_fire)
summary(lm3)

# compare models
anova(lm1, lm2)
anova(lm1, lm3)
anova(lm2, lm3)

#####################
#
# Practice Problems
#
####################

# 1. Create a new subset that includes only with an ISI (inital spread index) greater than the median

# 2. Create a linear model with area and wind as your predictors, and area as your response
# Call this model lm1_ISI

# 3. Determine if there are any correlated attributes

# 4. If there are any correlated attributes add the interaction between them to a new model 
# Call this model lm2_ISI

# 5. Compare these two models and determine which model is better at predicting the size of the area burned
