fire <- read.csv("/Users/yuyanzhang/Desktop/RWorkshop/Week 5 - Introduction to Modeling/forestfires.csv")
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
# Large p-value means that the additional factors do not contribute to predicting the value of the response
anova(lm1, lm3)
anova(lm2, lm3)


#####################
#
# Practice Problems
#
###################

# 1. Create a new subset that includes only with an ISI (inital spread index) greater than the median

# 2. Plot the relationship between area and wind

# 3. Add a linear regression line to the plot

# 4. Create a linear model with wind as your predictor, and area as your response
# Call this model lm1_ISI

# 5. Determine if there are any correlated attributes

# 6. If there are any correlated attributes add the interaction between them to a new model 
# Call this model lm2_ISI

# 7. Compare these two models and determine which model is better at predicting the size of the area burned


#####################
#
# Categorical variables
#
###################

#Suppose we want to develop a model to predict the area and also using month and day attributes
class(fire$month)
contrasts(fire$month)
class(fire$day)
contrasts(fire$day)

#Get the right dataset
xtm_fire_withCategorical <- subset(fire, area >= upperwhisk, select = c('FFMC', 'DMC', 'DC', 'ISI', 'temp', 'RH', 'wind', 'rain', 'area','month','day'))
lm4 <- lm(area~temp+FFMC+wind+(DC+DMC)^2+(ISI+FFMC)^2+(temp+FFMC)^2+month+day, data = xtm_fire_withCategorical)

#Practice problem: is lm4 better than lm3? How do you know?



#####################
#
# Evaluating the performance of model: training set and testing set
#
###################

#Why? Because we want to test the robustness of the model on unseen data

#Suppose we want to develop a model to predict the temp. Let's use the entire dataset (not just extreme cases)

#Set up training set and testing set
set.seed(123456) #Setting the seed of the random process
smp_size <- floor(0.75 * nrow(fire)) #We are seperating 75% of the data as the training data, and 25% as the testing data
train_ind <- sample(seq_len(nrow(fire)), size = smp_size)
train <- fire[train_ind, ]
test <- fire[-train_ind, ]

#Train the model on the training set
lm.train <- lm(temp~area+FFMC+wind+(DC+DMC)^2+(ISI+FFMC)^2+FFMC+month+day, data = train)
summary(lm.train)

#Use the trained model to generate predictions for the testing sets
predict(lm.train,test) #The command gives a list of predicted value for each datapoint in the testing set
pred <- as.vector(predict(lm.train,test)) 

#Evaluate the performance of the model based on the predictions: calculating the MSE (mean squared error) of predictions
mse <- function(p, r)
{
  mean((p-r)^2)
  
}

pmse.lm.train<-mse(pred,test$temp)
pmse.lm.train

#Practice question: how does pmse.lm.train compared to the MSE on the training set? Which value better represents the robustness of the model?
#Hint: use mean((lm.train$residuals)^2) to compute the MSE on training set

#####################
#
# Practice Problems
#
###################
#1. Develop a linear model to predict the rain, using all attributes (hint: you can write the formula this way: lm(rain~., data = fire) to indicate you are using all attributes)

#2. Look at the summary of the model. Which attributes are statistically significant? (with p-value < 0.05)

#3. Develop a linear model only using attributes that are statistically significant

#4. Compare model 1 and model 3. Are they statistically different? Which one would you choose?

#5. Split the dataset into trainning and testing set. Then use the training set to train the model you selected. 

#6. Generate predictions for the testing sets.

#7. Compute the mean squared error of the predictions