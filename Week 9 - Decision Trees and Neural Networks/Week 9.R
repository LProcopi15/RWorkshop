concrete <- read.csv("/Users/yuyanzhang/Desktop/Concrete_Data.csv")

#############Part 1: Neural Network#################
###1.1 Normalize the data########
#Normalize all the predictors to the range 0-1.
for (i in 1:(ncol(concrete))){
  concrete[,i] <- (max(concrete[,i])-concrete[,i])/(max(concrete[,i])-min(concrete[,i]))
}

#Why? You always need to normalize numeric data before fitting a neural network in order to make the algorithm converge. 

###1.2 Fitting a neural network###
#install.packages("neuralnet")
library(neuralnet)
f <- formula(Concrete.compressive.strength~Cement+Blast.Furnace.Slag+Fly.Ash+Water+Superplasticizer+Coarse.Aggregate+Fine.Aggregate+Age )
nn <- neuralnet(formula = f, data = concrete, hidden = 3, threshold=0.01)
#Create multiple hidden layers, use vector
#nn <- neuralnet(formula = f, data = concrete, hidden = c(3,2))
plot(nn) #Plot the neural network

#####1.3 Creating predictions####
nn.pred <- compute(nn, concrete[,1:(ncol(concrete)-1)]) #Feed the fitted model and predictors into this function
nn.pred$net.result

nn.pred_actual <- data.frame(predicted = nn.pred$net.result,actual = concrete$Concrete.compressive.strength)
View(nn.pred_actual)
####1.4 Model evaluation: in this problem, we calculate the correlation between predicted strength and actual strength#########
cor(pred_actual$predicted,pred_actual$actual) #0.9329926593


########Part 1 Practice######
#Split the dataset into training and testing set. 


#Use the training set to fit a neural net


#Generate predictions for the testing set


#Evaluate the result


#Tune your model to make it more accurate!



###########Part 2: Decision Tree###############
#2.1 Fit a decision tree
#install.packages("rpart")
library(rpart)
dt <- rpart(formula = f, data=concrete)
plot(dt)
text(dt)
#2.2 Generate predictions
dt.pred <- predict(dt,concrete)
dt.pred_actual <- data.frame(predicted = dt.pred,actual = concrete$Concrete.compressive.strength)
View(dt.pred_actual)
#3.3 Calculate the correlation
cor(dt.pred_actual$predicted,dt.pred_actual$actual)


###########Part 3: Random Forest#########
#The intuition behind a random forest is to take a bunch of simple decision trees and combine their predictions 
#3.1 Fit a random forest
#install.packages("randomForest")
library(randomForest)
rf <- randomForest(formula = f, data = concrete)

# Importance of each predictor.
print(importance(rf,type = 2)) #let's not worry about type for now because it has to do with the information gain theory

#3.2 Generate prediction
rf.pred <- predict(rf,concrete)
rf.pred_actual <- data.frame(predicted = rf.pred,actual = concrete$Concrete.compressive.strength)
View(rf.pred_actual)

#3.3 Calculate the correlation
cor(rf.pred_actual$predicted,rf.pred_actual$actual) #How does this compare to a single decision tree

#3.4 Tune your model!
#Here only introduce two parameter: mtry and ntree
rf2 <- randomForest(formula = f, data = concrete, ntree = 200, mtry = 3)


#######Part 3 Practice#######
#Split the dataset into training and testing set. 


#Use the training set to fit a random forest model


#Use the training set to fit a single decision tree model


#Generate predictions for the testing set


#Evaluate the result. Which model is better?


#Try different parameters to make your random forest more accurate!



#####Practice problem: PIMA dataset######
#Dataset information: https://www.kaggle.com/uciml/pima-indians-diabetes-database
#Pregnancies: Number of times pregnant
#Glucose: Plasma glucose concentration a 2 hours in an oral glucose tolerance test
#BloodPressure: Diastolic blood pressure (mm Hg)
#SkinThickness: Triceps skin fold thickness (mm)
#Insulin: 2-Hour serum insulin (mu U/ml)
#BMI: Body mass index (weight in kg/(height in m)^2)
#DiabetesPedigreeFunction: Diabetes pedigree function
#Age: Age (years)
#Outcome: Class variable (0 or 1)
pima <- read.csv("/Users/yuyanzhang/Desktop/diabetes.csv")
View(pima)
#Our challenge is to come up a model to predict the outcome!


#Step 1: Split the dataset into training set and testing set


#Step 2: Fit a model (If you want neuralnet model, be sure to normalize the data first)


#Step 3: Generate predictions for the testing set
#Hint: if it is a neuralnet model

#Step 4: Evaluate the model performance. What metrics do you want to use?
#Hint: confusion matrix, accuracy, ROC, True positive/False positive.....


#Step 5: Tune your model if needed


#Step 6: Develop another model and repeat step 3,4,5. 


#Step 7: Be creative! Make your model as robust as possible!
#You can cenerate new features (Maybe the ratio between Glucose and age?)
#Or bin the feature into new ranges (May bin the age attribute into five category?)







