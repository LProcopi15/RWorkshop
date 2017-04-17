####################This is the case study for the linear regression modeling###############
#Our goal is to analyze which factor(s) contributes to effective social media campaign (more clicks, likes etc.)
#Which attribute(s) best represents the success of social media campaign? (total number of likes? lifetime.engaged users? share?)


######Step 1: Data exploration############
#1.1 Load the data
fb <- read.table("C:/Users/Student/Documents/UVA 2016-2017/RWorkshop/Week 7 - Case Study (regression modeling)/dataset_Facebook.csv", sep = ";", header = TRUE)
#The header = TRUE parameter will tell R to read the first line in the file as the header of the dataset
View(fb)

#1.2 Check the class of each attribute
check.class <- function(dataset) {
  for(i in 1:ncol(dataset))
    print(paste(colnames(dataset[i]), ": ", class(dataset[,i]), sep = ""))
}

check.class(fb)
#Is there any attribute that needs recoding?


#1.3 Check for missing data

#Use imputation methods to replace missing data, or simply remove the entries with missing values
#It's your call! But remember how you deal with missing values may influence the result of modeling.


#1.4 Look at the summary statistics of the dataaset. 


#1.5 Plot the correlation matrix to see which attributes are highly correlated



#1.6 How does Post.Month influence Page.total.likes?
#Hint: you can plot Page.total.likes agianst Post.Month
#Alternatively, you can plot the average against Post.Month (use ggplot!)
plot(fb$Post.Month,fb$Page.total.likes)
library(ggplot2)
ggplot(fb, aes(x=factor(Post.Month), y=Page.total.likes)) + stat_summary(fun.y="mean", geom="bar")
#Does this trend make sense?
#How helpful is it?

#1.7 How does Post.Month influence Lifetime.Engaged.Users?

#1.8 How does Post.Weekday influence Page.total.like?

#1.9 How does Post.Weekday influence Lifetime.Engaged.Users?

#1.10 Do the same analyses for Post.Hour


#1.11 Based on your exploratory analysis, when do you recommend to post?


######Step 2: Predict Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post############
#2.1 Create a linear model (lm1). Which attributes can we use? 
#Type,category,Post.Month,Post.Weekday,Post.Hour,Paid (Because we only have information for these attributes prior to releasing the campaign)


#2.2 Look at the summary statistics. Which attribute(s) is predictive at confidence level 0.05?


#2.3 How does the model perform? Which metric(s) indicates the robustness of the model?



#2.4 Create diagnostics plots of the model. Is there any violation of assumptions?

#2.5 Create a model (lm2) using only variables that are predictive at 0.05 confidence level

#2.6 Compare lm 2 to lm1. Which one is better and how do you know?


#2.7 Create a stepwise model of lm1 (lm1.step). 



#2.8 Look at the diagnositics plot of lm1.step and lm2. Is there any significant problem?


#Which model do you think is the best? lm1, lm2, or lm1.step?




###########Step3: Training set and testing set###########
#Now, evaluate the model through generating predictions for unseen data
#3.1 Setup the training and testing set (you can reserve 75% of the data as the training set)


#3.2 Use the training set to fit lm1 (lm1.train)


#3.3 Use lm1.train to generate predictions for the testing set and compute the mse


#3.4 Fit the stepwise model of lm1.train (lm.train.step)

#3.5 Use lm.train.step to generate predictions for the testing set and compute the mse

#3.6 Compare two models. Which one do you recommend and why?

##########Step 4: Transformation of response variable#####################
#4.1 Why transforming the response variable: "Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post"?


#4.2 Use a boxcot method to determine the optimal lambda for transformation

#4.3 Transform the response variable and fit a new model (lm1.trans)


#4.4 Look at new diagnostics
