####################This is the case study for the linear regression modeling###############
#Our goal is to analyze which factor(s) contributes to effective social media campaign (more clicks, likes etc.)
#Which attribute(s) best represents the success of social media campaign? (total number of likes? lifetime.engaged users? share?)


######Step 1: Data exploration############
#1.1 Load the data
fb <- read.table("/Users/yuyanzhang/Desktop/RWorkshop/Week 7 - Case Study (regression modeling)/dataset_Facebook.csv", sep = ";", header = TRUE)
#The header = TRUE parameter will tell R to read the first line in the file as the header of the dataset
View(fb)

#1.2 Check the class of each attribute
for (i in 1:ncol(fb)){
  print(paste(colnames(fb)[i],class(fb[,i]), sep = ": "))
}

#Is there any attribute that needs recoding?
fb$Category <- as.factor(fb$Category)
fb$Post.Month <- as.factor(fb$Post.Month)
fb$Post.Weekday <- as.factor(fb$Post.Weekday)
fb$Post.Hour <- as.factor(fb$Post.Hour)
fb$Paid <- as.factor(fb$Paid)

#1.3 Check for missing data
which(is.na(fb))
#Use imputation methods to replace missing data, or simply remove the entries with missing values
#It's your call! But remember how you deal with missing values may influence the result of modeling.
fb <- fb[complete.cases(fb),]
which(is.na(fb))

#1.4 Look at the summary statistics of the dataaset. 
summary(fb)

#1.5 Plot the correlation matrix to see which attributes are highly correlated
num_var <- c()
for (i in 1:ncol(fb)){
  if ((class(fb[,i])=="integer") || class(fb[,i])=="numeric"){
    num_var <- c(num_var,i)
  }
}
symnum(cor(fb[,num_var]))


#1.6 How does Post.Month influence Page.total.likes?
#Hint: you can plot Page.total.likes agianst Post.Month
#Alternatively, you can plot the average against Post.Month (use ggplot!)
plot(fb$Post.Month,fb$Page.total.likes)
library(ggplot2)
ggplot(fb, aes(x=factor(Post.Month), y=Page.total.likes)) + stat_summary(fun.y="mean", geom="bar")
#Does this trend make sense?
#How helpful is it?

#1.7 How does Post.Month influence Lifetime.Engaged.Users?
plot(fb$Post.Month,fb$Lifetime.Engaged.Users)
ggplot(fb, aes(x=factor(Post.Month), y=Lifetime.Engaged.Users)) + stat_summary(fun.y="mean", geom="bar")


#1.8 How does Post.Weekday influence Page.total.like?
plot(fb$Post.Weekday,fb$Page.total.likes)
#1.9 How does Post.Weekday influence Lifetime.Engaged.Users?
plot(fb$Post.Weekday,fb$Lifetime.Engaged.Users)
#1.10 Do the same analyses for Post.Hour
plot(fb$Post.Hour,fb$Page.total.likes)
plot(fb$Post.Hour,fb$Lifetime.Engaged.Users)

#1.11 Based on your exploratory analysis, when do you recommend to post?


######Step 2: Predict Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post############
#2.1 Create a linear model (lm1). Which attributes can we use? 
#Type,category,Post.Month,Post.Weekday,Post.Hour,Paid (Because we only have information for these attributes prior to releasing the campaign)
lm1 <- lm(Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post~Type+Category+Post.Hour+Post.Month+Post.Weekday+Paid, data = fb)


#2.2 Look at the summary statistics. Which attribute(s) is predictive at confidence level 0.05?
summary(lm1)

#2.3 How does the model perform? Which metric(s) indicates the robustness of the model?
AIC(lm1)
BIC(lm1)


#2.4 Create diagnostics plots of the model. Is there any violation of assumptions?
par(mfrow = c(2,2))
plot(lm1)
par(mfrow = c(1,1))


#2.5 Create a model (lm2) using only variables that are predictive at 0.05 confidence level
lm2 <- lm(Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post~Type+Category+Post.Month, data = fb)

#2.6 Compare lm 2 to lm1. Which one is better and how do you know?
AIC(lm2)
anova(lm1,lm2)

#2.7 Create a stepwise model of lm1 (lm1.step). 
lm1.step <- step(lm1) 


#2.8 Look at the diagnositics plot of lm1.step and lm2. Is there any significant problem?
plot(lm1.step)


#Which model do you think is the best? lm1, lm2, or lm1.step?
#Use adjusted R-squre, AIC, BIC, etc. to evaluate the performance of models


###########Step3: Training set and testing set###########
#Now, evaluate the model through generating predictions for unseen data
#3.1 Setup the training and testing set (you can reserve 75% of the data as the training set)
set.seed(123)
train_idx <- sample(seq(1:nrow(fb)), size = 0.75*nrow(fb))
train_data <- fb[train_idx,]
test_data <- fb[-train_idx,]

#3.2 Use the training set to fit lm1 (lm1.train)
lm1.train <- lm(Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post~Type+Category+Post.Hour+Post.Month+Post.Weekday+Paid, data = train_data)

#3.3 Use lm1.train to generate predictions for the testing set and compute the mse

mse <- function(p, r)
{
  mean((p-r)^2)
}

id <- which(!(test_data$Post.Hour %in% levels(train_data$Post.Hour)))
test_data$Post.Hour[id] <- NA

lm1.pred <- predict.lm(lm1.train,test_data)
mse(lm1.pred,test_data$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post)

#3.4 Fit the stepwise model of lm1.train (lm.train.step)
lm1.train.step <- step(lm1.train)

#3.5 Use lm.train.step to generate predictions for the testing set and compute the mse
lm1.step.pred <- predict(lm1.train.step,test_data)
mse(lm1.step.pred,test_data$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post)

#3.6 Compare two models. Which one do you recommend and why?

##########Step 4: Transformation of response variable#####################
#4.1 Why transforming the response variable: "Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post"?


#4.2 Use a boxcot method to determine the optimal lambda for transformation
library(MASS)
L<-boxcox(lm1, plotit = F)$x[which.max(boxcox(lm1, plotit = F)$y)] 
L

#4.3 Transform the response variable and fit a new model (lm1.trans)
lm1.trans <- lm(Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post^L~Type+Category+Post.Hour+Post.Month+Post.Weekday+Paid, data = fb)
summary(lm1.trans)

#4.4 Look at new diagnostics
par(mfrow=c(2,2))
plot(lm1.trans, labels.id = NULL)
par(mfrow=c(1,1))