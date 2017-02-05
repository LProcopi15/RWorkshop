######Workshop 2: data visulization, cleaning, and normalization##########
##############Part 1: Reading data and recoding attributes################
ac <- read.table("/Users/yuyanzhang/Desktop/RWorkshop/australian.dat")
View(ac)

#Check the class of attributes
for (i in 1:ncol(ac)){
  print(paste("A",i,":", class(ac[,i]),sep = ""))
}

#Bonus: you can put this into a function (Don't worry about writing a function yet, more to come)
check_class <- function(dataset){
  for (i in 1:ncol(dataset)){
    print(paste("Attribute",i,":", class(dataset[,i]),sep = ""))
  }
}
#Remember to run these lines of code before using the function
check_class(ac)

#We need to recode these attributes into factor type: A1, A4, A5, A6, A8, A9, A11, A12, A15
#We write a loop to save our time!

torecode <- c(1,4,5,6,8,9,11,12,15)
for (i in torecode){
  ac[,i] <- as.factor(ac[,i])
}

#Now, we check if classes of attributes have been changed
check_class(ac)

##############Part 2: Basic visulization################
##############Part 2.1: Numeric variables, hist, boxplot, scatter plot################
#You can do plot histgrams of numeric variables, but first, how can we quickly know which variables are numeric?
var_numeric <- c() #We are going to record which variables are numeric in this vector
for (i in 1:ncol(ac)){
  if (is.numeric(ac[,i]) || is.integer(ac[,i])){
    var_numeric <- c(var_numeric,i)
  }
}
var_numeric

hist(ac[,2])
#Changing the name of the graph
hist(ac[,2], main = "Histogram of A2", xlab = "Some unknown value")
#We can do more! 
hist(ac[,2], main = "Histogram of A2", xlab = "Some unknown value", col = "orange") #Changing colors
?hist
hist(ac[,2], main = "Histogram of A2", xlab = "Some unknown value", col = "orange", breaks = 80)#Changing breaks

#Do we dare to plot all these variables in one graph? Yes we do!
length(var_numeric) #We have 6 numeric variables
par(mfrow=c(2,3))
for (i in var_numeric){
  name = paste("A",i,sep = "")
  hist(ac[,i], main = paste("Histogram of ",name,sep = ""),xlab = "Some unknown value",col = "blue",breaks = 80)
}
par(mfrow=c(1,1))


#######Exercise time: plot the boxplots of all numeric attributes in one screen##############



#Boxplots and outliers evaluation
boxplot(ac$V2, col = "orange", outcol = "blue", main= "Boxplot of V2")
boxplot(ac$V2, col = "orange", outcol = "blue" ,outcex = 4, main= "Boxplot of V2")
#What if we want to know which points lie out side the upper whiskers?
bp <- boxplot(ac$V2)
bp$out
which(ac$V2 %in% bp$out)


#Correlation and scatter plots 
plot(ac$V2,ac$V13)
cor(ac$V2,ac$V13) #Error message!
cor(ac$V2,ac$V13, use = "complete")
#Adding a regression line
abline(lm(ac$V2 ~ ac$V13),col="orange")


#Alternatively: interactive plots
library(plotly) #You need to install ggplot library
plot_ly(ac, x = ac$V2, y = ac$V13, text = paste("Approval: ", ac$V15),
        mode = "markers", color = ac$V2, size = ac$V2)
boxplot(ac$V13)
ac <- ac[-which(ac$V13==max(ac$V13)),]
nrow(ac)
plot_ly(ac, x = ac$V2, y = ac$V13, text = paste("Approval: ", ac$V15),
        mode = "markers", color = ac$V2, size = ac$V2)


#Scatter plot matrix
pairs(ac[,var_numeric], 
      main="Simple Scatterplot Matrix")

#Correlation matrix
cor(ac[,var_numeric],use = "complete")

#Alternatively, to make your report look nicer
library(corrgram)
corrgram(ac[,var_numeric], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlation matrix of numeric variables")
?corrgram

#Alternatively, use corplot library
library(corrplot)
corrplot(cor(ac[,var_numeric],use = "complete"), method="number")
corrplot(cor(ac[,var_numeric],use = "complete"), method="color")
corrplot(cor(ac[,var_numeric],use = "complete"), method="circle")
corrplot(cor(ac[,var_numeric],use = "complete"), method="pie")

#######Excercise: give the correlation plot a title, x axis label, and y axis label

##############Part 2.2: Categorical variables: barplot################
#Barplot: categorical variable
#####Practice: Where are our categorical variables?

table(ac$V4) #Table function gives you the count of each levels
barplot(table(ac$V4), main = "Barplot of V4", xlab = "Levels", ylab = "Frequency")

#####Practice: plot all the categorical variables on the same graph


##############Part 2.3: Numeric against categorical################
#Plot numeric variables against categorical variables
plot(ac$V2,ac$V6)
plot(ac$V6,ac$V2)

library(ggplot2)

check_class(ac)
####Excercise: plot all numeric variables against V15 to see how each variable influence credit approval






##############Part 3: Data cleaning################
#Are they any missing values?
which(is.na(ac))
##################Practice: what percentage of data is missing?


#Where are the missing values?
summary(ac)
ac[!complete.cases(ac),]


#How to deal with missing values?
mean(ac$V2)
mean(ac$V2, na.rm = TRUE )



#Option 1: remove the entry
ac_noNA <- ac[complete.cases(ac),]
#Alternatively
ac_delete_row6 <- ac[-6,]
nrow(ac_delete_row6)
#Option 2: imputation by mean (when the distribution of the variable is close to normal)
hist(ac$V2) 
ac[which(is.na(ac$V2)),2] <- mean(ac$V2,na.rm = TRUE)
which(is.na(ac$V2))
###############Excercise: imputate other numeric variables with MEDIAN

#Option 3: Knn imputation
library(DMwR)
knnOutput <- knnImputation(ac)  # perform knn imputation.
which(is.na(knnOutput))


##############Part 4: Normalization################
#Practice (from your SYS2001): normalize V2 to be in ranges [0,1]


#Practice: normalize all the numeric variables



#Why normalization?
#Multivariate analysis, neural network etc.



#####Bonus visulization trick: word cloud (More advanced visulization to come next time)##############
require(devtools)
install.packages("devtools")
install_github("lchiffon/wordcloud2")
library(wordcloud2)
wordcloud2(data = demoFreq)
View(demoFreq)
head(demoFreq)
wordcloud2(demoFreq, color = "random-light", backgroundColor = "grey")
letterCloud(demoFreq, word = "UVA", size = 2)