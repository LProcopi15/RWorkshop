#########################
#
# Review problems
#
#########################

# 1. Open the data file 
adult <- read.csv('C:/Users/Student/Documents/UVA 2016-2017/RWorkshop/Week 3 - Visualizations and Data Cleaning/adult.data')
View(adult)

# 2. Name the columns properly (information can be found in information.txt)
# Hint: use the colnames command 
colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'education', 'education.number', 'marital.status', 'occupation', 'relationship', 'race', 'sex', 'capital.gain', 'capital.loss', 'hours.per.week', 'native.country', 'income')

# 3a. Write a loop function to the determine the class of each attribute
check_class <- function(dataset){
  for (i in 1:ncol(dataset)){
    print(paste("Attribute",i,":", class(dataset[,i]),sep = ""))
  }
}

# 3b. Call that function on the adult dataset
check_class(adult)

# 3c. Look at the dataset do these results make sense? Is there anything that obvously needs to be recoded? 
View(adult) # All the categorical are factors, all the numbers are int or numeric - no action needed

# 4. Create a histogram of final wages, make sure to label the axes and title
# Optional: change the color
hist(adult$fnlwgt, xlab = 'Final Wages', main = 'Histogram of Final Wages', col = 'red')

# 5a. Create a box plot of Final Wage and store it as a variable called wage_box
wage_box <- boxplot(adult$fnlwgt)

# Use wage_box to answer the following questions
# 5b. What is the median value of final wage? 
# Hint: There is an example of this in Lecture1.R, practice problem 2b
wage_box$stats[3,] # 178363

# 5c. What are the outlier values for final wage?
wage_box$out # values outside of the whiskers
wage_box_out <- wage_box$out

# 5d. What is the largest outlier value? What is the smallest?
max(wage_box_out) # 1484705
min(wage_box_out) # 415913

# 6a. Create a scatter plot of final wage vs. age
plot(adult$fnlwgt, adult$age, main = "Final wage vs. Age")
### To change the tick marks on the x-axis
ticks <- seq(0, max(adult$fnlwgt), 100000)
axis(side = 1, at = ticks)

# 6b. Determine the correlation for these two attributes
cor(adult$age, adult$fnlwgt) # -0.0766462

# 7. Create a scatterplot matrix that includes: age, final wage, capital gain, capital loss and hours per week
pairs(adult[c('fnlwgt', 'age', 'capital.gain', 'capital.loss', 'hours.per.week')])

# 8. Create a scatterplot of fnlwgt, age and hours.per.week using the plotly library (i.e. plot_ly)
plot_ly(adult, x=adult$fnlwgt, y=adult$age, z=adult$hours.per.week)

#########################
#
# Basic plots completed
#
#########################

### Barplots
barplot(table(adult$native.country))

# Take aways: 
# It seems that there are ?'s in the data to indicate missing information
# There is not much over information available in this plot because there are so many different native countries

### Plotting categorical vs. numeric
plot(adult$sex, adult$fnlwgt, xlab = 'Gender', ylab = 'Final Wage', main = 'Gender vs. Final Wage')

#########################
#
# Data Cleaning
#
#########################

# Remove useless columns - we have education and education number; we only need one
# of these since they reflect the same information
adult$education.number <- NULL
View(adult)

# Datasets use different values to indicate that there is no data for that point
# It appears this dataset uses question marks to indicate missing information
# R uses 'NA' as a key word to indicate missing information - lets replaces the ?
adult[adult == " ?"] <- NA
which(is.na(adult))

### Working with NAs/missing data
# 1. Remove them
AD <- na.omit(adult)
summary(AD)

# 2. Imputation - can be used for missing numerical values
hist(adult$hours.per.week)
length(which(adult$hours.per.week == 99))
summary(adult$hours.per.week)

#   a. Mean imputation 
mean(adult$hours.per.week)
adult$hours.per.week[which(adult$hours.per.week == 99)] <- mean(adult$hours.per.week)

#   b. Correlation based imputuation
# When numeric attributes are highly correlated you can use the correlation to fill in missing values
# Deteremine the correlation - example code; you need numeric attributes for this to work
adult$age <- as.numeric(adult$age)
adult$fnlwgt <- as.numeric(adult$fnlwgt)
symnum(cor(adult[,c('hours.per.week','age','fnlwgt')], use = "complete.obs"))

# If two attributes are highly correlated create a linear regression model based on these two attributes
hist(adult$fnlwgt)
summary(adult$fnlwgt)
which(adult$fnlwgt > 1300000)

lm(hours.per.week~fnlwgt, data = adult)

# Use the intercept and coefficient to filling in missing values
adult[14450,"fnlwgt"]<- 4.063e+01+(-1.807e-06)*adult[14450,"hours.per.week"]
adult[16740,"fnlwgt"]<- 4.063e+01+(-1.807e-06)*adult[16740,"hours.per.week"]
adult[18139,"fnlwgt"]<- 4.063e+01+(-1.807e-06)*adult[18139,"hours.per.week"]

#   c. kNN imputation - machine learning technique 
# Compares missing values to the points near that value to generate a solution
install.packages('DMwR')
library(DMwR)
adult <- knnImputation(adult, k=10)

#########################
#
# Review problems
#
#########################

# Load the Cloud data, label it CD and label the columns 
CD <- read.csv("~/UVA 2015-2016/DS5559/CloudData1.csv", header = FALSE)
colnames(CD) <- c('VI.mean', 'VI.max', 'VI.min', 'VI.mean.distribution', 'VI.contrast', 'VI.entropy', 'VI.second.angular.momentum', 'IR.mean', 'IR.max', 'IR.min')

# 1. Look at the data - use summary, hist, etc. Replace any ?'s with NA
View(CD)
summary(CD)
CD[CD == "?"] <- NA
# 2. Make sure all attributes are type numeric
check_class(CD)
CD$VI.max <- as.numeric(CD$VI.max)
CD$VI.max <- as.numeric(CD$VI.max)
CD$VI.min <- as.numeric(CD$VI.min)
CD$VI.mean.distribution <- as.numeric(CD$VI.mean.distribution)
CD$VI.contrast <- as.numeric(CD$VI.contrast)
CD$VI.entropy <- as.numeric(CD$VI.entropy)
CD$VI.second.angular.momentum <- as.numeric(CD$VI.second.angular.momentum)
CD$IR.mean <- as.numeric(CD$IR.mean)
CD$IR.max <- as.numeric(CD$IR.max)

# 3. Determine if there is any highly correlated attributes 
symnum(cor(CD[,1:10], use = "complete.obs"))

# 4. For any  highly correlated attributes, use correlation based imputation to replace missing values
# Hint: to find which values are missing use which(is.na(CD$VI.entropy))

### VI.entropy and VI.mean.distribution are highyly correlated
which(is.na(CD$VI.entropy))
which(is.na(CD$VI.mean.distribution))
# Use VA.mean.distribution to replace missing valyes of VA.entropy
lm(VI.entropy~VI.mean.distribution,data=CD)
CD[104,"VI.entropy"]<- 538.1309+(-0.8837)*CD[104,"VI.mean.distribution"]
CD[368,"VI.entropy"]<- 538.1309+(-0.8837)*CD[368,"VI.mean.distribution"]
CD[483,"VI.entropy"]<- 538.1309+(-0.8837)*CD[483,"VI.mean.distribution"]
CD[549,"VI.entropy"]<- 538.1309+(-0.8837)*CD[549,"VI.mean.distribution"]
CD[686,"VI.entropy"]<- 538.1309+(-0.8837)*CD[686,"VI.mean.distribution"]
CD[793,"VI.entropy"]<- 538.1309+(-0.8837)*CD[793,"VI.mean.distribution"]
# Use VA.entropy to replace missing values of VA.mean.distribution
lm(VI.mean.distribution~VI.entropy,data=CD)
CD[267,"VI.mean.distribution"]<- 577.460+(-1.006)*CD[267,"VI.entropy"]
CD[553,"VI.mean.distribution"]<- 577.460+(-1.006)*CD[553,"VI.entropy"]
CD[612,"VI.mean.distribution"]<- 577.460+(-1.006)*CD[612,"VI.entropy"]
CD[763,"VI.mean.distribution"]<- 577.460+(-1.006)*CD[763,"VI.entropy"]
CD[801,"VI.mean.distribution"]<- 577.460+(-1.006)*CD[801,"VI.entropy"]
# Check that all values are filled
which(is.na(CD$VI.entropy))
which(is.na(CD$VI.mean.distribution))
# 549 is still NA because it was missing for both atributes

### IR.min and IR.max were also highly correlated
which(is.na(CD$IR.max))
which(is.na(CD$IR.min))
# USe IR.min to replace missing values of IR.max
lm(IR.max~IR.min,data=CD)
CD[3, "IR.max"] <- -27.8381+0.2829*CD[3, "IR.min"]
CD[514, "IR.max"] <- -27.8381+0.2829*CD[514, "IR.min"]
CD[553, "IR.max"] <- -27.8381+0.2829*CD[553, "IR.min"]
CD[575, "IR.max"] <- -27.8381+0.2829*CD[575, "IR.min"]
CD[636, "IR.max"] <- -27.8381+0.2829*CD[636, "IR.min"]
CD[734, "IR.max"] <- -27.8381+0.2829*CD[734, "IR.min"]
CD[774, "IR.max"] <- -27.8381+0.2829*CD[774, "IR.min"]
CD[801, "IR.max"] <- -27.8381+0.2829*CD[801, "IR.min"]
# Use IR.max to replace missing values of IR.min
lm(IR.min~IR.max,data=CD)
CD[427, "IR.min"] <- 135.199+2.494*CD[427, "IR.max"]
CD[924, "IR.min"] <- 135.199+2.494*CD[924, "IR.max"]

# 5. Use knn imputation to replace any additional missing values 
summary(CD) # 8 of 10 attributes still have at least 1 missing valu
CD <- knnImputation(CD, k=10)
summary(CD) # No more missing values (NAs)
