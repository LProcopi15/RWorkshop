#########################
#
# Review problems
#
#########################

# 1. Open the data file 

# 2. Name the columns properly (information can be found in information.txt)
# Hint: use the colnames command 

# 3a. Write a loop function to the determine the class of each attribute

# 3b. Call that function on the adult dataset


# 3c. Look at the dataset do these results make sense? Is there anything that obvously needs to be recoded? 

# 4. Create a histogram of final wages, make sure to label the axes and title
# Optional: change the color

# 5a. Create a box plot of Final Wage and store it as a variable called wage_box

# Use wage_box to answer the following questions
# 5b. What is the median value of final wage? 
# Hint: There is an example of this in Lecture1.R, practice problem 2b

# 5c. What are the outlier values for final wage?


# 5d. What is the largest outlier value? What is the smallest?

# 6a. Create a scatter plot of final wage vs. age

# 6b. Determine the correlation for these two attributes

# 7. Create a scatterplot matrix that includes: age, final wage, capital gain, capital loss and hours per week

# 8. Create a scatterplot of fnlwgt, age and hours.per.week using the plotly library (i.e. plot_ly)

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
adult <- read.table('C:/Users/Student/Documents/UVA 2016-2017/RWorkshop/Week 3 - Visualizations and Data Cleaning/adult.data', sep = ',')
colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'education', 'education.number', 'marital.status', 'occupation', 'relationship', 'race', 'sex', 'capital.gain', 'capital.loss', 'hours.per.week', 'native.country', 'income')

summary(adult)

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
adult <- na.omit(adult)
summary(adult)

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
CD <- read.csv()
colnames(CD) <- c('VI.mean', 'VI.max', 'VI.min', 'VI.mean.distribution', 'VI.contrast', 'VI.entropy', 'VI.second.angular.momentum', 'IR.mean', 'IR.max', 'IR.min')

# 1. Look at the data - use summary, hist, etc. Replace any ?'s with NA

# 2. Make sure all attributes are type numeric

# 3. Determine if there is any highly correlated attributes 

# 4. For any  highly correlated attributes, use correlation based imputation to replace missing values
# Hint: to find which values are missing use which(is.na(CD$VI.entropy))

# 5. Use knn imputation to replace any additional missing values 


