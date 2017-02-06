#######################
#
# Lecture 1: R Basics
#
#######################

### Data Types

# 1. int - integer whole numbers 
# put an L after the number to ensure to is read as an integer
type <- 2L
class(type)
# 2. num - numbers that can have decimal places
# 3. character - any character, treated as non-numeric
# 4. Logical - True or False 
# 5. Vector
type <- c('a', 'b', 'c')
type
class(type)
# 6. Matrices
type <- matrix(c('a', 'b', 'c', 'd'), nrow = 2, ncol = 2, byrow = TRUE)
# 7. Factors - a vector with distinct values of the elements
bike <- read.csv("C:/Users/Student/Documents/UVA 2016-2017/RWorkshop/bike.csv")
View(bike)
summary(bike)
str(bike)

# Change day of week to factor
str(bike$weekday)
bike$weekday[which((bike$weekday)    == 6)] <- "Sat"  
bike$weekday[which((bike$weekday)    == 0)] <- "Sun"  
bike$weekday[which((bike$weekday)    == 1)] <- "Mon"  
bike$weekday[which((bike$weekday)    == 2)] <- "Tue"  
bike$weekday[which((bike$weekday)    == 3)] <- "Wed"  
bike$weekday[which((bike$weekday)    == 4)] <- "Thr"  
bike$weekday[which((bike$weekday)    == 5)] <- "Fri"  
class(bike$weekday)
bike$weekday <- as.factor(bike$weekday)

#### Basic visualizations
str(bike)
# histogram
hist(bike$cnt)
# box plot
boxplot(bike$cnt)
boxplot(cnt~weathersit,data=bike, main="Bike Rentals", 
        xlab="Weather Classification", ylab="Total Count of Bikes")
# bar
barplot(table(bike$weathersit))

### String manipulation
bike$weathersit[which((bike$weathersit)    == 1)] <- "      THIS WEATHER is awesome"  
bike$weathersit[which((bike$weathersit)    == 2)] <- "this weather is okay     "  
bike$weathersit[which((bike$weathersit)    == 3)] <- "   this weather is not great   "
bike$weathersit[which((bike$weathersit)    == 4)] <- "Yup THIS WEATHER IS THE WORST"
bike$weathersit <- as.factor(bike$weathersit)
summary(bike$weathersit)

# First we have to install a package which enables us to use the desired function
install.packages('stringr')
# We only need to install the package once, but need to load the library every time
library(stringr)
# Now we can manipulate these strings
bike$weathersit <- str_trim(bike$weathersit)
bike$weathersit <- toupper(bike$weathersit) 
bike$weathersit <- tolower(bike$weathersit)

# After using string manipulating of this attribute, it is converted into type character
# We need to redefine it again as a factor
class(bike$weathersit)
bike$weathersit <- as.factor(bike$weathersit)

# Character matching
grepl("worst", bike$weathersit)
grep("worst", bike$weathersit)
grep("^y", bike$weathersit) 


#############################
#
# Practice Problems
#
#############################

# ***IMPORTANT NOTE: to change more data types into a different one the general 
# command is as.datatype (ex. as.factor, as.Date). Also, if you are confused about
# the meaning or the input of a command you can use the following format:
?as.Date

# For 1b, in order to determine which numbers correspond to the seasons you can 
# find this information in the description document

# 1a) Change month to a factor with the levels Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sept, Oct, Nov, Dec
summary(bike$mnth)
bike$mnth[which((bike$mnth)    == 1)] <- "Jan"  
bike$mnth[which((bike$mnth)    == 2)] <- "Feb"  
bike$mnth[which((bike$mnth)    == 3)] <- "Mar"  
bike$mnth[which((bike$mnth)    == 4)] <- "Apr"  
bike$mnth[which((bike$mnth)    == 5)] <- "May"  
bike$mnth[which((bike$mnth)    == 6)] <- "Jun"
bike$mnth[which((bike$mnth)    == 7)] <- "Jul"  
bike$mnth[which((bike$mnth)    == 8)] <- "Aug"  
bike$mnth[which((bike$mnth)    == 9)] <- "Sep"  
bike$mnth[which((bike$mnth)    == 10)] <- "Oct"  
bike$mnth[which((bike$mnth)    == 11)] <- "Nov"  
bike$mnth[which((bike$mnth)    == 12)] <- "Dec"
bike$mnth <- as.factor(bike$mnth)
levels(bike$mnth)

# 1b) Change season to a factor with the levels Win, Spr, Sum, Fall
summary(bike$season)
bike$season[which((bike$season) == 1)] <- "Spr"
bike$season[which((bike$season) == 2)] <- "Sum"
bike$season[which((bike$season) == 3)] <- "Fall"
bike$season[which((bike$season) == 4)] <- "Win"
bike$season <- as.factor(bike$season)

# 1c) Change dteday to a date element
  # Note: be sure to check that the dates were processed correctly
# First we can view the output of as.Date
as.Date(bike$dteday)
# We see that these dates don't make  (i.e. "0002-12-20")
# Use the ? to determine how to fix this; we see that we can specify the format
# View the dataset, determine the proper format, then specify that
as.Date(bike$dteday, format = "%m/%d/%Y")
# This is the proper format, update the values of bike$dteday
bike$dteday <- as.Date(bike$dteday, format = "%m/%d/%Y")

# 2a) Create a boxplot of the count of casual users
casbox <- boxplot(bike$casual) 
# Recall you can add labels which are helpful for reports and clarity
boxplot(bike$casual, ylab = "Count of Users", main = "Boxplot of Casual User Count")

# 2b) What is the median?
median(bike$casual) # This can be visually checked on the boxplot
# Also, notice above I defined casbox as the boxplot - we can use this to determine information about the variable
casbox$stats[3]
# Why 3? For a box plot, 5 different statistics are stored in the stats vector. They are:
# [1] lower whisker
# [2] lower hinge
# [3] median
# [4] upper hinge 
# [5] extreme of the upper 
# Thus, by specifying index 3 in the stats vector, the median is returned

# 2c) What is the 50% CCI? 
# We can use the casbox stats again here
casbox$stats[2]
casbox$stats[4]
# This tells us the 50% CCI is [4,48]

# 3a) Change all the days of the week to capital letters
summary(bike$weekday)
# Remember to check the output before redefining your attribute values
toupper(bike$weekday)
# This is correct so we can redefine our variable
bike$weekday <- toupper(bike$weekday)
summary(bike$weekday) # now weekday is character type - recode as factor
bike$weekday <- as.factor(bike$weekday)

# 3b) Create boxplots that compare the total count of bike rentals for when the 
# day of the week starts with F and when it does not
indexDayF <- grep("^F", bike$weekday)
# Again use the ? to determine how to specify a particular range for a boxplot
boxplot(bike$cnt, subset = indexDayF)