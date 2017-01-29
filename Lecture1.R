#######################
#
# Lecture 1: R Basics
#
#######################

### Data Types

# 1. int - integer whole numbers 
# put an L after the number to ensure to is read as an integer
type <- 2L
print(class(type))
# 2. num - numbers that can have decimal places
# 3. character - any character, treated as non-numeric
# 4. Logical - True or False 
# 5. Vector
type <- c('a', 'b', 'c')
print(type)
print(class(type))
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
print(class(bike$weekday))
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

# Character matching
grepl("worst", bike$weathersit)
grep("worst", bike$weathersit)
grep("^Y", bike$weathersit) # Does not return the proper thing? 


#############################
#
# Practice Problems
#
#############################
# ***IMPORTANT NOTE: to change more data types into a different one the general 
# command is as.datatype (ex. as.factor, as.Date). Also, if you are confused about
# the meaning or the input of a command you can use the following format:
?as.Date

# 1a) Change month to a factor with the levels Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sept, Oct, Nov, Dec
# 1b) Change season to a factor with the levels Win, Spr, Sum, Fall
# 1c) Change dteday to a date element
  # Note: be sure to check that the dates were processed correctly

# 2a) Create a boxplot of the count of casual users
# 2b) What is the median?
# 2c) What is the 50% CCI? 

# 3a) Change all the days of the week to capital letters
# 3b) Create boxplots that compare the total count of bike rentals for when the 
# day of the week starts with F and when it does not
