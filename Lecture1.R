#######################
#
# Lecture 1: R Basics
#
#######################

# Data types and basic visualizations

# Data Types
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
# Change month to a factor
# Change season to a factor
# Change dteday to a date element
bike$dteday <- as.Date.character(bike$dteday, format = "%m/%d/%Y")
?as.Date
# Change holiday to a logic element
str(bike$holiday)
bike$holiday <- as.logical(bike$holiday)

# Basic visualizations
str(bike)
# histogram
hist(bike$cnt)
# box plot
boxplot(bike$cnt)
# bar
barplot(table(bike$weathersit))



