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
# 7. Factors
View(airquality)
summary(airquality)
str(airquality)



