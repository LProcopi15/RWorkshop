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
summary(adult)

# Remove useless columns - we have education and education number; we only need one
# of these since they reflect the same information
adult$education.number <- NULL
View(adult)

# Datasets use different values to indicate that there is no data for that point
# It appears this dataset uses question marks to indicate missing information
# R uses 'NA' as a key word to indicate missing information - lets replaces the ?
adult[adult == "?"] <- NA # This isn't working?
which(is.na(adult))

#########################
#
# Review problems
#
#########################