#############Week 4: Case studies##################
#############Medical decision making: What contribute(s) to mortality?############
#############Step 1: Understand the dataset###########
#1.1 Load the data
MD_medical <- read.csv("C:/Users/Student/Documents/UVA 2016-2017/RWorkshop/Week 4 - Case Study/MortData_medical.csv")
MD_personal <- read.csv("C:/Users/Student/Documents/UVA 2016-2017/RWorkshop/Week 4 - Case Study/MortData_personal.csv")

#Merge two datasets
colnames(MD_personal)[1] <- "Patient_id"
MD <- (merge(MD_medical,MD_personal,by="Patient_id"))
MD$death.x <- NULL #Delete the duplicated columns
MD$Patient_id <- NULL #Delete useless columns
View(MD)

#1.2 Read the description files and understand what is each attributes.
#Rename attributes if you find it helpful

#1.3 Check the class of each variables. 


#Do we need to recode anything?



#############Step 2: Data preprocessing###########
#2.1 General information about the datasets
#How many patients do we have?

#What is the percentage of mortality?


#2.2 Missing data treatment
#Check to see if we have any missing data


#2.2.1 Temperature: mean imputation


#Additional question: why can we use mean imputation for temperature? Consider the disbribution of temp attribute.

#Use histogram to make sure that the disbribution of the attribute did not change too drastically after imputation


#2.2.2 Avtisst: mean imputation


#2.2.3 pafi: Blood Gase
#Consider the real meaning of the attributes. Let's convert the attribute into a factor with two levels, ventilator patients and non-ventilator patients
#Hint: you can use cut function. Type ?Cut to see the usage of the function
# ventilator patients non-ventilator patients


#In medical world, bili and albi are measured in the same test. 
#2.2.4 bili (bilirubin): replace NAs in Bilirubin with median if Albumin measurement not NA

#Additional question: why using median instead of mean?

#2.2.5 albi (Albumin): replace NAs in Albumin with median if Bilirubin measurement not NA


#Create a new column to indicate if a bili/albi test has been taken (or not)
#In another word: if both bili and albi is NA, the has not been taken

#Don't forget to convert the attribute into a factor




#Additional question: why are we creating this new column?

#2.2.6 age: binning age into following categories
#0-20: very young
#20-40: young
#40-60:middleaged
#60-80: old
#80+: elderly


#Additional question: why do we want to bin the data?


#2.2.7 What about other missing data?
#We can use knnImputation




#But for some categorial data, such as race, it is very difficult to recover the missing information
which(MD$race=="")
#Since we don't have many missing entries, we can remove these observations



#2.2.8: A final check on missing values



#############Step 3: Data visualization###########
#3.1 Create scatter plot matrix on all numeric variables
#Hint: you will need to find out which variables are numeric first!

#3.2 Create boxplots of numeric variables against attribute death
#For example: create a boxplot of bili versus death. How does the value of bili influence the chance of mortality?


#3.3 Create tables of factor variables against attribute death
#Example: Within each age group, how many patient passed away/survived?
age_death <- table(MD$age,MD$death)
plot(age_death) #You can use visulization to help with your analysis
hist(age_death)
#Now examine other categorical (factors) attributes

#3.4 Write down three trends that you observed through visulization. 
#How does each attribute affect mortality? 


#############Step 4: A preview on modeling! More to come later in this workshop (and in your SYS4102 class!)###########
#Fancy stuff: neural network!
#We are not gonna go into the technical details. But this is how you can use the neural net in R
#4.1 First, you need to normalize numeric variables so that each variables lies within range [0,1]
#An explaination on normalization: http://stats.stackexchange.com/questions/70801/how-to-normalize-data-to-0-1-range
MD$death <- as.integer(MD$death) #For neural network, you need to convert death into an integer
#Get all the numeric varibales


#Create a function that normalizes an attribute into [0,1]



#Now let's normalize all the numeric variables (hint: you can do that in a loop!)



#Now do a sanity check to make sure the normalization is correct
summary(MD)

#4.2 Neural network: run the following lines of code to get a feeling of it 
install.packages("neuralnet")
library(neuralnet)
nn_data <- MD[,num_var]
colnames(nn_data)[ncol(nn_data)]<- "death"
f <- as.formula(paste("death ~", paste(colnames(nn_data)[1:(ncol(nn_data)-1)], collapse = " + ")))
nn <- neuralnet(f, data = nn_data,hidden=2,stepmax=1e6)
plot(nn)


