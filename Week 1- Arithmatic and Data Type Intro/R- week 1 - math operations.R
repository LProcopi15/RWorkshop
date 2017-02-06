#Basic operations in R: arithmatic operations, mathematical functions, vectorized arithmetic, statistic, sorting and ordering

##########################Part 1: R as a basic calcualtor#############################
#Addition, substraction, multiplication, division
1+1
1-1
2*2
2/4
(1+2)/9
0/0
#You can create variables to store the result of your calculation
result <- 1+4/2
result = 1+4/2
result
result2 <- result*7
result2
result2 <- 0 #Override the value of a variable
result2 #Now, new value has been assigned to this variable
#Other operations
18%/%4 #The integer part of the division
18%%4 #The remainder (modulo operator)
2^4 #exponentiation
factorial(5) #factorial !
choose(4,2) #combination

##########################Part 2: Mathematical functions#############################
abs(-1) #absolute value
log(10) #by default, natural log ln()
log10(10)
log2(10)
sqrt(4)
floor(4.1)
ceiling(4.1)
round(log(10),digits = 3)
#trigonometry
x <- pi
cos(x)
sin(x)
tan(x)

acos(x)
asin(x)
atan(x)
atan2(y, x)

##########################Part 3: Vectorized arithmetic#############################
#######creating vector##########
1:10
num <- 10:1
seq(1,10,2) #seq(a,b,c) Creating a sequence from a to b with step c
class(num)
is.vector(num)
#######arithmtic#############
#Let's create two vector
a <- seq(-9,18,3)
length(a)
b <- rep(1,length(a)) #Creating a vector b that has the same length of a
#Now we can do arithmetic just as we did in the previous section
a+b
a-b
a*b 
a/b
a*2
log(b)

##########################Part 4: Statistics (more to come in later workshop)#############################
sum()
mean()
var()
sd()
max()
min()
range()
median()
#Example - calculating sum of square
X = 12:4
freq=c(3,2,0,3,3,5,3,0,1)
all.in.one = rep(X, freq)
all.in.one
#Basic statistical plot
hist(all.in.one)
boxplot(all.in.one)
#Basic statistical of the sample
mean(all.in.one) #8.3
var(all.in.one) # 5.273684
sd(all.in.one) # 2.29645
var(all.in.one) == sd(all.in.one)^2 #Note the accuracy of calculation in R
round(var(all.in.one),digits = 10) == round(sd(all.in.one)^2,digits = 10)

#What do the following lines of code do? Guess and then run the command to see the result
sum(X * freq) / sum(freq)           
sum((all.in.one-mean(all.in.one))^2) -> SS
SS / (length(all.in.one)-1)
sqrt(SS / (length(all.in.one)-1))

#Another example: stock prices
#Assume we live in a world where only the oracle knows what's gonna happen tomorrow in the market...
####Simulating the stock prices for 50 days (more to come on simulation in later workshops!)######
price_now = 50
rnorm(50)      # random numbers from a normal distribution; N(0,1)
price_future <- price_now*rnorm((50))
barplot(price_future)
#More to come on prediction and regression analysis in later workshops!

##########################Part 5: Sorting and ordering#############################
#A practical example: height of students in a class
height.inches = c(68,65,70,71,69)
height.cm = height.inches * 2.54
height.cm
sort(height.inches)
?sort
sort(height.inches, decreasing = TRUE)
order(height.inches) #This function return indices!
rank(height.inches) 
#Get top three highest students in the class
sort(height.inches)[1:3]
top3_idx <- order(height.inches)[1:3]
height.inches[top3_idx]


##########################Note: logical values#############################
# > means greater than
# >= means greater than or equal to
# < means less than
# <= means less than or equal to
# == means equal to (two equal signs--one equal sign is assignment)
# != means not equal to (exclamation mark means "not")
height.inches[height.inches>70] #Passing in the logical value as the selection criteria


##########################Bonus part: matrix and linear algebra#############################
#Let's create two matrices
mat8 <- matrix(1:6, 2)
mat8
#[,1] [,2] [,3] 
#[1,]    1    3    5
#[2,]    2    4    6

mat9 <- matrix(c(rep(1, 3), rep(2, 3)), 2, byrow = T)
mat9
#[,1] [,2] [,3] 
#[1,]    1    1    1
#[2,]    2    2    2


#addition
mat9 + mat8


mat9 + 3


#subtraction
mat8 - mat9


#inverse
solve(mat8[, 2:3])


#transpose
t(mat9)

#multiplication
#we transpose mat8 since the dimension of the matrices have to match
#dim(2, 3) times dim(3, 2)
mat8 %*% t(mat9)


#element-wise multiplication
mat8 * mat9

mat8 * 4


#division
mat8/mat9


mat9/2


#using submatrices from the same matrix in computations
mat8[, 1:2]


mat8[, 2:3]


#Inner product
x <- 1:4
z <- x %*% x  # scalar ("inner") product (1 x 1 matrix)
drop(z)             # as scalar
norm(x%*%x) #Norm 

y <- diag(x)
z <- matrix(1:12, ncol = 3, nrow = 4)
y %*% z
y %*% x
x %*% z

#Solve a linear system
#Sample problem: solve for x, y and z
# x + 2y + 3z = 20  
# 2x + 5y + 9z = 100  
# 5x + 7y + 8z = 200
A <- matrix(data=c(1, 2, 3, 2, 5, 9, 5, 7, 8), nrow=3, ncol=3, byrow=TRUE)    
b <- matrix(data=c(20, 100, 200), nrow=3, ncol=1, byrow=FALSE)
round(solve(A, b), 3)
solve(A,b)

##########################Challenge: Exercise Time!#############################

#1) Create a vector of the values of (e^x)*cos(x) at x = 3, 3.1, 3.2, . . . , 6.
exp()
x<-seq(3,6,0.1)
vec <- exp(x)*cos(x)

#2)Execute the following lines which create two vectors of random integers which are chosen with replacement
#from the integers 0, 1, . . . , 999. Both vectors have length 250.
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)

#2.a) Create the vector (y2 − x1, . . . , yn − xn−1).
yVec_start2 <- yVec[2:250]
xVec_end249 <- xVec[1:249]
yVec_start2-xVec_end249

#2.b) Create the vector (sin(y1)/cos(x2),sin(y2)/cos(x3), . . . ,sin(yn−1)/cos(xn)).


#2.c) Create the vector (x1 + 2x2 − x3, x2 + 2x3 − x4, . . . , xn−2 + 2xn−1 − xn).
x1 <- xVec[1:248]
x2 <- xVec[2:249]
x3 <- xVec[3:250]
x1+2*x3-x3


#3) 
#3.a) Pick out the values in yVec which are > 600.
yVec[yVec>600]
#3.b) What are the index positions in yVec of the values which are > 600?
yPos <- which(yVec > 600)
yPos

#3.c) What are the values in xVec which correspond to the values in yVec which are > 600? (By correspond,we mean at the same index positions.)
xVec[yPos]

#3.d) Create the vector (|x1 − x¯|^ 1/2  , |x2 − x¯|^ 1/2, . . . , |xn − x¯|^1/2) where x¯ denotes the mean of the vector
#and x = (x1, x2, . . . , xn).
x_bar = mean(xVec)
(abs(xVec-x_bar))^(1/2)
#Alternatively
sqrt(abs(xVec-x_bar))

#3.e) How many values in yVec are within 200 of the maximum value of the terms in yVec?
y_max = max(yVec)
y_within200ofmax <- which(yVec >= (y_max-200))
length(y_within200ofmax) #This give you the number of values

#3.f) How many numbers in xVec are divisible by 2? (Note that the modulo operator is denoted %%.)
xVec%%2 #%% gives you the remainder of the division operations, if the remainder == 0, the number is divisible by 2
which(xVec%%2==0)
length(which(xVec%%2==0)) 

#3.g) Sort the numbers in the vector xVec in the order of increasing values in yVec.


#3.h) Pick out the elements in yVec at index positions 1, 4, 7, 10, 13, . . . .
idx <- seq(1,250,3)
yVec[idx]

#For more challenges: http://www.dcc.fc.up.pt/~ines/aulas/1516/DM1/rexercises-1-R-basic.pdf



