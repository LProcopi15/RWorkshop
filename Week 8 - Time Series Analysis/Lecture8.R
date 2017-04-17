bike <- read.csv("C:/Users/Student/Documents/UVA 2016-2017/RWorkshop/Week 1- Arithmatic and Data Type Intro/bike.csv")

###################
#
# Time Series
#
###################

bike.ts <- ts(bike$cnt)

# In time series look for 3 things:
# 1. Trend - overall long acting upward or downward movement
# 2. Seasonality - repeating patterns within a year
# 3. Cycles - patterns that repeat in over a year period
plot(bike.ts)

# 1. Modeling for trend

# Create a new variable time.bike which is a matrix of (length(bike.ts))
time.bike<-c(1:(length(bike.ts)-7))

# Build a new model, bike.trend which predicts bike.ts based on the time variable, time.bike- use all data except the last week of your bike time series
bike.trend<-lm(bike.ts[time.bike]~time.bike)

# Use the summary() command on bike.trend
summary(bike.trend)

# Is time significant in predicting spam frequency? Yes; p-value <= 0.001
# Add trend line to time series plot
plot(bike.ts)
abline(bike.trend,col='red') # Overall upward trend

# 2. Modeling for seasonality

# Get the periodogram from bike.ts
pg.bike<-spec.pgram(bike.ts,spans=9,demean=T,log='no')

# Find the peak, max.omega.bike
max.omega.bike<-pg.bike$freq[which(pg.bike$spec==max(pg.bike$spec))]

# What is the period?
1/max.omega.bike # 23.97; there is a repeating cycle every ~24 days

# Conclusion: there is both seasonality and trend in this data set - need to address both of these in the model
# Get the residuals for trend
e.ts.bike <- ts(bike.trend$residuals)


# Plot autocorrelation (acf) and partial autocorrelation (pacf)
par(mfrow=c(1,2))
acf(e.ts.bike, main="ACF of Residuals from e.ts.bike")
pacf(e.ts.bike,main="PACF of Residuals from e.ts.bike")
par(mfrow=c(1,1))

# Use the ACF and PACF to choose a model type
# See table for details
# ACF is sidusodial and PACF cannot see any trends
# Choose AR or ARMA

# Choose r values for AR model
# ar(3) p=3
bike.ar3 <- arima(e.ts.bike, order=c(4,0,0))
summary(bike.ar3)
AIC(bike.ar3)

# Use the auto.arima from 'forecast' library- Automatically find p, q, & d terms
library('forecast')
bike.auto <- auto.arima(e.ts.bike, trace=TRUE)
# Using autoregressive function the best model is ARIMA(2,1,1)
AIC(bike.auto)

# Transform weekly info
Day <- rep(NA, length(bike.ts)-7)
Day[which((time.bike %% 7)  == 1)] <- "Sat"  
Day[which((time.bike %% 7)  == 2)] <- "Sun"  
Day[which((time.bike %% 7)  == 3)] <- "Mon"  
Day[which((time.bike %% 7)  == 4)] <- "Tue"  
Day[which((time.bike %% 7)  == 5)] <- "Wed"  
Day[which((time.bike %% 7)  == 6)] <- "Thr"  
Day[which((time.bike %% 7)  == 0)] <- "Fri"  

Day <- as.factor(Day)

# View default contrasts
contrasts(Day)

# Build a model bike.season to model the trend and seasonality of ham.
bike.season<- lm(bike.ts[time.bike]~Day)
summary(bike.season)

# Get the residuals from the bike.season model above and store in e.ts.bike2:
e.ts.bike2 <- ts(bike.season$residuals)

# Plot acf and pacf side by side for easier examination
par(mfrow=c(1,2))
acf(e.ts.bike2, main="ACF of Residuals from bike.season")
pacf(e.ts.bike2,main="PACF of Residuals from bike.season")
par(mfrow=c(1,1))

# Sinusodial decay on ACF and not on PACF - AR or ARMA
bike.arma13 <- arima(e.ts.bike2, order=c(1,0,3))

# Use the auto.arima from 'forecast' library- Automatically find p, q, & d terms
bike2.auto <- auto.arima(e.ts.bike2, trace=TRUE)
# Best model is ARIMA(2,1,1) 

###################
#
# Practice problems
# 
###################

# 1. Create a time series dataset base on 'causual'

# 2. Create a model for the trend

# 3. Plot the ts data and the trend line

# 4. Determine if trend is significant

# 5. Obtain the residuals from the trend model

# 6. Plot the ACF and the PACF for this model and determine if AR, ARMA, or ARIMa should be used

# 7. Use auto.arima to determine the optimal model; also create a few other models with varying values for p, q and r

# 8. Compare models using AIC, BIC and adjs-R2

# 9. Create a periodgram for this data

# 10. Determine if seasonality is significant

# 11. Obtain the residuals, plot the ACF and PACF and interpret them

# 12. Create appropriate models for seasonality

# 13. Compare models using AIC, BIC and adjst-R2