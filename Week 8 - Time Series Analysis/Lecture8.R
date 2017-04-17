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
# Periodograms 'examining frequency-domain models of an equi-spaced time series.'
pg.bike<-spec.pgram(bike.ts,spans=9,demean=T,log='no')

# Find the peak, max.omega.bike
max.omega.bike<-pg.bike$freq[which(pg.bike$spec==max(pg.bike$spec))]

# What is the period?
1/max.omega.bike # 23.97; there is a repeating cycle every ~24 days

# Conclusion: there is both seasonality and trend in this data set - need to address both of these in the model
# Create a seasonality model
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

# Combine the trend model with the seasonal model (because both trend and seasonilty were significant)        
bike.trendseason <-lm(bike.ts[time.bike]~time.bike+Day)
summary(bike.trendseason)

# Get the residuals for this model
# An assumption for linear modeling is that the residuals are not correlated
# However, often in time series data that is not true thus we need to check for correlation in the residuals
e.ts.bike <- ts(bike.trendseason$residuals)

# Plot autocorrelation (acf) and partial autocorrelation (pacf)
# ACF and PACF measure the correlation of residuals and give incite into which models to use (see diagram)
par(mfrow=c(1,2))
acf(e.ts.bike, main="ACF of Residuals from e.ts.bike")
pacf(e.ts.bike,main="PACF of Residuals from e.ts.bike")
par(mfrow=c(1,1))

# ACF is sidusodial decay
# PACF cannot see any trends
# Choose AR or ARMA

# Autoregressive: AR(p) models: "The AR model establishes that a realization at time t is a linear combination of the p previous realization plus some noise term."
# Moving Average: MA(q): "can define correlated noise structure in our data and goes beyond the traditional assumption where errors are iid."
# ARMA: combines autoregressive and moving average terms
# ARIMA: combines autoregressive and moving average terms when the data in non-stationary - i.e. mean, variance, autocorrelation, etc. are not constant overtime

# Choose p values for AR model
# ar(3) p=3
bike.ar3 <- arima(e.ts.bike, order=c(3,0,0))
summary(bike.ar3)

# Choose p and r values for ARMA model
bike.arma13 <- arima(e.ts.bike, order = c(1,0,3))
summary(bike.arma13)

# Use the auto.arima from 'forecast' library- Automatically find p, q, & d terms
library('forecast')
bike.auto <- auto.arima(e.ts.bike, trace=TRUE)
# Using autoregressive function the best model is ARIMA(2,1,1)

# Compare time series models using AIC and BIC
AIC(bike.trendseason)
AIC(bike.auto) # 206349
AIC(bike.ar3) # 204358.6
AIC(bike.arma13) # 204264.8

AIC(bike.trendseason, k = log(length(bike.ts))) # 228669.7
AIC(bike.auto, k = log(length(e.ts.bike))) # 206380
AIC(bike.ar3, k = log(length(e.ts.bike))) # 204397.4
AIC(bike.arma13, k = log(length(e.ts.bike))) # 204311.4

# Best model is the bike.arma13 [ARMA(1,3)]

###################
#
# Prediction using ts
#
###################

# Create a dataframe which is a 1 week period (recall we removed a week previously)
next.week.time<-c((length(bike.ts)-6):(length(bike.ts)))

# the test data frame
next.week<-data.frame(time.bike = next.week.time, count = bike.ts[next.week.time])

# Prediction for the next week by bike.arma13:
E_Y.pred<-predict(bike.trend,newdata=next.week)
e_t.pred<-forecast(bike.arma13,h=7)
next.week.prediction<-E_Y.pred+e_t.pred$mean

# MSE:
# can compute mse based on predicted error minus counts 
mean((next.week.prediction-next.week$count)^2) 
# MSE is 22592

####################
#
# Assumption checking
#
####################

# Autocorrelation has been accounted for - all p-values for L-B are below significance line
tsdiag(bike.arma13,gof.lag=20)

# Recheck ACF and PACF for residuals
par(mfrow=c(1,2))
acf(bike.arma13$residuals, main="ACF of Residuals from bike.arma13")
pacf(bike.arma13$residuals,main="PACF of Residuals from bike.arma13")
par(mfrow=c(1,1))
# 1. Looking for no pattern - i.e. not exponential or sinusidal decay 
# 2. Looking for insignficant lags - i.e. below the blue line 

###################
#
# Practice problems
# 
###################

# 1. Create a time series dataset bases on 'causual'

# 2. Create a model for the trend

# 3. Plot the ts data and the trend line

# 4. Determine if trend is significant

# 5. Create a periodgram for this data set

# 6. Determine if seasonality is significant

# 7. Create a model based on which terms are significant - seasonality, trend or both

# 8. Obtain the residuals for this model and plot the ACF and PACF

# 9. Based on the ACF and PACF determine which model should be used (AR, MA, ARMA or ARIMA) and create them

# 10. Create a model using the auto-arima function

# 11. Compare the models using AIC and BIC - conclude which is best

# 12. Predict the next weeks casual user count