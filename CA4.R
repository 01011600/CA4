require(xlsx)
# read in excel document sheet 1
epa_data_GHG <-read.xlsx("GHG_Final data_1990-2017_website.xlsx", sheetName = "1")
str(epa_data_GHG)
# use the column National total from the data frame total_data
total_data <- epa_data_GHG$National.Total
# print the structure of the data
str(total_data)
# Convert the data to time series
time_series_total_GHG <- ts(total_data, start = c(1990, 1), frequency = 1)
# outlier removal using tsclean
#clean_GHG <-tsclean(time_series_total_GHG)

# Using functions to determine the start,end and frequency of the time series
start(time_series_total_GHG)
end(time_series_total_GHG)
frequency(time_series_total_GHG)

#Decompose the time series into various components.
#plot(decompose(new_GHG), xlab= Year) # did not work so commenting out

library(forecast)
# chanage the dispaly setting to have 2 graph side by side
default_settings <- par(no.readonly = TRUE)
par(mfrow= c(2,2))
y_boundry <- c(min(time_series_total_GHG), max(time_series_total_GHG))

# Plot the raw data and the smooth it to remove noise
# using moving average

plot(time_series_total_GHG, main = "Raw time series")
# ma() function is used to smooth the GHG time series
plot(ma(time_series_total_GHG, 2), 
     main = "Simple moving averages (k =2)",
     ylim = y_boundry)
plot(ma(time_series_total_GHG, 3), 
     main = "Simple moving averages (k =3)",
     ylim = y_boundry)
plot(ma(time_series_total_GHG, 4), 
     main = "Simple moving averages (k =4)",
     ylim = y_boundry)
par(default_settings)


library(tseries)
# Run Kwiatkowski–Phillips–Schmidt–Shin (KPSS) test for stationarity
kpss.test(time_series_total_GHG, null = "Level")
# Run Kwiatkowski–Phillips–Schmidt–Shin (KPSS) test for trend
kpss.test(time_series_total_GHG, null = "Trend")
# Use ndiffs to determie the amout of lag required
ndiffs(time_series_total_GHG, max.d = 20)
# lag the GHG times series data by 12
diff12_GHG <- diff(time_series_total_GHG, lag = 12)
# Run the Augmented Dickey-Fuller Test to test for stationarity
adf.test(diff12_GHG)
# lag the GHG times series data by 1,2,3 & 4
diff1_GHG <- diff(time_series_total_GHG, lag = 1)
diff2_GHG <- diff(time_series_total_GHG, lag = 2)
diff3_GHG <- diff(time_series_total_GHG, lag = 3)
diff4_GHG <- diff(time_series_total_GHG, lag = 4)
# Adjust the plot margins using mar
# to get all 4 graphs and legends on the same graph
par(mar=c(5,5,2,2))
# plot the lag data 1,2,4,& 4
plot(diff1_GHG)
plot(diff2_GHG)
plot(diff3_GHG)
plot(diff4_GHG)

# Run the Augmented Dickey-Fuller Test to test for stationarity
adf.test(diff2_GHG)
# Read in dataset with three manufactured data points at the start of the data
require(xlsx)
epa1_data_GHG <-read.xlsx("GHG_Final data_1990-2017_new.xlsx", sheetName = "1")
str(epa1_data_GHG)

total_data1 <- epa1_data_GHG$National.Total.New
str(total_data1)
# convert to time series
new_GHG <- ts(total_data1, start = c(1987, 1), frequency = 1)
# check how many lags the data needs
ndiffs(new_GHG, max.d = 20)
# Plot the new data
plot(new_GHG)
# diff the data by 1
diff_new_GHG <- diff(new_GHG, lag = 1)
# plot the differ data
plot(diff_new_GHG)
# Run the ndiffs to test for lag required
ndiffs(new_GHG)
# Run the ndiffs to test for lag required on the lagged data
ndiffs(diff_new_GHG)
# Plot the Acf & PACF charts to  measure of how the observations
# in a time series relate to each other
# Plot the acf chart lag 2
acf_result <- Acf(diff2_GHG)
# Plot the pacf(autocorrelation) chart lag 2
pacf_result <-Pacf(diff2_GHG)
# Plot the acf chart lag 12
acf_result2 <- Acf(diff12_GHG)
# Plot the pacf(autocorrelation) chart lag 12
pacf_result2 <-Pacf(diff12_GHG)

# Show both side-by-side comparison of diffed and non diffed data
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(time_series_total_GHG)
plot(diff2_GHG)
par(opar)
# check the stationarity of the diff2 data
adf.test(diff2_GHG)

# Fitting the ARIMA model
library(forecast)
# Run ARIMA model
arima_model <- Arima(time_series_total_GHG, order = c(1,2,1))
# Print the model
arima_model
# Print the accuracy of the model
accuracy(arima_model)

# Auto Arima of the GHG dataset
auto_arima_model <- auto.arima(time_series_total_GHG, trace=TRUE,stepwise=FALSE, approximation=FALSE)
# Print the model
auto_arima_model
# Print the accuracy of the model
accuracy(auto_arima_model)

# Evaluate the model fit
# qqnorm produces a normal QQ plot on the values of y
# qqline adds a theoritical line to the residuals
qqnorm(arima_model$residuals)
qqline(arima_model$residuals)
# Auto Arima model residuals
qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)


# Box test function provides a test that correlations are all zero
Box.test(arima_model$residuals, type ="Ljung-Box")
# Run the Ljung- Box test with auto arima
Box.test(auto_arima_model$residuals, type ="Ljung-Box")

# Forecast 3 years ahead for the GHG time series
forecast(arima_model, 3)
# Plot the forecast
plot(forecast(arima_model, 3),
     xlab ="Year",
     ylab ="Mt C02 eq")

# Forecast 3 years ahead for auto arima using the GHG time series
forecast(auto_arima_model, 3)
# plot the forecast
plot(forecast(auto_arima_model, 3),
     xlab ="Year",
     ylab ="Mt C02 eq")

# Remove last three data points from GHG data
require(xlsx)
epa2_data_GHG <-read.xlsx("GHG_Final data_1990-2014_website.xlsx", sheetName = "1")
str(epa2_data_GHG)

total_data2 <- epa2_data_GHG$National.Total
str(total_data2)
validate_GHG <- ts(total_data2, start = c(1990, 1), frequency = 1)
validate_GHG

library(forecast)
# running ARIMA model to validate model
arima_model_validate <- Arima(validate_GHG, order = c(1,2,1))
arima_model_validate
accuracy(arima_model_validate)

# Forecast 3 years of data that is already measured
forecast(arima_model_validate, 3)
plot(forecast(arima_model_validate, 3),
     xlab ="Year",
     ylab ="Mt C02 eq")

# Create an Auto Arima to validate the model using data that is already measured
auto_arima_model_validate <- auto.arima(validate_GHG, trace=TRUE)
auto_arima_model_validate
accuracy(auto_arima_model_validate)

# Forecast 3 years of data that is already measured using auto ARIMA
forecast(auto_arima_model_validate, 3)
# Plot the model used for validation
plot(forecast(auto_arima_model_validate, 3),
     xlab ="Year",
     ylab ="Mt C02 eq")
#################################