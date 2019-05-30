require(xlsx)
epa_data_GHG <-read.xlsx("GHG_Final data_1990-2017_website.xlsx", sheetName = "1")
str(epa_data_GHG)

total_data <- epa_data_GHG$National.Total
str(total_data)
time_series_total_GHG <- ts(total_data, start = c(1990, 1), frequency = 1)
#clean_GHG <-tsclean(time_series_total_GHG)

# Using functions to determine the start,end and frequency of the time series
start(time_series_total_GHG)
end(time_series_total_GHG)
frequency(time_series_total_GHG)

#Decompose the time series into various components.
#plot(decompose(new_GHG), xlab= Year) # did not work so commenting out

library(forecast)
# chnage the dispaly setting to have 2 graph side by side
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

require(xlsx)
epa1_data_GHG <-read.xlsx("GHG_Final data_1990-2017_new.xlsx", sheetName = "1")
str(epa1_data_GHG)

total_data1 <- epa1_data_GHG$National.Total.New
str(total_data1)
new_GHG <- ts(total_data1, start = c(1987, 1), frequency = 1)
ndiffs(new_GHG, max.d = 20)
plot(new_GHG)
diff_new_GHG <- diff(new_GHG, lag = 1)
plot(diff_new_GHG)
# Run the ndiffs to test for lag required
ndiffs(new_GHG)
# Run the ndiffs to test for lag required on the lagged data
ndiffs(diff_new_GHG)
# Plot the Acf chart - measure of how the observations
# in a time series relate to each other
acf_result <- Acf(diff2_GHG)
# Partial the pacf(autocorrelation) chart
pacf_result <-Pacf(diff2_GHG)
#pacf_result <-Pacf(T_Data_box_d)
acf_result2 <- Acf(diff12_GHG)
# Partial the pacf(autocorrelation) chart
pacf_result2 <-Pacf(diff12_GHG)


#transform the series using boxcox() function
#lambda <- BoxCox.lambda(time_series_total_GHG)
#plot.ts(BoxCox(time_series_total_GHG, lambda = lambda))
#T_Data_box <- BoxCox(time_series_total_GHG, lambda = lambda)
#T_Data_box_d <- diff(T_Data_box)
#adf.test(T_Data_box_d)

# Show both side-by-side comparison
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(time_series_total_GHG)
plot(diff_GHG)
par(opar)
plot(diff_GHG)
ndiffs(diff_GHG, max.d = 5)
# Show both side-by-side f comparison
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(time_series_total_GHG)
plot(diff_GHG)
par(opar)

ndiffs(diff_GHG)
kpss.test(diff_GHG)
kpss.test(diff_GHG, null = "Level")
kpss.test(diff_GHG, null = "Trend")

adf.test(diff_GHG)
adf.test(time_series_total_GHG)

# Fitting the ARIMA model
#******** NOTE THAT WE USE THE ORIGINAL DATASET FOR TEH ARIMA MODEL**********
# and we modify the d value to suit our original model
# and we modify the d value to suit our original findings
# and d= 1
library(forecast)
arima_model <- Arima(time_series_total_GHG, order = c(1,2,1))
arima_model
accuracy(arima_model)

# Auto Arima of the GHG dataset
auto_arima_model <- auto.arima(time_series_total_GHG, trace=TRUE)
auto_arima_model
accuracy(auto_arima_model)

# Accuracy measured through the mean absolute percentage error (MAPE)
# is a measurment of the prediction accuracy

# Evaluate the model fit
# qqnorm produces a normal QQ plot on the values of y
# qqline adds a theoritical qq plot
# which passes through the proobability quantiles
# by defaualtr the 1st and 3rd quantiles

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
plot(forecast(arima_model, 3),
     xlab ="Year",
     ylab ="Mt C02 eq")

# Forecast 3 years ahead for the GHG time series
forecast(auto_arima_model, 3)
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
arima_model_validate <- Arima(validate_GHG, order = c(1,2,1))
arima_model_validate
accuracy(arima_model_validate)

# Forecast 3 years ahead for the GHG time series
forecast(arima_model_validate, 3)
plot(forecast(arima_model_validate, 3),
     xlab ="Year",
     ylab ="Mt C02 eq")

# Auto Arima of the GHG dataset
auto_arima_model_validate <- auto.arima(validate_GHG, trace=TRUE)
auto_arima_model_validate
accuracy(auto_arima_model_validate)

# Forecast 3 years ahead for the GHG time series
forecast(auto_arima_model_validate, 3)
plot(forecast(auto_arima_model_validate, 3),
     xlab ="Year",
     ylab ="Mt C02 eq")
#################################
lin_model <- lm(National.Total~Year, data=epa_data_GHG)
lin_model
accuracy(lin_model)
qqnorm(lin_model$residuals)
qqline(lin_model$residuals)

#d <- epa_data(National.Total,Year)  ## need to use data in a data.frame for predict()
d <- epa_data[,c("National.Total","Year")]  ## need to use data in a data.frame for predict()

logEstimate <- lm(log(National.Total)~Year,data=d)
plot(d$Year,log(d$National.Total))
xvec <- seq(1990,2017,length=nrow(d))
xvec <-setNames(as.data.frame(xvec),c("Year"))

logpred <- predict(logEstimate,newdata=data.frame(x=xvec))

lines(xvec$Year,logpred)

#x.ar <- ar(time_series_total_GHG, method = "mle")
#x.ar$order
#x.ar$ar