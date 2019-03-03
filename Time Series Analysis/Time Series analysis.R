library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(zoo)
#require(zoo)
library(forecast)
library(tseries)
require(graphics)

#setwd("~/Desktop/timeseries/")
global_superstore <- read.csv("Global Superstore.csv")

# View(global_superstore)
# check the structuere of global_superstore dataframe
str(global_superstore)

# ---------------Data cleansing----------------------------------
# Taking total row count
nrow(global_superstore)
# Output: 51290
#check for NA values
sum(is.na(global_superstore$Order.Date))
# Output: 0 hence no NA values

# Step:1 Converting string to date
# Count distinct date before transformation
length(unique(global_superstore$Order.Date))
# Output 1430

global_superstore$Order.Date <-
  as.Date(global_superstore$Order.Date, '%d-%m-%Y')
# Checking Count distinc date after transformation
length(unique(global_superstore$Order.Date))
# Output 1403 , hece there is no droppage


# Count distinc date before transformation
length(unique(global_superstore$Ship.Date))
# Output 1464

global_superstore$Ship.Date <-
  as.Date(global_superstore$Ship.Date, '%d-%m-%Y')
# Checking Count distinc date after transformation
length(unique(global_superstore$Ship.Date))
# Output 1464 , hece there is no droppage

# Now leats fetch the required fields from the data frame and discard others
global_superstore_clean <-
  dplyr::select(
    global_superstore,
    Order.Date,
    Ship.Date,
    Segment,
    Market,
    Sales,
    Quantity,
    Discount,
    Profit,
    Shipping.Cost
  )

# lets find out max and min date
max(global_superstore_clean$Order.Date)
# output 2014-12-31
min(global_superstore_clean$Order.Date)
# output 2011-01-01

# Lets convert order data into mm-yyyy format and store it in a new column ord_month_year
global_superstore_clean$ord_month_year <-
  format(global_superstore_clean$Order.Date, "%m-%Y")

# lets find out max and min date
max(global_superstore_clean$ord_month_year)
# output 12-2014
min(global_superstore_clean$ord_month_year)
# output 01-2011

# Lets aggregate Sales, Quantity & Profit over ord_month_year,Market and Segment
global_superstore_grp <-
  group_by(global_superstore_clean, ord_month_year, Market, Segment)

global_superstore_segmented <-
  summarise(
    global_superstore_grp,
    monthly_Sales = sum(Sales),
    monthly_Profit = sum(Profit),
    monthly_Quantity = sum(Quantity)
  )

# View(global_superstore_grp)

# lets calculate coeficient of variation (COV) for profit all the segments
global_superstore_seg_grp <-
  group_by(global_superstore_segmented, Market, Segment)

global_superstore_seg_COV <-
  summarise(global_superstore_seg_grp,
            cov = sd(monthly_Profit) / mean(monthly_Profit))

global_superstore_seg_COV <-
  global_superstore_seg_COV[order(global_superstore_seg_COV$cov),]
# View(global_superstore_seg_COV)

#-----------------------Top 2--------------#
global_superstore_seg_COV[c(1, 2),]
#1. Market - EU, Segment - Consumer, COV - 0.624
#2. Market - APAC, Segment - Consumer, COV - 0.632

##Market - EU , Segment - Consumer
global_superstore_EU_Consumer <-
  filter(global_superstore_clean, Market == 'EU' &
           Segment == 'Consumer')
#View(global_superstore_EU_Consumer)
#nrow(global_superstore_EU_Consumer)
##Market - APAC , Segment - Consumer
global_superstore_APAC_Consumer <-
  filter(global_superstore_clean, Market == 'APAC' &
           Segment == 'Consumer')
#View(global_superstore_APAC_Consumer)
#nrow(global_superstore_APAC_Consumer)


global_superstore_EU_Consumer <-
  global_superstore_EU_Consumer[order(global_superstore_EU_Consumer$ord_month_year),]
global_superstore_EU_Consumer_grp <-
  group_by(global_superstore_EU_Consumer, ord_month_year)
global_superstore_EU_Consumer_Segmntd <-
  summarise(
    global_superstore_EU_Consumer_grp,
    monthly_sales = sum(Sales),
    monthly_quantity = sum(Quantity)
  )

global_superstore_EU_Consumer_Segmntd$ord_month_year <-
  as.yearmon(global_superstore_EU_Consumer_Segmntd$ord_month_year,
             '%m-%Y')

global_superstore_EU_Consumer_Segmntd <-
  global_superstore_EU_Consumer_Segmntd[order(global_superstore_EU_Consumer_Segmntd$ord_month_year,
                                              decreasing = FALSE),]
global_superstore_EU_Consumer_Segmntd$Month <-
  1:nrow(global_superstore_EU_Consumer_Segmntd)
#View(global_superstore_EU_Consumer_Segmntd)


timeser_global_superstore_EU_Consumer_Sales <-
  ts(global_superstore_EU_Consumer_Segmntd$monthly_sales)

plot(timeser_global_superstore_EU_Consumer_Sales)

#smoothing with holt winter's method
cols <- c("red", "blue", "green", "black")
alphas <- c(0.02, 0.4, 0.6)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1, length(alphas))) {
  smoothedseries <-
    HoltWinters(
      timeser_global_superstore_EU_Consumer_Sales,
      alpha = alphas[i],
      beta = FALSE,
      gamma = FALSE
    )
  
  #lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}


#Smoothing the series - Moving Average Smoothing

w <- 1
smoothedseries <-
  stats::filter(
    timeser_global_superstore_EU_Consumer_Sales,
    filter = rep(1 / (2 * w + 1), (2 * w + 1)),
    method = 'convolution',
    sides = 2
  )

#Smoothing left end of the time series

diff <- smoothedseries[w + 2] - smoothedseries[w + 1]
for (i in seq(w, 1, -1)) {
  smoothedseries[i] <- smoothedseries[i + 1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_global_superstore_EU_Consumer_Sales)
diff <- smoothedseries[n - w] - smoothedseries[n - w - 1]
for (i in seq(n - w + 1, n)) {
  smoothedseries[i] <- smoothedseries[i - 1] + diff
}

#Plot the smoothed time series

lines(smoothedseries, col = "blue", lwd = 2)


global_superstore_EU_Consumer_Segmntd$monthly_sales_ns <-
  global_superstore_EU_Consumer_Segmntd$monthly_sales
global_superstore_EU_Consumer_Segmntd$monthly_sales <-
  smoothedseries
plot(ts(global_superstore_EU_Consumer_Segmntd$monthly_sales_ns),
     col = "black")
lines(global_superstore_EU_Consumer_Segmntd$monthly_sales, col = "red")

View(global_superstore_EU_Consumer_Segmntd)

global_superstore_EU_Consumer_Segmntd_test <-
  global_superstore_EU_Consumer_Segmntd[43:48, ]
global_superstore_EU_Consumer_Segmntd_train <-
  global_superstore_EU_Consumer_Segmntd[1:42,]




#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

#lmfit <- lm(monthly_sales ~ sin(0.5*Month) * poly(Month,6) + cos(0.5*Month) * poly(Month,6)
#            + Month, data=global_superstore_EU_Consumer_Segmntd_train)
lmfit <-
  lm(monthly_sales ~ Month, data = global_superstore_EU_Consumer_Segmntd_train)
global_pred <-
  predict(lmfit, Month = global_superstore_EU_Consumer_Segmntd_train$Month)
summary(global_pred)
#global_pred <- as.data.frame(global_pred)
gpa_dfa <- data.frame(predictions = global_pred)
nrow(gpa_dfa)
#View(timevals_in)

plot(ts(global_superstore_EU_Consumer_Segmntd_train$monthly_sales))
lines(global_superstore_EU_Consumer_Segmntd_train$Month,
      gpa_dfa$predictions)
#View(data.frame(predictions=global_pred))


#Now, let's look at the locally predictable series
#We will model it as an ARMA series
global_superstore_EU_Consumer_Segmntd_train$monthly_sales_No_trend <-
  global_superstore_EU_Consumer_Segmntd_train$monthly_sales -
  global_pred
# View(global_superstore_EU_Consumer_Segmntd_train)

# Lets plot the resudual series
gbl_supstore_EU_Cons_Sales_NO_trend <-
  ts(global_superstore_EU_Consumer_Segmntd_train$monthly_sales_No_trend)
plot(gbl_supstore_EU_Cons_Sales_NO_trend)

# Lets check acf and pacf
acf(gbl_supstore_EU_Cons_Sales_NO_trend)
pacf(gbl_supstore_EU_Cons_Sales_NO_trend)
acf(gbl_supstore_EU_Cons_Sales_NO_trend, type = "partial")
adf.test(gbl_supstore_EU_Cons_Sales_NO_trend, alternative = "stationary")
kpss.test(gbl_supstore_EU_Cons_Sales_NO_trend)



#?auto.arima
armafit <-
  auto.arima(ts(
    global_superstore_EU_Consumer_Segmntd_train$monthly_sales_No_trend
  ))

tsdiag(armafit)
armafit
global_superstore_EU_Consumer_Segmntd_train$predictedsales <-
  fitted(armafit) + global_pred
#We'll check if the residual series is white noise

resi <-
  ts(global_superstore_EU_Consumer_Segmntd_train$monthly_sales_No_trend) - fitted(armafit)
plot(ts(resi))

acf(ts(resi))
acf(ts(resi), type = "partial")

adf.test(resi, alternative = "stationary")
kpss.test(resi)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(armafit, n.ahead = 6)

predict_df <- data.frame(Month = c(43:48))

fcast_lm_gbl_Trend <- predict(lmfit, data.frame(Month = c(43:48)))


actual_pred <-
  fcast_auto_arima$pred + fcast_lm_gbl_Trend

MAPE_auto_arima <-
  accuracy(actual_pred,
           global_superstore_EU_Consumer_Segmntd_test$monthly_sales)[5]
MAPE_auto_arima


View(
  data.frame(
    global_superstore_EU_Consumer_Segmntd_test$monthly_sales,
    actual_pred
  )
)
auto_arima_pred <- c(fitted(armafit), ts(fcast_auto_arima$pred))
plot(ts(global_superstore_EU_Consumer_Segmntd_train$monthly_sales),
     col = "black")
lines(global_superstore_EU_Consumer_Segmntd_train$predictedsales,
      col = "red")
lines(global_superstore_EU_Consumer_Segmntd_train$monthly_sales_ns,
      col = "blue")
#####Forecast For next six months###################################################################
fcast_auto_arima_nxt_sixmonths <- predict(armafit, n.ahead = 12)

fcast_lm_gbl_Trend_nxt_sixmonths <- predict(lmfit, data.frame(Month = c(43:54)))


fcast_nxt_sixmonths <-
  fcast_auto_arima_nxt_sixmonths$pred + fcast_lm_gbl_Trend_nxt_sixmonths
#Next Six Months Values - Sales EU Consumer 
#1. 41818.91
#2. 42233.52
#3. 42664.43
#4. 43117.19
#5. 43586.15
#6. 44062.84 

#----------------------------EU Quantity-----------------------------------------------------------

timeser_global_superstore_EU_Consumer_Quantity <-
  ts(global_superstore_EU_Consumer_Segmntd$monthly_quantity)

plot(timeser_global_superstore_EU_Consumer_Quantity)

w <- 1
smoothedseries_eu_quantity <-
  stats::filter(
    timeser_global_superstore_EU_Consumer_Quantity,
    filter = rep(1 / (2 * w + 1), (2 * w + 1)),
    method = 'convolution',
    sides = 2
  )

#Smoothing left end of the time series

diff <- smoothedseries_eu_quantity[w + 2] - smoothedseries_eu_quantity[w + 1]
for (i in seq(w, 1, -1)) {
  smoothedseries_eu_quantity[i] <- smoothedseries_eu_quantity[i + 1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_global_superstore_EU_Consumer_Quantity)
diff <- smoothedseries_eu_quantity[n - w] - smoothedseries_eu_quantity[n - w - 1]
for (i in seq(n - w + 1, n)) {
  smoothedseries_eu_quantity[i] <- smoothedseries_eu_quantity[i - 1] + diff
}

#Plot the smoothed time series

lines(smoothedseries_eu_quantity, col = "blue", lwd = 2)

#Store the existing monthly quantity in a new variable monthly_quantity_ns
global_superstore_EU_Consumer_Segmntd$monthly_quantity_ns <-
  global_superstore_EU_Consumer_Segmntd$monthly_quantity

#populate monthly_quantity with smoothned values
global_superstore_EU_Consumer_Segmntd$monthly_quantity <-
  smoothedseries_eu_quantity

#plot non smooth original quantity values
plot(ts(global_superstore_EU_Consumer_Segmntd$monthly_quantity_ns),
     col = "black")

#plot smooth original quantity values
lines(global_superstore_EU_Consumer_Segmntd$monthly_quantity, col = "red")

# View(global_superstore_EU_Consumer_Segmntd)

global_superstore_EU_Consumer_Segmntd_test <-
  global_superstore_EU_Consumer_Segmntd[43:48, ]
global_superstore_EU_Consumer_Segmntd_train <-
  global_superstore_EU_Consumer_Segmntd[1:42,]




#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_eu_quant <- lm(monthly_quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                     + Month, data=global_superstore_EU_Consumer_Segmntd_train)
View(global_superstore_EU_Consumer_Segmntd_train)
#lmfit_eu_quant <-
#  lm(monthly_sales ~ Month, data = global_superstore_EU_Consumer_Segmntd_train)
global_pred_quant <-
  predict(lmfit_eu_quant, Month = global_superstore_EU_Consumer_Segmntd_train$Month)
summary(global_pred_quant)
#global_pred <- as.data.frame(global_pred)
gpa_dfa <- data.frame(predictions = global_pred_quant)
nrow(gpa_dfa)
#View(timevals_in)

plot(ts(global_superstore_EU_Consumer_Segmntd_train$monthly_quantity))
lines(global_superstore_EU_Consumer_Segmntd_train$Month,
      gpa_dfa$predictions)
#View(data.frame(predictions=global_pred))


#Now, let's look at the locally predictable series
#We will model it as an ARMA series
global_superstore_EU_Consumer_Segmntd_train$monthly_quantity_No_trend <-
  global_superstore_EU_Consumer_Segmntd_train$monthly_quantity -
  global_pred_quant
# View(global_superstore_EU_Consumer_Segmntd_train)

# Lets plot the resudual series
gbl_supstore_EU_Cons_Quantity_NO_trend <-
  ts(global_superstore_EU_Consumer_Segmntd_train$monthly_quantity_No_trend)
plot(gbl_supstore_EU_Cons_Quantity_NO_trend)

# Lets check acf and pacf
acf(gbl_supstore_EU_Cons_Quantity_NO_trend)
acf(gbl_supstore_EU_Cons_Quantity_NO_trend, type = "partial")
adf.test(ts(gbl_supstore_EU_Cons_Quantity_NO_trend), alternative = "stationary")
kpss.test(ts(gbl_supstore_EU_Cons_Quantity_NO_trend))



#?auto.arima
armafit_eu_quant <-
  auto.arima(ts(
    global_superstore_EU_Consumer_Segmntd_train$monthly_quantity_No_trend
  ))

tsdiag(armafit_eu_quant)
armafit_eu_quant
global_superstore_EU_Consumer_Segmntd_train$predictedquantity <-
  fitted(armafit_eu_quant) + global_pred_quant
#We'll check if the residual series is white noise

resi <-
  ts(global_superstore_EU_Consumer_Segmntd_train$monthly_quantity_No_trend) - fitted(armafit_eu_quant)
plot(ts(resi))

acf(ts(resi))
acf(ts(resi), type = "partial")

adf.test(resi, alternative = "stationary")
kpss.test(resi)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

#global_pred_quant <-
#  predict(lmfit_eu_quant, Month = global_superstore_EU_Consumer_Segmntd_train$Month)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_eu_quantity <- predict(armafit_eu_quant, n.ahead = 6)



predict_df<-data.frame(Month = c(43:48))
fcast_lm_gbl_Trend_eu_quantity <- stats::predict(lmfit_eu_quant,newdata =data.frame(Month=predict_df$Month))


actual_pred_eu_quantity <-
  fcast_auto_arima_eu_quantity$pred + fcast_lm_gbl_Trend_eu_quantity

MAPE_auto_arima_eu_quantity <-
  accuracy(fcast_auto_arima_eu_quantity$pred,
           global_superstore_EU_Consumer_Segmntd_test$monthly_quantity)[5]
MAPE_auto_arima_eu_quantity

View(
  data.frame(
    global_superstore_EU_Consumer_Segmntd_test$monthly_quantity,
    actual_pred_eu_quantity
  )
)
auto_arima_pred_eu_quantity <- c(fitted(armafit_eu_quant), ts(fcast_auto_arima$pred))
plot(ts(global_superstore_EU_Consumer_Segmntd_train$monthly_quantity),
     col = "black")
lines(global_superstore_EU_Consumer_Segmntd_train$predictedquantity,
      col = "red")
lines(global_superstore_EU_Consumer_Segmntd_train$monthly_quantity_ns,
      col = "blue")

#####Forecast For next six months###################################################################
fcast_auto_arima_quant_nxt_sixmonths <- predict(armafit_eu_quant, n.ahead = 12)

fcast_lm_gbl_Trend_quant_nxt_sixmonths <- predict(lmfit_eu_quant, data.frame(Month = c(43:54)))


fcast_quant_nxt_sixmonths <-
  fcast_auto_arima_quant_nxt_sixmonths$pred + fcast_lm_gbl_Trend_quant_nxt_sixmonths
#Next Six Months Values 
#1. 500.7522 
#2. 526.5148 
#3. 574.8082 
#4. 636.8295 
#5. 697.3406 
#6. 739.1034

#----------------------------END EU-----------------------------------------------------------

#----------------------------Start APAC-----------------------------------------------------------

global_superstore_APAC_Consumer <-
  global_superstore_APAC_Consumer[order(global_superstore_APAC_Consumer$ord_month_year),]
global_superstore_APAC_Consumer_grp <-
  group_by(global_superstore_APAC_Consumer, ord_month_year)
global_superstore_APAC_Consumer_Segmntd <-
  summarise(
    global_superstore_APAC_Consumer_grp,
    monthly_sales = sum(Sales),
    monthly_quantity = sum(Quantity)
  )

global_superstore_APAC_Consumer_Segmntd$ord_month_year <-
  as.yearmon(global_superstore_APAC_Consumer_Segmntd$ord_month_year,
             '%m-%Y')

global_superstore_APAC_Consumer_Segmntd <-
  global_superstore_APAC_Consumer_Segmntd[order(global_superstore_APAC_Consumer_Segmntd$ord_month_year,
                                                decreasing = FALSE),]
global_superstore_APAC_Consumer_Segmntd$Month <-
  1:nrow(global_superstore_APAC_Consumer_Segmntd)
#View(global_superstore_APAC_Consumer_Segmntd)

#----------------------------APAC Quantity-----------------------------------------------------------

timeser_global_superstore_APAC_Consumer_Quantity <-
  ts(global_superstore_APAC_Consumer_Segmntd$monthly_quantity)

plot(timeser_global_superstore_APAC_Consumer_Quantity)

w <- 1
smoothedseries_APAC_quantity <-
  stats::filter(
    timeser_global_superstore_APAC_Consumer_Quantity,
    filter = rep(1 / (2 * w + 1), (2 * w + 1)),
    method = 'convolution',
    sides = 2
  )

#Smoothing left end of the time series

diff <- smoothedseries_APAC_quantity[w + 2] - smoothedseries_APAC_quantity[w + 1]
for (i in seq(w, 1, -1)) {
  smoothedseries_APAC_quantity[i] <- smoothedseries_APAC_quantity[i + 1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_global_superstore_APAC_Consumer_Quantity)
diff <- smoothedseries_APAC_quantity[n - w] - smoothedseries_APAC_quantity[n - w - 1]
for (i in seq(n - w + 1, n)) {
  smoothedseries_APAC_quantity[i] <- smoothedseries_APAC_quantity[i - 1] + diff
}

#Plot the smoothed time series

lines(smoothedseries_eu_quantity, col = "blue", lwd = 2)

#Store the existing monthly quantity in a new variable monthly_quantity_ns
global_superstore_APAC_Consumer_Segmntd$monthly_quantity_ns <-
  global_superstore_APAC_Consumer_Segmntd$monthly_quantity

#populate monthly_quantity with smoothned values
global_superstore_APAC_Consumer_Segmntd$monthly_quantity <-
  smoothedseries_APAC_quantity

#plot non smooth original quantity values
plot(ts(global_superstore_APAC_Consumer_Segmntd$monthly_quantity_ns),
     col = "black")

#plot smooth original quantity values
lines(global_superstore_APAC_Consumer_Segmntd$monthly_quantity, col = "red")

# View(global_superstore_EU_Consumer_Segmntd)

global_superstore_APAC_Consumer_Segmntd_test <-
  global_superstore_APAC_Consumer_Segmntd[43:48, ]
global_superstore_APAC_Consumer_Segmntd_train <-
  global_superstore_APAC_Consumer_Segmntd[1:42,]

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_apac_quant <- lm(monthly_quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                     + Month, data=global_superstore_APAC_Consumer_Segmntd_train)
View(global_superstore_APAC_Consumer_Segmntd_train)
#lmfit_eu_quant <-
#  lm(monthly_sales ~ Month, data = global_superstore_EU_Consumer_Segmntd_train)
global_pred_quant <-
  predict(lmfit_apac_quant, Month = global_superstore_APAC_Consumer_Segmntd_train$Month)
summary(global_pred_quant)
#global_pred <- as.data.frame(global_pred)
gpa_dfa <- data.frame(predictions = global_pred_quant)
nrow(gpa_dfa)
#View(timevals_in)

plot(ts(global_superstore_APAC_Consumer_Segmntd_train$monthly_quantity))
lines(global_superstore_APAC_Consumer_Segmntd_train$Month,
      gpa_dfa$predictions)
#View(data.frame(predictions=global_pred))


#Now, let's look at the locally predictable series
#We will model it as an ARMA series
global_superstore_APAC_Consumer_Segmntd_train$monthly_quantity_No_trend <-
  global_superstore_APAC_Consumer_Segmntd_train$monthly_quantity -
  global_pred_quant
# View(global_superstore_EU_Consumer_Segmntd_train)

# Lets plot the resudual series
gbl_supstore_APAC_Cons_Quantity_NO_trend <-
  ts(global_superstore_APAC_Consumer_Segmntd_train$monthly_quantity_No_trend)
plot(gbl_supstore_APAC_Cons_Quantity_NO_trend)

# Lets check acf and pacf
acf(gbl_supstore_APAC_Cons_Quantity_NO_trend)
acf(gbl_supstore_APAC_Cons_Quantity_NO_trend, type = "partial")
adf.test(ts(gbl_supstore_APAC_Cons_Quantity_NO_trend), alternative = "stationary")
kpss.test(ts(gbl_supstore_APAC_Cons_Quantity_NO_trend))



#?auto.arima
armafit_apac_quant <-
  auto.arima(ts(
    global_superstore_APAC_Consumer_Segmntd_train$monthly_quantity_No_trend
  ))

tsdiag(armafit_apac_quant)
armafit_apac_quant
global_superstore_APAC_Consumer_Segmntd_train$predictedquantity <-
  fitted(armafit_apac_quant) + global_pred_quant
#We'll check if the residual series is white noise

resi <-
  ts(global_superstore_APAC_Consumer_Segmntd_train$monthly_quantity_No_trend) - fitted(armafit_apac_quant)
plot(ts(resi))

acf(ts(resi))
acf(ts(resi), type = "partial")

adf.test(resi, alternative = "stationary")
kpss.test(resi)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months


#Also, let's evaluate the model using MAPE
fcast_auto_arima_apac_quantity <- predict(armafit_apac_quant, n.ahead = 6)



predict_df<-data.frame(Month = c(43:48))
fcast_lm_gbl_Trend_apac_quantity <- stats::predict(lmfit_apac_quant,newdata =data.frame(Month=predict_df$Month))


actual_pred_apac_quantity <-
  fcast_auto_arima_apac_quantity$pred + fcast_lm_gbl_Trend_apac_quantity

MAPE_auto_arima_apac_quantity <-
  accuracy(fcast_auto_arima_apac_quantity$pred,
           global_superstore_APAC_Consumer_Segmntd_test$monthly_quantity)[5]
MAPE_auto_arima_apac_quantity

View(
  data.frame(
    global_superstore_APAC_Consumer_Segmntd_test$monthly_quantity,
    actual_pred_apac_quantity
  )
)
auto_arima_pred_eu_quantity <- c(fitted(armafit_apac_quant), ts(fcast_auto_arima$pred))
plot(ts(global_superstore_APAC_Consumer_Segmntd_train$monthly_quantity),
     col = "black")
lines(global_superstore_APAC_Consumer_Segmntd_train$predictedquantity,
      col = "red")
lines(global_superstore_APAC_Consumer_Segmntd_train$monthly_quantity_ns,
      col = "blue")

#####Forecast For next six months###################################################################
fcast_auto_arima_apac_quant_nxt_sixmonths <- predict(armafit_apac_quant, n.ahead = 12)

fcast_lm_gbl_Trend_apac_quant_nxt_sixmonths <- predict(lmfit_apac_quant, data.frame(Month = c(43:54)))


fcast_apac_quant_nxt_sixmonths <-
  fcast_auto_arima_apac_quant_nxt_sixmonths$pred + fcast_lm_gbl_Trend_apac_quant_nxt_sixmonths

#Next Six Months Values 
#1. 864.3199 
#2. 748.6772     
#3. 607.0337
#4. 472.7701
#5. 388.4563 
#6. 396.6228 

#----------------------------APAC Sales-----------------------------------------------------------

timeser_global_superstore_APAC_Consumer_Sales <-
  ts(global_superstore_APAC_Consumer_Segmntd$monthly_sales)

plot(timeser_global_superstore_APAC_Consumer_Sales)

w <- 1
smoothedseries_apac_sales <-
  stats::filter(
    timeser_global_superstore_APAC_Consumer_Sales,
    filter = rep(1 / (2 * w + 1), (2 * w + 1)),
    method = 'convolution',
    sides = 2
  )

#Smoothing left end of the time series

diff <- smoothedseries_apac_sales[w + 2] - smoothedseries_apac_sales[w + 1]
for (i in seq(w, 1, -1)) {
  smoothedseries_apac_sales[i] <- smoothedseries_apac_sales[i + 1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_global_superstore_APAC_Consumer_Sales)
diff <- smoothedseries_apac_sales[n - w] - smoothedseries_apac_sales[n - w - 1]
for (i in seq(n - w + 1, n)) {
  smoothedseries_apac_sales[i] <- smoothedseries_apac_sales[i - 1] + diff
}

#Plot the smoothed time series

lines(smoothedseries_apac_sales, col = "blue", lwd = 2)

#Store the existing monthly quantity in a new variable monthly_quantity_ns
global_superstore_APAC_Consumer_Segmntd$monthly_sales_ns <-
  global_superstore_APAC_Consumer_Segmntd$monthly_sales

#populate monthly_quantity with smoothned values
global_superstore_APAC_Consumer_Segmntd$monthly_sales <-
  smoothedseries_apac_sales

#plot non smooth original quantity values
plot(ts(global_superstore_APAC_Consumer_Segmntd$monthly_sales_ns),
     col = "black")

#plot smooth original quantity values
lines(global_superstore_APAC_Consumer_Segmntd$monthly_sales, col = "red")

# View(global_superstore_APAC_Consumer_Segmntd)

global_superstore_APAC_Consumer_Segmntd_test <-
  global_superstore_APAC_Consumer_Segmntd[43:48, ]
global_superstore_APAC_Consumer_Segmntd_train <-
  global_superstore_APAC_Consumer_Segmntd[1:42,]




#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_apac_sales <- lm(monthly_sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                     + Month, data=global_superstore_APAC_Consumer_Segmntd_train)
View(global_superstore_APAC_Consumer_Segmntd_train)
#lmfit_apac_sales <-
#  lm(monthly_sales ~ Month, data = global_superstore_APAC_Consumer_Segmntd_train)
global_pred_sales <-
  predict(lmfit_apac_sales, Month = global_superstore_APAC_Consumer_Segmntd_train$Month)
summary(global_pred_sales)
#global_pred <- as.data.frame(global_pred)
gpa_dfa <- data.frame(predictions = global_pred_sales)
nrow(gpa_dfa)
#View(timevals_in)

plot(ts(global_superstore_APAC_Consumer_Segmntd_train$monthly_sales))
lines(global_superstore_APAC_Consumer_Segmntd_train$Month,
      gpa_dfa$predictions)
#View(data.frame(predictions=global_pred))


#Now, let's look at the locally predictable series
#We will model it as an ARMA series
global_superstore_APAC_Consumer_Segmntd_train$monthly_sales_No_trend <-
  global_superstore_APAC_Consumer_Segmntd_train$monthly_sales -
  global_pred_sales
# View(global_superstore_EU_Consumer_Segmntd_train)

# Lets plot the resudual series
gbl_supstore_APAC_Cons_Sales_NO_trend <-
  ts(global_superstore_APAC_Consumer_Segmntd_train$monthly_sales_No_trend)
plot(gbl_supstore_APAC_Cons_Sales_NO_trend)

# Lets check acf and pacf
acf(gbl_supstore_APAC_Cons_Sales_NO_trend)
acf(gbl_supstore_APAC_Cons_Sales_NO_trend, type = "partial")
adf.test(ts(gbl_supstore_APAC_Cons_Sales_NO_trend), alternative = "stationary")
kpss.test(ts(gbl_supstore_APAC_Cons_Sales_NO_trend))



#?auto.arima
armafit_apac_sales <-
  auto.arima(ts(
    global_superstore_APAC_Consumer_Segmntd_train$monthly_sales_No_trend
  ))

tsdiag(armafit_apac_sales)
armafit_apac_sales
global_superstore_APAC_Consumer_Segmntd_train$predictedsales <-
  fitted(armafit_apac_sales) + global_pred_sales
#We'll check if the residual series is white noise

resi <-
  ts(global_superstore_APAC_Consumer_Segmntd_train$monthly_sales_No_trend) - fitted(armafit_apac_sales)
plot(ts(resi))

acf(ts(resi))
acf(ts(resi), type = "partial")

adf.test(resi, alternative = "stationary")
kpss.test(resi)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

#Also, let's evaluate the model using MAPE
fcast_auto_arima_apac_sales <- predict(armafit_apac_sales, n.ahead = 6)



predict_df<-data.frame(Month = c(43:48))
fcast_lm_gbl_Trend_apac_sales <- stats::predict(lmfit_apac_sales,newdata =data.frame(Month=predict_df$Month))


actual_pred_apac_sales <-
  fcast_auto_arima_apac_sales$pred + fcast_lm_gbl_Trend_apac_sales

MAPE_auto_arima_apac_sales <-
  accuracy(fcast_auto_arima_apac_sales$pred,
           global_superstore_APAC_Consumer_Segmntd_test$monthly_sales)[5]
MAPE_auto_arima_eu_quantity

View(
  data.frame(
    global_superstore_APAC_Consumer_Segmntd_test$monthly_sales,
    actual_pred_apac_sales
  )
)
auto_arima_pred_eu_quantity <- c(fitted(armafit_apac_sales), ts(fcast_auto_arima_apac_sales$pred))
plot(ts(global_superstore_APAC_Consumer_Segmntd_train$monthly_sales),
     col = "black")
lines(global_superstore_APAC_Consumer_Segmntd_train$predictedsales,
      col = "red")
lines(global_superstore_APAC_Consumer_Segmntd_train$monthly_sales_ns,
      col = "blue")

#####Forecast For next six months###################################################################
fcast_auto_arima_sales_nxt_sixmonths <- predict(armafit_apac_sales, n.ahead = 12)

fcast_lm_gbl_Trend_sales_nxt_sixmonths <- predict(lmfit_apac_sales, data.frame(Month = c(43:54)))


fcast_sales_nxt_sixmonths <-
  fcast_auto_arima_sales_nxt_sixmonths$pred + fcast_lm_gbl_Trend_sales_nxt_sixmonths
#Next Six Months Values 
#1. 48248.48 
#2. 55131.85  
#3. 59623.64  
#4. 58345.69 
#5. 49105.84  
#6. 32050.28 





