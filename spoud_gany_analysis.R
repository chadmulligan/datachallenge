setwd("D:/jisnard/Documents/spoud/")
source("helpers.R")

# load("applis_gany_bucket.RData")

library(forecast)
library(tseries)
library(splines)
library(TSA)

load("applis_bucket.RData")


applis_mond_bucket <- applis_bucket[grepl("Mond", applis_bucket$tkNameIdProvider), ]

ts.throughput <- ts(applis_mond_bucket$throughput, freq=60)
plot(ts.throughput)

ts.throughput.train <- ts(ts.throughput[1:(0.6*720)], freq = 60)o
ts.throughput.test <- ts(ts.throughput[433:720], freq = 60, start = 8.2)



### bruteforcing the arima
grid <- expand.grid(ar = 0:25, diff = 0:1, ma = 0:1)
grid[1,]
grid$mape <- rep(NA, length(grid))
nrow(grid)


### we want to force the model to test all order combinations
stl.train <- function(grid = data.frame(), train = ts(), test = ts()){
  
  test.length <- length(test)
  
  for (i in 1:nrow(grid)){
    
    fit <- NULL
    forecast.fit <- NULL
    
    ###modelfunction to apply to the stl decomposition
    order.grid <- c(grid[i, 1], grid[i, 2], grid[i, 3])
    
    arima.mod <- function(x, ...) {return(arima(x, order= order.grid, ...))}
    
    tryCatch({
      fit <- stlm(y = train, modelfunction = arima.mod) 
      forecast.fit <- forecast(object=fit, h = test.length)
      grid$mape[i] <- accuracy(forecast.ts, test)["Test set", "MAPE"]
    },
    error=function(e) {cat("ERROR :",conditionMessage(e), "\n")},
    warning=function(w) {cat("WARNING :",conditionMessage(w), "\n")}
    )
  }

  grid
  
}

results <- stl.train(grid, ts.throughput.train, ts.throughput.test)

results %>%
  arrange(accuracy) %>%
  slice(1) %>%
  as.data.frame()


###refitting and ploting the forecasts of the selected model
arima.mod <- function(x , ...) {return(stats::arima(x, order= c(15, 0, 1), ...))}

fit.ts <- stlm(y = ts.throughput.train, modelfunction = arima.mod) 
forecast.ts <- forecast(object=fit.ts, h = length(ts.throughput.test))
autoplot(forecast.ts) + autolayer(ts.throughput.test, series = "Mond (test)")


####TBATS modelling
fit.bats <- tbats(ts.throughput.train)
fc.tbats <- forecast(fit.bats, h=287)
autoplot(fc.tbats)


####Fourier test#### WARNING
####differences to make the ts stationary
ndiffs(ts.throughput.train)
p <- periodogram(ts.throughput.train)


data.frame(period=1/p$freq, spec=p$spec) %>%
  arrange(desc(spec)) %>%
  head()

###fit arima with external fourier regressors ##### WARNING, too long to compute. 
bestfit <- list(aicc=c(), i=c(), j=c(), fit=c())

for(i in 1:3) {
  for (j in 1:3){
    z1 <- fourier(ts(ts.throughput.train, frequency=216), K=i)
    z2 <- fourier(ts(ts.throughput.train, frequency=144), K=j)
    fit <- auto.arima(ts.throughput.train, xreg=cbind(z1, z2))
    if(fit$aicc < bestfit$aicc) {
      bestfit <- list(aicc=fit$aicc, i=i, j=j, fit=fit)
    }
    print(paste0("j= ", j, ", done"))
  }
print(paste0("i= ", i, ", done"))
}

bestfit



### AVG RESPONSE TIME ######
ts.avgresp <- ts(applis_mond_bucket$avg_respTime, freq=60)
plot(ts.avgresp)

ts.avgresp.train <- ts(ts.avgresp[1:(.6*720)], freq = 60)
ts.avgresp.test <- ts(ts.avgresp[433:720], freq = 60, start = 8.2)
plot(ts.avgresp.train)

# ###un-tuned fit -NOPE
# fit0 <- auto.arima(ts.avgresp.train)
# fc0 <- forecast(fit0, h=288)
# accuracy(fc0, ts.avgresp.test)
# autoplot(fc0) + autolayer(ts.avgresp.test, series = "Mond (test)")
# 
# ###seasonalised fit -NOPE
# fit.avg.ts <- stlm(y = ts.avgresp.train, modelfunction = arima, s.window = 3)
# forecast.avg.ts <- forecast(fit.avg.ts, h = length(ts.avgresp.test))
# accuracy(forecast.avg.ts, ts.avgresp.test)
# autoplot(forecast.avg.ts) + autolayer(ts.avgresp.test, series = "Mond (test)")
# 
# ####TBATS modelling -NOPE
# fit.avg.bats <- tbats(ts.avgresp.train)
# fc.tbats <- forecast(fit.avg.bats, h=288)
# autoplot(fc.tbats)

#### the "other" method
maxes <- which(max(ts.avgresp.train) == ts.avgresp.train)
diff(maxes)

min.avg <- min(ts.avgresp.train)
slope <- (max(ts.avgresp.train) - min(ts.avgresp.train))/diff(maxes)[1]

min.avg + slope*90

forecast_avg_resptime <- function(ts.train=ts.avgresp.train, h=length(ts.avgresp.test), 
                                  period = diff(maxes)[1], 
                                  ts.slope=slope, ts.min.avg=min.avg) {
  
  #find where you are 
  position <- round((ts.train[length(ts.train)] - ts.min.avg) / ts.slope)
  
  periods <- c()
  
  #fill the 90-min 
  periods[1:(period - position)] <- c((position+1):period)
  
  fullperiods <- (h - (period - position)) %/% period
  remainder <- (h - (period - position)) %% period
  
  if(fullperiods != 0) {periods <- c(periods, rep(1:period, fullperiods))}
  
  if(remainder != 0) {periods <- c(periods, 1:remainder)}
  
  ##return the "forecast" 
  forecast_avg.ts <- ts(sapply(periods, function(x){x <- round(ts.min.avg + ts.slope * x)}), freq = 60, start = length(ts.train)/60+1)
  
  forecast_avg.ts
  
}


forecast_avg.ts <- forecast_avg_resptime()
autoplot(ts.avgresp.train) + autolayer(forecast_avg.ts)  + autolayer(ts.avgresp.test)
accuracy(forecast_avg.ts, ts.avgresp.test)["Test set", "MAPE"]

max(applis_mond_bucket$throughput)
max(applis_mond_bucket$avg_respTime)
max(applis_mond_bucket$worktime)


save(list = ls(), file ="analysisworkspace.RData")



### worktime
head(forecast.ts$lower)
head(forecast.ts$upper)

autoplot(forecast.ts)

ts.worktime <- ts(applis_mond_bucket$worktime, freq=60)
ts.worktime.train <- ts(ts.worktime[1:(0.6*720)], freq = 60)
ts.worktime.test <- ts(ts.worktime[433:720], freq = 60, start = 8.2)

forecast.worktime <- forecast.ts
forecast.worktime$upper <- forecast.worktime$upper * forecast_avg.ts
forecast.worktime$lower <- forecast.worktime$lower * forecast_avg.ts
forecast.worktime$x <- ts.worktime.train
forecast.worktime$mean <- forecast.worktime$mean* forecast_avg.ts
forecast.worktime$method <- "Throughput * AVG RespTime Forecast"
autoplot(forecast.worktime) + autolayer(ts.worktime.test)

accuracy(forecast.worktime, ts.worktime.test)

save(list=c("forecast.worktime", "ts.worktime", "ts.worktime.test", "ts.worktime.train"), file = "worktime_forecast.Rdata")
