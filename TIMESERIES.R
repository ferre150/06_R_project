#Noah Ferrel
rm(list=ls())

library('TSA')
library('forecast')
library('tseries')
library('tidyquant')
library('plyr')


#Problem 1.)

#a.)

for(x in stock_names){
  getSymbols(x,src ="yahoo", from = "2015-03-18", to = "2020-05-15", return.class='ts')
}

stock_list = list(FB, AAPL, INTC, IBM, NVDA, SIRI, GOOG, AMZN, WFC, GE, EBAY, S, VZ, DIS,
                  NKE, NFLX, WMT, TSLA, TGT)
 
names(stock_list)= stock_names

find_arima <- function(temp) {
  temp_list = list()
  for(x in 1:13){
    fit = auto.arima(temp[(1+100*(x-1)):(100*x),4])
    temp_val = fit$arma
    temp_list = rbind(temp_list,temp_val)
    rownames(temp_list)[x] = paste0('Model: ',x)
  }
  return(temp_list)
}

all_arima = lapply(stock_list, function(x) find_arima(x))
test = do.call("rbind", all_arima)
test = as.data.frame(test)
colnames(test) = c('p','q','P','Q','Period','d','D')

arimaorder(auto.arima(FB[1:100,4]))

find_arima_order <- function(temp) {
  temp_list = list()
  for(x in 1:13){
    fit = auto.arima(temp[(1+100*(x-1)):(100*x),4])
    temp_val = arimaorder(fit)
    temp_list = rbind(temp_list,temp_val)
  }
  return(temp_list)
}

test4 = find_arima(FB)
test3 = find_arima_order(FB)

all_models = lapply(stock_list, function(x) find_arima_order(x))
ARIMA_info = do.call("rbind", all_models)

ARIMA_info = as.data.frame(ARIMA_info)
rownames(ARIMA_info) <- c()

ARIMA_info$ARIMA = paste0('ARIMA(',ARIMA_info$p,ARIMA_info$d,ARIMA_info$q,')')

Frequency_table = count(ARIMA_info$ARIMA)
names(Frequency_table) = c('ARIMA(pdq)','Freq')

#Frequency Table for selected Arimas

# ARIMA(pdq) Freq
# 1  ARIMA(010)  181
# 2  ARIMA(011)    8
# 3  ARIMA(012)    2
# 4  ARIMA(013)    2
# 5  ARIMA(021)    6
# 6  ARIMA(100)   13
# 7  ARIMA(101)    1
# 8  ARIMA(102)    2
# 9  ARIMA(103)    1
# 10 ARIMA(110)   15
# 11 ARIMA(111)    5
# 12 ARIMA(113)    1
# 13 ARIMA(121)    1
# 14 ARIMA(210)    1
# 15 ARIMA(211)    1
# 16 ARIMA(212)    4
# 17 ARIMA(311)    2
# 18 ARIMA(413)    1

#b.)

last_500 = lapply(stock_list, function(x) x[800:1300,4])

forecast_df = function(temp_xts){
  list_Match = c()
  temp = ts(unlist(temp_xts))
  for (x in 0:(length(temp)-101)){
    temp2 = temp[(0+x):(100+x)] # grabs the 100 data points to train on
    HW_model = HoltWinters(temp2,beta = FALSE,gamma = FALSE)
    ARIMA_model = auto.arima(temp2)
    future_HW = forecast(HW_model,h=1) # forecasts
    future_ARIMA = forecast(ARIMA_model,h=1)
    diff_pred_hw = as.numeric(future_HW$mean) - as.numeric(temp[(100+x)]) # gives the direction of the predicted
    diff_pred_ARIMA = as.numeric(future_ARIMA$mean) - as.numeric(temp[(100+x)]) # gives the direction of the predicted
    diff_act_hw = as.numeric(temp[(101+x)]) - as.numeric(temp[(100+x)]) # gives the direction of the actual
    diff_act_ARIMA = as.numeric(temp[(101+x)]) - as.numeric(temp[(100+x)]) # gives the direction of the actual
    
    if(diff_pred_hw>0 && diff_act_hw>0){# Both are Positive
      up_down_hw = 1
    }else if(diff_pred_hw<0 && diff_act_hw<0){# Both are Negative
      up_down_hw = 1
    }else{ # they dont match
      up_down_hw = 0
    }
    
    if(diff_pred_ARIMA>0 && diff_act_ARIMA>0){# Both are Positive
      up_down_ARIMA = 1
    }else if(diff_pred_ARIMA<0 && diff_act_ARIMA<0){# Both are Negative
      up_down_ARIMA = 1
    }else{ # they dont match
      up_down_ARIMA = 0
    }
    
    p_val_hw = Box.test(resid(HW_model))$p.value
    p_val_arima = Box.test(resid(ARIMA_model))$p.value
    
    if(is.na(p_val_hw)){
      print(paste0('Error',x))
      return(temp2)
    }
    
    if(p_val_hw > 0.05){# GOOD
      goodness_hw = 1
    }else { #BAD
      goodness_hw = 0
    }
    
    if(p_val_arima > 0.05){# GOOD
      goodness_ARIMA = 1
    }else { #BAD
      goodness_ARIMA = 0
    }

    info = c(up_down_hw,up_down_ARIMA,goodness_hw,goodness_ARIMA)
    
    list_Match = rbind(list_Match,info)
  }
  return(list_Match)
}

HW_ARIMA = lapply(last_500, function(x) forecast_df(x))
HW_ARIMA_df = do.call("rbind", HW_ARIMA)
HW_ARIMA_df = as.data.frame(HW_ARIMA_df)
colnames(HW_ARIMA_df) = c('HW_M','ARIMA_M','HW_fit','ARIMA_fit')
HW_ARIMA_df$HWGOOD = (HW_ARIMA_df$HW_M + HW_ARIMA_df$HW_fit) == 2
HW_ARIMA_df$ARIMAGOOD = (HW_ARIMA_df$ARIMA_M + HW_ARIMA_df$ARIMA_fit) == 2

Freq_HW = count(HW_ARIMA_df$HWGOOD)
Freq_ARIMA = count(HW_ARIMA_df$ARIMAGOOD)

Freq_table = rbind(Freq_HW,Freq_ARIMA)

colnames(Freq_table) = c('Goodness of Fit','Freq')
row.names(Freq_table) = c('HW1','HW2','ARIMA1','ARIMA2')

Freq_table$Percent = paste0(Freq_table$Freq/length(HW_ARIMA_df$HWGOOD)*100,'%')


#Frequency Table for Goodness of Fit

#False = p-value lower than 0.05 and/or picked wrong direction
#True = p-value greater than 0.05 and picked write direction

#       Goodness of Fit Freq Percent
#HW1              FALSE 3852 50.7%
#HW2               TRUE 3748 49.3%
#ARIMA1           FALSE 6450 84.9%
#ARIMA2            TRUE 1150 15.1%

#Problem 5

airpass = 'http://homepage.divms.uiowa.edu/~kchan/TSA/Datasets/airpass.dat'

ap_data = as.ts(read.table(airpass, header = T))
head(ap_data)


#a.)
plot(ap_data) # The time series seems to have a drift and as an increase in variance
plot(log(ap_data)) # taking the log gets rid of the exploding variance, so this is the appropriate transformation

#b.)
plot(diff(log(ap_data))) # differencing took away the drift

#c.)
plot(diff(diff(log(ap_data)), lag = 12))

#d.)

acf(as.vector(diff(diff(log(ap_data)), lag = 12)),ci.type = 'ma') 
# this gives us our MA component which seems to be 1, but it is a little suspicious

#e.)

#fit = arima(ap_data, order = c(0,1,1), seasonal = list(order = c(0,1,1),period=12))
#fit = arima(diff(diff(log(ap_data))), order = c(0,1,1), seasonal = list(order = c(0,1,1),period=12))
fit = arima(log(ap_data), order = c(0,1,1), seasonal = list(order = c(0,1,1),period=12))

#f.)
plot(window(rstandard(fit)), ylab='Standardized Residuals',type='o') # the residuals look good there isn't any periodic coponents
abline(h=0)

acf(as.vector(window(rstandard(fit))), lag.max=36) # the only statistically significant correlation is at lag 6

hist(window(rstandard(fit)),xlab='Standardized Residuals') # the risiduals look normally distributed which is good

qqnorm(window(rstandard(fit))) # ther are a few ouliers, but it suggest normality in the residuals
qqline(window(rstandard(fit)))

shapiro.test(window(rstandard(fit)))

# Shapiro-Wilk normality test
# 
# data:  window(rstandard(fit))
# W = 0.98045, p-value = 0.1441

#Normality is not rejected with a P-value of 0.1441

#g.)

plot(fit,n.ahead=24,xlab='Year',type='o', ylab='Airline Passengers')
