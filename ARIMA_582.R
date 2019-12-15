library(tseries)
library(forecast)
library(pastecs)
library(fUnitRoots)
library(pracma)
library(MASS)

Annual<-read.csv("C:/rt_fiveminsysload_20190422.csv",skip = 5) #row.names=1
actual<-read.csv("C:/rt_fiveminsysload_20190423.csv",skip = 5) 
actual_val<-na.omit(actual[['MW']])
# Annual1<-read.csv("C:/582_DOM_data.csv")library(MASS)

History1<-na.omit(Annual['MW'])
hist_vect<-na.omit(Annual[['MW']])

# myts<-ts(hist_vect,start=c(2017,1),frequency=24) # Set freq is 365 24 yearsNumber 730 monthly
# plot(myts)
# stl1<-stl(myts,s.window="periodic")
# a<-stl1[["time.series"]]
# seas<-a[1:dim(a)[1],1]
# Scale<-linspace(1,dim(a)[1],dim(a)[1])
# plot(stl1)
# plot(Scale,seas,"l")
hist1<-as.numeric(unlist(History1))
var(hist_vect)
sd(hist_vect)
mean(hist_vect)

#Descriptive stclearatistics
summary(History1)
adf.test(hist_vect,alternative="stationary")
adfTest(hist_vect)
unitrootTest(hist_vect)

#plotting the data
plot(hist_vect)
acf(hist_vect,lag=length(hist_vect)-1)
pacf(hist_vect,lag=length(hist_vect)-1)
acf(hist_vect)
pacf(hist_vect)

# Seasonal differencing
diff1<-diff(hist_vect,lag = 1)
diff1_frame<-data.frame(diff1)
plot(diff1)

#stl decomposition
ts1<-ts(diff1,start=c(2017,1),frequency = 24)
stldiff<-stl(ts1,s.window="periodic")
plot(stldiff)

summary(diff1)
adf.test(diff1,alternative="stationary")
adfTest(diff1)
unitrootTest(diff1)

# qq norm plot
qqnorm(diff1)
qqline(diff1,col="steelblue",lwd=2)

# Normal Probability plot
fit<-fitdistr(diff1,"normal")
para<-fit$estimate
hist(diff1,prob="TRUE")
x<-seq(-5,5)
curve(dnorm(x,para[1],para[2]),col=2,add=TRUE)

# Determing the model order
scale<-linspace(1,length(diff1),length(diff1))
plot(scale,diff1,"l")
acf(diff1,lag=length(diff1)-1)
pacf(diff1,lag=length(diff1)-1)
acf(diff1)
pacf(diff1)

# First Order differencing
# diff2<-diff(diff1,lag=1)
# scale=linspace(1,length(diff2),length(diff2))
# plot(scale,diff2,"l")
# adf.test(diff2,alternative="stationary")
# adfTest(diff2)
# acf(diff2,lag=length(diff2)-1)
# pacf(diff2,lag=length(diff1)-1)
# unitrootTest(diff2)
# qqnorm(diff2)
# qqline(diff2,col="steelblue",lwd=2)

#ARIMA model fit
arima1<-arima(hist_vect,order=c(1,1,2))
checkresiduals(arima1)
f1<-forecast(arima1)
predicted<-predict(arima1,n.ahead=288)
p<-as.numeric(predicted[["pred"]])
plot(p,col='red',type='l')
plot(actual_val,col='green')

a1<-auto.arima(hist_vect)
summary(a1)
checkresiduals(a1)
