library(tseries)
library(forecast)
library(pastecs)
library(fUnitRoots)
library(pracma)

# Ani Katchova # History1<-read.csv("History_noise.csv",header=FALSE,col.names = 'history')
Annual<-read.csv("Annual_load_Profile_PJM.csv") #row.names=1
History1<-(Annual['mw'])

row.names(Annual)
#hist_vect<-c(Annual[['mw']]) # hist<-History[,1]
hist_vect=History1[21:69,] # 21:93 72 6 days 21:188 7 days 21:69 2 days
# Try STL decomposition length(vect/list) - List number of columns
# myts<-ts(hist_vect,start=c(2017,2),frequency=7)
Dim1<-dim(History1)
hist_2<-History1[1:Dim1[1],]

stldd$time.series[,"trend"]
myts<-ts(hist_2,start=c(2017,1),frequency=24) # Set freq is 365 24 yearsNumber 730 monthly
plot(myts)
stl1<-stl(myts,s.window="periodic")
a<-stl1[["time.series"]]
seas<-a[1:dim(a)[1],1]
Scale<-linspace(1,8761,1)
plot(stl1)
plot(Scale,seas)

var(hist_vect)
sd(hist_vect)
mean(hist_vect)

diff1<-diff(hist_vect,differences = 1)
plot(diff1)

#Descriptive stclearatistics
summary(History1)
adf.test(hist_vect,alternative="stationary")
adfTest(hist_vect)
unitrootTest(hist_vect)

#plotting the data
plot(hist_vect)
acf(hist_vect,lag=length(hist_vect)-1)
pacf(hist_vect,lag=length(hist_vect)-1)

summary(diff1)
plot(diff1)
adf.test(diff1,alternative="stationary")
adfTest(diff1)
unitrootTest(diff1)

acf(diff1,lag=length(diff1)-1)
pacf(diff1,lag=length(diff1)-1)

# arima1<-arima(hist_vect,order=c(1,1,2))
# checkresiduals(arima1)
# f1<-forecast(arima1)
# predicted<-predict(arima1,n.ahead=168)
# plot(predicted)
# a1<-auto.arima(hist_vect)
