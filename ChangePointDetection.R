#DOWNLOADING HISTORICAL DATA ON CRUDE OIL CLOSING PRICES

library(yahoofinancer)

crude.oil <- Ticker$new('CL=F')
crudeoil<- crude.oil$get_history(start = '2021-01-26', 
end = '2026-01-26', interval = '1d')

crudeoil$date<- substr(as.POSIXct(strftime(crudeoil$date, 
format="%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d", tz=""), 1, 10)

crudeoil.data<- as.data.frame(list(crudeoil$date, round(crudeoil$close,2)),
col.names=c("Date", "Close"), make.names=FALSE)

crudeoil.data<- na.omit(crudeoil.data)

crudeoil.data$Date<- as.Date(crudeoil.data$Date, format="%Y-%m-%d")

#RUNNING CHANGE-POINT DETECTION ANALYSIS
library(changepoint)
ansmean<- cpt.mean(crudeoil.data$Close, penalty="AIC", method="BinSeg", Q=3)
plot(ansmean,lwd=2, cpt.col="red", cpt.width=3, ylab="Daily Closing Price", 
main="Change Point Detection for Change in Mean")
print(ansmean)

ansvar<- cpt.var(crudeoil.data$Close, penalty="AIC", method="BinSeg", Q=3)
plot(ansvar, lwd=2, cpt.col="green", cpt.width=3, ylab="Daily Closing Price", 
main="Change Point Detection for Change in Variance")
print(ansvar)

ansmeanvar<- cpt.meanvar(crudeoil.data$Close, penalty="AIC", method="BinSeg", Q=3)
plot(ansmeanvar, lwd=2, cpt.col="blue", cpt.width=3, ylab="Daily Closing Price", 
main="Change Point Detection for Change in Mean and Variance")
print(ansmeanvar)
