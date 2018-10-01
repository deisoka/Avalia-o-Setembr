
install.packages("urca")                                                        
library("urca")                                                                
library(readxl)

colnames(Taxa_Di)[2] <- "Taxa_Di"  
Taxa_Di <- ts(Taxa_Di, start = 1994-07-04)
Taxa_Di <- Taxa_Di[,-1]


TesteDF_Taxa_Di <- ur.df(Taxa_Di, "none",lags = 0)           
summary(TesteDF_Taxa_Di_none)                                       
TesteDF_Taxa_Di_drift <- ur.df(Taxa_Di, "drift", lags=0)           
summary(TesteDF_Taxa_Di_drift)                                     
TesteDF_Taxa_Di_trend <- ur.df(Taxa_Di, "trend", lags = 0)        
summary(TesteDF_Taxa_Di_trend)   



acf(Taxa_Di, main="Função autocorrelação")
pacf(Taxa_Di, main="Função Autocorrelação Parcial")


AR1 <- arima(Taxa_Di,order = c(1,0,0))


AR2 <- arima(Taxa_Di,order = c(2,0,0))
AR2

MA1 <- arima(Taxa_Di, order = c(0,0,1))
MA2 <- arima(Taxa_Di, order = c(0,0,2))
MA3 <- arima(Taxa_Di, order = c(0,0,3))
MA4 <- arima(Taxa_Di, order = c(0,0,4))
MA5 <- arima(Taxa_Di, order = c(0,0,5))
MA6 <- arima(Taxa_Di, order = c(0,0,6))
MA7 <- arima(Taxa_Di, order = c(0,0,7))
MA8 <- arima(Taxa_Di, order = c(0,0,8))
MA9 <- arima(Taxa_Di, order = c(0,0,9))

ARMA11 <- arima(Taxa_Di, order = c(1,0,1))
ARMA12 <- arima(Taxa_Di, order = c(1,0,2))
ARMA13 <- arima(Taxa_Di, order = c(1,0,3))
ARMA14 <- arima(Taxa_Di, order = c(1,0,4))
ARMA15 <- arima(Taxa_Di, order = c(1,0,5))
ARMA16 <- arima(Taxa_Di, order = c(1,0,6))
ARMA17 <- arima(Taxa_Di, order = c(1,0,7))
ARMA18 <- arima(Taxa_Di, order = c(1,0,8))
ARMA19 <- arima(Taxa_Di, order = c(1,0,9))

ARMA21 <- arima(Taxa_Di, order = c(2,0,1))
ARMA22 <- arima(Taxa_Di, order = c(2,0,2))
ARMA23 <- arima(Taxa_Di, order = c(2,0,3))
ARMA24 <- arima(Taxa_Di, order = c(2,0,4))
ARMA25 <- arima(Taxa_Di, order = c(2,0,5))
ARMA26 <- arima(Taxa_Di, order = c(2,0,6))
ARMA27 <- arima(Taxa_Di, order = c(2,0,7))
ARMA28 <- arima(Taxa_Di, order = c(2,0,8))
ARMA29 <- arima(Taxa_Di, order = c(2,0,9))

estimacoes <- list(AR1, AR2, MA1, MA2, MA3, MA4, MA5, MA6, MA7, MA8, MA9, 
                   ARMA11,ARMA12, ARMA13, ARMA14,ARMA15, ARMA16,ARMA17,ARMA18,ARMA19,
                   ARMA21,ARMA22,ARMA23,ARMA24,ARMA25,ARMA26,ARMA27,ARMA28,ARMA29)      
sapply(estimacoes, AIC)

AIC <- sapply(estimacoes, AIC) 
BIC <- sapply(estimacoes, BIC)
Modelo <- c("AR1", "AR2", "MA1", "MA2", "MA3", "MA4", "MA5", "MA6", "MA7", "MA8", "MA9", "ARMA11","ARMA12", "ARMA13", "ARMA14","ARMA15", "ARMA16","ARMA17","ARMA18","ARMA19","ARMA21","ARMA22","ARMA23","ARMA24","ARMA25","ARMA26","ARMA27","ARMA28","ARMA29")
sapply(estimacoes, BIC)


     


