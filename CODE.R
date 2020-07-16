install.packages("psych")
#install.packages(“matrixTests”)
install.packages("tseries")
install.packages("fDMA")
install.packages("urca")
install.packages("rmgarch")
install.packages("xts")
install.packages("MTS")
install.packages("riskR")
library(psych)
library(matrixTests)
library(tseries)
library(fDMA)
library(urca)
library(rmgarch)
library(xts)
library(MTS)
library(riskR)
library(readxl)
continuous_data_1 <- read_excel("~/Downloads/continuous data_1.xlsx")

AR_GARCH_DCC_WITH_OR_WITHOUT_SYMMETRY <- function(rX1,varmodel)
{
  ug_spec <- ugarchspec(mean.model=list(armaOrder=c(1,0)))
  print(ug_spec)
  #univariate garch model
  ugfit = ugarchfit(spec = ug_spec, solver='hybrid',data = continuous_data_1$Daily_returns_Stocks)
  #print(ugfit)
  #multivariate garch model
  uspec.n = multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)),variance.model = list(model = varmodel))))
  multf = multifit(uspec.n,rX1)
  #to specify the correlation specification we use the dccspec function
  spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
  fit1 = dccfit(spec1,data = rX1,fit.control = list(eval.se = TRUE), fit = multf)
  print(fit1)
  cov1 = rcov(fit1)
  cor1 = rcor(fit1)
  #plot the correlation between OIL and NTG, OIL and Silver ,OIL and N50
  par(mfrow=c(3,1))
  
  cor_OG <- cor1[1,2,]
  cor_OG <- as.xts(cor_OG)
  cor_OG <- ts(cor_OG,start=c(2006,191),frequency=365)
  plot(cor_OG)
  cor_OSi <- cor1[1,3,]
  cor_OSi <- as.xts(cor_OSi)
  cor_OSi <- ts(cor_OSi,start=c(2006,191),frequency=365)
  plot(cor_OSi)
  cor_OS <- cor1[1,4,]
  cor_OS <- as.xts(cor_OS)
  cor_OS <- ts(cor_OS,start=c(2006,191),frequency=365)
  plot(cor_OS)
  #plot the correlation between NTG and Silver, NTG and N50 ,Silver and N50
  par(mfrow=c(3,1))
  
  cor_GSi <- cor1[2,3,]
  cor_GSi <- as.xts(cor_GSi)
  cor_GSi <- ts(cor_GSi,start=c(2006,191),frequency=365)
  plot(cor_GSi)
  cor_GS <- cor1[2,4,]
  cor_GS <- as.xts(cor_GS)
  cor_GS <- ts(cor_GS,start=c(2006,191),frequency=365)
  plot(cor_GS)
  cor_SiS <- cor1[3,4,]
  cor_SiS <- as.xts(cor_SiS)
  cor_SiS <- ts(cor_SiS,start=c(2006,191),frequency=365)
  plot(cor_SiS)
  
  
}

AR_GARCH_CCC_WITH_OR_WITHOUT_SYMMETRY <- function(rX1,varmodel)
{
  uspec = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(model = varmodel))
  spec = cgarchspec(uspec = multispec( replicate(4, uspec) ), VAR = FALSE,  
                    dccOrder = c(1,1), distribution.model = list(copula = c("mvnorm"), method = c("ML"),
                                                                 time.varying = FALSE, transformation = "parametric"), start.pars = list(), fixed.pars = list())
  #fit1 = cgarchfit(spec, data = rX1, spd.control = list(lower = 0.1, upper = 0.9, type = "pwm",
  #                                                     kernel = "epanech"), fit.control = list(eval.se = TRUE, trace = TRUE), solver = "solnp")
  fit1 = cgarchfit(spec, data = rX1,  fit.control = list(eval.se = TRUE, trace = TRUE), solver = "solnp")
  
  print(fit1)
  cov1 = rcov(fit1)
  cor1 = rcor(fit1)
  #print the constant correlation between indices C1,C2,C3,C4,C5,C6
  print(cor1)
}

VARMA_GARCH_DCC_WITH_OR_WITHOUT_SYMMETRY<- function(rX1,varmodel)
{
  m2=VARMA(rX1,p=1,q=1,include.mean=TRUE)
  print(m2)
  ug_spec <- ugarchspec(mean.model=list(m2))
  #print(ug_spec)
  uspec.n = multispec(replicate(4, ugarchspec(variance.model=list(model=varmodel,
                                                                  garchOrder=c(1,1)),mean.model = list(m2,include.mean=TRUE))))
  multf = multifit(uspec.n, rX1)
  spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
  fit1 = dccfit(spec=spec1, data = rX1,out.sample =0, solver = "solnp",fit.control = list(eval.se = TRUE), fit = multf)
  print(fit1)
  cov1 = rcov(fit1)
  cor1 = rcor(fit1)
  #plot the correlation between OIL and NTG, OIL and Silver ,OIL and N50
  par(mfrow=c(3,1))
  
  cor_OG <- cor1[1,2,]
  cor_OG <- as.xts(cor_OG)
  cor_OG <- ts(cor_OG,start=c(2006,191),frequency=365)
  plot(cor_OG)
  cor_OSi <- cor1[1,3,]
  cor_OSi <- as.xts(cor_OSi)
  cor_OSi <- ts(cor_OSi,start=c(2006,191),frequency=365)
  plot(cor_OSi)
  cor_OS <- cor1[1,4,]
  cor_OS <- as.xts(cor_OS)
  cor_OS <- ts(cor_OS,start=c(2006,191),frequency=365)
  plot(cor_OS)
  #plot the correlation between NTG and Silver, NTG and N50 ,Silver and N50
  par(mfrow=c(3,1))
  
  cor_GSi <- cor1[2,3,]
  cor_GSi <- as.xts(cor_GSi)
  cor_GSi <- ts(cor_GSi,start=c(2006,191),frequency=365)
  plot(cor_GSi)
  cor_GS <- cor1[2,4,]
  cor_GS <- as.xts(cor_GS)
  cor_GS <- ts(cor_GS,start=c(2006,191),frequency=365)
  plot(cor_GS)
  cor_SiS <- cor1[3,4,]
  cor_SiS <- as.xts(cor_SiS)
  cor_SiS <- ts(cor_SiS,start=c(2006,191),frequency=365)
  plot(cor_SiS)
  return (fit1)
}

VARMA_GARCH_CCC_WITH_WITHOUT_SYMMETRY<- function(rX1,varmodel)
{
  m2=VARMA(rX1,p=1,q=1,include.mean=TRUE)
  #print(m2)
  uspec.n = multispec(replicate(4, ugarchspec(variance.model=list(model=varmodel,
                                                                  garchOrder=c(1,1)),mean.model = list(m2,include.mean=TRUE))))
  spec = cgarchspec(uspec = uspec.n, VAR = TRUE,  
                    dccOrder = c(1,1), distribution.model = list(copula = c("mvnorm"), method = c("ML"),
                                                                 time.varying = FALSE, transformation = "parametric"), start.pars = list(), fixed.pars = list())
  fit1 = cgarchfit(spec, data = rX1, spd.control = list(lower = 0.1, upper = 0.9, type = "pwm",
                                                        kernel = "epanech"), fit.control = list(eval.se = TRUE, trace = TRUE), solver = "solnp")
  print(fit1)
  cov1 = rcov(fit1)
  cor1 = rcor(fit1)
  #plot the correlation between OIL and NTG, OIL and Silver ,OIL and N50
  print(cor1)
  
}


DESCRIPTIVE_STATISTICS<-function()
{  
  #descriptive statistics of OIL,GAS,N50 and Silver
  print("OIL")
  print(describe(continuous_data_1$Daily_returns_oil))
  print("NTG")
  print(describe(continuous_data_1$Daily_returns_gas))
  print("N50")
  print(describe(continuous_data_1$Daily_returns_Stocks))
  print("SILVER")
  print(describe(continuous_data_1$Silver_Price))

  #Jarque-Bera Test for normality
  #test1
  print("Jarque-Bera Test for normality(Test1)")
  print("OIL")
  print(col_jarquebera(continuous_data_1$Daily_returns_oil))
  print("NTG")
  print(col_jarquebera(continuous_data_1$Daily_returns_gas))
  print("N50")
  print(col_jarquebera(continuous_data_1$Daily_returns_Stocks))
  print("SILVER")
  print(col_jarquebera(continuous_data_1$Silver_Price))
  #test2
  print("Jarque-Bera Test for normality(Test2)")
  print("OIL")
  print(jarque.bera.test(continuous_data_1$Daily_returns_oil))
  print("NTG")
  print(jarque.bera.test(continuous_data_1$Daily_returns_gas))
  print("N50")
  print(jarque.bera.test(continuous_data_1$Daily_returns_Stocks))
  print("SILVER")
  print(jarque.bera.test(continuous_data_1$Silver_Price))

  #Lagrange Multiplier (LM) test for autoregressive conditional heteroscedasticity (ARCH)  Engle's LM ARCH Test
  print("Lagrange Multiplier (LM) test for ARCH")
  print("OIL")
  wti <-continuous_data_1$Daily_returns_oil
  arch1 <- archtest(ts=as.vector(wti),lag=10)
  print(arch1)
  print("NTG")
  wti <-continuous_data_1$Daily_returns_gas
  arch2 <- archtest(ts=as.vector(wti),lag=10)
  print(arch2)
  print("N50")
  wti <-continuous_data_1$Daily_returns_Stocks
  arch3 <- archtest(ts=as.vector(wti),lag=10)
  print(arch3)
  print("SILVER")
  wti <-continuous_data_1$Silver_Price
  arch4 <- archtest(ts=as.vector(wti),lag=10)
  print(arch4)

  #unit root tests to check stationarity of timeseries
  #ADF test

  print("OIL")
  y_none = ur.df(continuous_data_1$Daily_returns_oil,type = "none", selectlags = "AIC")
  print(summary(y_none))
  print("NTG")
  y_none = ur.df(continuous_data_1$Daily_returns_gas,type = "none", selectlags = "AIC")
  print(summary(y_none))
  print("N50")
  y_none = ur.df(continuous_data_1$Daily_returns_Stocks,type = "none", selectlags = "AIC")
  print(summary(y_none))
  print("SILVER")
  y_none = ur.df(continuous_data_1$Silver_Price,type = "none", selectlags = "AIC")
  print(summary(y_none))

  #PP Test
  print("OIL")
  dp_constant = ur.pp(continuous_data_1$Daily_returns_oil,type = "Z-tau", model="constant", lags = "short")
  print(summary(dp_constant))
  print("NTG")
  dp_constant = ur.pp(continuous_data_1$Daily_returns_gas,type = "Z-tau", model="constant", lags = "short")
  print(summary(dp_constant))
  print("N50")
  dp_constant = ur.pp(continuous_data_1$Daily_returns_Stocks,type = "Z-tau", model="constant", lags = "short")
  print(summary(dp_constant))
  print("SILVER")
  dp_constant = ur.pp(continuous_data_1$Silver_Price,type = "Z-tau", model="constant", lags = "short")
  print(summary(dp_constant))

  #KPSS test

  print("OIL")
  k_test = ur.kpss(continuous_data_1$Daily_returns_oil,type = "tau", lags = "short")
  print(summary(k_test))
  print("NTG")
  k_test = ur.kpss(continuous_data_1$Daily_returns_gas,type = "tau", lags = "short")
  print(summary(k_test))
  print("N50")
  k_test = ur.kpss(continuous_data_1$Daily_returns_Stocks,type = "tau", lags = "short")
  print(summary(k_test))
  print("SILVER")
  k_test = ur.kpss(continuous_data_1$Silver_Price,type = "tau", lags = "short")
  print(summary(k_test))

  #Q for finding serial correlations
  #data=continuous_data_1$Daily_returns_oil
  #data=continuous_data_1$Daily_returns_gas
  #data=continuous_data_1$Daily_returns_Stocks
  data=continuous_data_1$Silver_Price
  fit1=arima(data,order=c(1,0,0))
  print(Box.test(resid(fit1),type="Ljung",lag=20,fitdf=1))

}

#Cointegration Tests
CONINTEGRATION_TESTS<-function()
{
  #Engle and Granger (1987) cointegration tests :put type="trend" for the test with trend
  print("NTG~OIL")
  longrun=lm(continuous_data_1$Daily_returns_gas~continuous_data_1$Daily_returns_oil)
  resid_longrun=longrun$residuals
  y=ur.df(resid_longrun,type="trend",selectlags = "AIC")
  print(summary(y))
  
  print("NTG~N50")
  longrun=lm(continuous_data_1$Daily_returns_gas~continuous_data_1$Daily_returns_Stocks)
  resid_longrun=longrun$residuals
  y=ur.df(resid_longrun,type="none",selectlags = "AIC")
  print(summary(y))
  
  print("NTG~SILVER")
  longrun=lm(continuous_data_1$Daily_returns_gas~continuous_data_1$Silver_Price)
  resid_longrun=longrun$residuals
  y=ur.df(resid_longrun,type="trend",selectlags = "AIC")
  print(summary(y))

  print("OIL~N50")
  longrun=lm(continuous_data_1$Daily_returns_oil~continuous_data_1$Daily_returns_Stocks)
  resid_longrun=longrun$residuals
  y=ur.df(resid_longrun,type="trend",selectlags = "AIC")
  print(summary(y))

  print("OIL~SILVER")
  longrun=lm(continuous_data_1$Daily_returns_oil~continuous_data_1$Silver_Price)
  resid_longrun=longrun$residuals
  y=ur.df(resid_longrun,type="none",selectlags = "AIC")
  print(summary(y))

  print("SILVER~N50")
  longrun=lm(continuous_data_1$Silver_Price~continuous_data_1$Daily_returns_Stocks)
  resid_longrun=longrun$residuals
  y=ur.df(resid_longrun,type="trend",selectlags = "AIC")
  print(summary(y))

  
  #Johansen cointegration tests

  jotest=ca.jo(data.frame(continuous_data_1$Daily_returns_oil,continuous_data_1$Daily_returns_gas,continuous_data_1$Silver_Price,continuous_data_1$Daily_returns_Stocks), type="trace", K=2, ecdet="none", spec="longrun")
  print(summary(jotest))

  #Phillips-Ouliaris cointegration test

  dat<-data.frame(continuous_data_1$Daily_returns_Stocks,continuous_data_1$Daily_returns_gas,continuous_data_1$Daily_returns_oil,continuous_data_1$Silver_Price)
    
  print(po.test(dat, demean = TRUE, lshort = FALSE))
  print(ca.po(dat, demean = "constant", lag = "long", type = "Pu"))
  print(ca.po(dat, demean = "constant", lag = "long", type = "Pz"))

}

#forecast correlation between OIL and NTG, OIL and Silver,OIL and N50,NTG and Silver, NTG and N50,Silver and N50
FORECAST_CORRELATION<-function(bestmodel)
{
  cor1 = rcor(bestmodel)
  cov1 = rcov(bestmodel)
  
  
  dccf1 <- dccforecast(bestmodel, n.ahead = 20)
  plot(dccf1, which = 1, series=c(1,2,3,4))
  plot(dccf1, which = 2, series=c(1,2,3,4))
  Rf <- dccf1@mforecast$R
  str(Rf)
  corf_OG <- Rf[[1]][1,2,]
  corf_OSi <- Rf[[1]][1,3,]
  corf_OS <- Rf[[1]][1,4,]
  corf_GSi <- Rf[[1]][2,3,]
  corf_GS <- Rf[[1]][2,4,]
  corf_SiS <- Rf[[1]][3,4,]
  print("Forecast of correlation between Oil and N50 for 20 days")
  #Forecast of correlation between Oil and N50 for 20 days
  print(corf_OS)
  #plot the forcasted correlation between OIL and NTG, OIL and Silver ,OIL and N50
  par(mfrow=c(3,1))
  
  c_OG <- c(tail(cor1[1,2,],30),rep(NA,20))
  cf_OG <- c(rep(NA,30),corf_OG)
  plot(c_OG,type = "l",main="Correlation OIL and NTG")
  lines(cf_OG,type = "l", col = "orange")
  
  c_OSi <- c(tail(cor1[1,3,],30),rep(NA,20))
  cf_OSi <- c(rep(NA,30),corf_OSi)
  plot(c_OSi,type = "l",main="Correlation OIL and SILVER")
  lines(cf_OSi,type = "l", col = "orange")
  
  c_OS <- c(tail(cor1[1,4,],30),rep(NA,20))
  cf_OS <- c(rep(NA,30),corf_OS)
  plot(c_OS,type = "l",main="Correlation OIL and N50")
  lines(cf_OS,type = "l", col = "orange")
  
  #plot the forcasted correlation between NTG and Silver, NTG and N50,Silver and N50
  par(mfrow=c(3,1))
  c_GSi <- c(tail(cor1[2,3,],30),rep(NA,20))
  cf_GSi <- c(rep(NA,30),corf_GSi)
  plot(c_GSi,type = "l",main="Correlation NTG and SILVER")
  lines(cf_GSi,type = "l", col = "orange")
  
  c_GS <- c(tail(cor1[2,4,],30),rep(NA,20))
  cf_GS <- c(rep(NA,30),corf_GS)
  plot(c_GS,type = "l",main="Correlation NTG and N50")
  lines(cf_GS,type = "l", col = "orange")
  
  c_SiS <- c(tail(cor1[3,4,],30),rep(NA,20))
  cf_SiS <- c(rep(NA,30),corf_SiS)
  plot(c_SiS,type = "l",main="Correlation SILVER and N50")
  lines(cf_SiS,type = "l", col = "orange")
  
}


PORTFOLIO_WEIGHTS_AND_HEDGE_RATIO<-function(rX1,vol,cor1,cov1)
{
  #nrow(rX1)=5021
  #Portfolio weights(w1,w2,w3) & Hedge Ratios(b1,b2,b3)
  #w1<-N50/OIL w2<-N50/NTG w3<-N50/Silver
  #b1<-N50/OIL b2<-N50/NTG b3<-N50/Silver
  w1<-rep(0,nrow(rX1))
  w2<-rep(0,nrow(rX1))
  w3<-rep(0,nrow(rX1))
  b1<-rep(0,nrow(rX1))
  b2<-rep(0,nrow(rX1))
  b3<-rep(0,nrow(rX1))
  for(i in 1:5021)
  {
    htc<-vol[i,1]
    hts<-vol[i,4]
    htcs<-cov1[1,4,i]
    b1[i]<-htcs/htc
    val<-hts-htcs
    val1<-htc-(2*htcs)+hts
    w1[i]<-1-(val/val1)
    htc<-vol[i,2]
    htcs<-cov1[2,4,i]
    b2[i]<-htcs/htc
    val<-hts-htcs
    val1<-htc-(2*htcs)+hts
    w2[i]<-1-(val/val1)
    htc<-vol[i,3]
    htcs<-cov1[3,4,i]
    b3[i]<-htcs/htc
    val<-hts-htcs
    val1<-htc-(2*htcs)+hts
    w3[i]<-1-(val/val1)
  }
  print("portfolio weights N50/OIL")
  print(describe(w1))
  print("portfolio weights N50/NTG")
  print(describe(w2))
  print("portfolio weights N50/SILVER")
  print(describe(w3))
  
  #plot portfolio weights w1,w2,w3
  w1_plot<-ts(w1,start=c(2006,191),frequency=365)
  plot(w1_plot)
  w2_plot<-ts(w2,start=c(2006,191),frequency=365)
  plot(w2_plot)
  w3_plot<-ts(w3,start=c(2006,191),frequency=365)
  plot(w3_plot)
  
  print("Hedge ratios N50/OIL")
  print(describe(b1))
  print("Hedge ratios N50/NTG")
  print(describe(b2))
  print("Hedge ratios N50/SILVER")
  print(describe(b3))
  #plot Hedging ratios b1,b2,b3
  par(mfrow=c(3,1))
  b1_plot<-ts(b1,start=c(2006,191),frequency=365)
  plot(b1_plot)
  b2_plot<-ts(b2,start=c(2006,191),frequency=365)
  plot(b2_plot)
  b3_plot<-ts(b3,start=c(2006,191),frequency=365)
  plot(b3_plot)
  
}




#plot the timeseries OIL,NTG,SILVER and N50
par(mfrow=c(3,1))
OIL<-ts(continuous_data_1$Daily_returns_oil,start=c(2006,191),frequency=365)
plot(OIL)
NTG<-ts(continuous_data_1$Daily_returns_gas,start=c(2006,191),frequency=365)
plot(NTG)
SILVER<-ts(continuous_data_1$Silver_Price,start=c(2006,191),frequency=365)
plot(SILVER)
N50<-ts(continuous_data_1$Daily_returns_Stocks,start=c(2006,191),frequency=365)
plot(N50)

#descriptive statistics of daily returns of OIL,NTG,SILVER and N50
DESCRIPTIVE_STATISTICS()

#Cointegration tests for OIL, NTG, SILVER and N50
CONINTEGRATION_TESTS()

rX1 <- data.frame(continuous_data_1$Daily_returns_oil,continuous_data_1$Daily_returns_gas,continuous_data_1$Silver_Price,continuous_data_1$Daily_returns_Stocks)
names(rX1)[1] <- "OIL"
names(rX1)[2] <- "NTG"
names(rX1)[3] <- "Silver"
names(rX1)[4] <- "N50"

#AR (1)-multivariate GARCH(1,1) DCC WITHOUT ASYMMETRY
AR_GARCH_DCC_WITH_OR_WITHOUT_SYMMETRY(rX1,varmodel="sGARCH")

#AR (1)-multivariate GARCH(1,1) DCC WITH ASYMMETRY
AR_GARCH_DCC_WITH_OR_WITHOUT_SYMMETRY(rX1,varmodel="gjrGARCH")

#AR (1)-multivariate GARCH(1,1) CCC WITHOUT ASYMMETRY
AR_GARCH_CCC_WITH_OR_WITHOUT_SYMMETRY(rX1,varmodel="sGARCH")

#AR (1)-multivariate GARCH(1,1) CCC WITH ASYMMETRY
AR_GARCH_CCC_WITH_OR_WITHOUT_SYMMETRY(rX1,varmodel="gjrGARCH")

#VARMA (1,1)–multivariate GARCH(1,1) DCC WITHOUT ASYMMETRY
MODEL<-VARMA_GARCH_DCC_WITH_OR_WITHOUT_SYMMETRY(rX1,varmodel="sGARCH")

#VARMA (1,1)–multivariate GARCH(1,1) DCC WITH ASYMMETRY
bestmodel<-VARMA_GARCH_DCC_WITH_OR_WITHOUT_SYMMETRY(rX1,varmodel="gjrGARCH")

#VARMA (1,1)–multivariate GARCH(1,1) CCC WITHOUT ASYMMETRY
VARMA_GARCH_CCC_WITH_WITHOUT_SYMMETRY(rX1,varmodel="sGARCH")

#VARMA (1,1)–multivariate GARCH(1,1) CCC WITH ASYMMETRY
VARMA_GARCH_CCC_WITH_WITHOUT_SYMMETRY(rX1,varmodel="gjrGARCH")

#plot volatility for OIL,NTG,N50
vol<-sigma(bestmodel)
volatility <- ts(vol,start=c(2006,191),frequency=365)
plot(volatility)
cor1 = rcor(bestmodel)
cov1 = rcov(bestmodel)

#forecast correlation between OIL and NTG, OIL and Silver,OIL and N50,NTG and Silver, NTG and N50,Silver and N50
FORECAST_CORRELATION(bestmodel)

#Various Risk measures
print("Risk measures of N50/OIL")
risk.hedge(continuous_data_1$Daily_returns_Stocks, continuous_data_1$Daily_returns_oil, c(0.01, 0.05))
print("Risk measures of N50/NTG")
risk.hedge(continuous_data_1$Daily_returns_Stocks, continuous_data_1$Daily_returns_gas, c(0.01, 0.05))
print("Risk measures of N50/SILVER")
risk.hedge(continuous_data_1$Daily_returns_Stocks, continuous_data_1$Silver_Price, c(0.01, 0.05))


#Optimal portfolio weights and hedge ratios.
PORTFOLIO_WEIGHTS_AND_HEDGE_RATIO(rX1,vol,cor1,cov1)
