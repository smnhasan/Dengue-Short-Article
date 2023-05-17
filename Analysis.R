library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(psych)
require(MASS) # to access Animals data sets
require(scales) # to access break formatting functions

library(mgcv)
library(GGally)
library(mgcv)
library(visreg)



setwd('E:\\Dengue')
Dengue <- read.csv("WeatherData.csv")

#Descriptive
describe.by(Dengue$DC, Dengue$Year)
describe(Dengue$DC)

fmonthwise <- Dengue[which(Dengue$Year<='2010'), ]
NROW(fmonthwise)
mean(fmonthwise$DC)
sd(fmonthwise$DC)

smonthwise <- Dengue[which(Dengue$Year>'2010'), ]
NROW(smonthwise$DC)
mean(smonthwise$DC)
sd(smonthwise$DC)

t.test(fmonthwise$DC[1:132], smonthwise$DC[1:132], paired = TRUE, alternative = "two.sided")

# fyearwise <- aggregate(Dengue$Values, by=list(Category=Dengue$Years), FUN=sum)
# fyearwise <- fyearwise[which(fyearwise$Category<='2010'), ]
# fyearwise
# mean(fyearwise$x)
# sd(fyearwise$x)
# 
# syearwise <- aggregate(Dengue$Values, by=list(Category=Dengue$Years), FUN=sum)
# syearwise <- syearwise[which(syearwise$Category>'2010'), ]
# syearwise
# mean(syearwise$x)
# 
# 
# 
# t.test(fyearwise$x, syearwise$x[1:11], paired = TRUE, alternative = "two.sided")


monthwise <- aggregate(Dengue$DC, by=list(Category=Dengue$Month), FUN=min)
monthwise

monthwise <- aggregate(Dengue$DC, by=list(Category=Dengue$Month), FUN=mean)
monthwise

monthwise <- aggregate(Dengue$DC, by=list(Category=Dengue$Month), FUN=sd)
monthwise

monthwise <- aggregate(Dengue$DC, by=list(Category=Dengue$Month), FUN=max)
monthwise

#Monthly
library(ggplot2)
library(forecast)
theme_set(theme_classic())

DengueTS <- ts(Dengue$DC, frequency=12, start=c(2000,1), end=c(2022,12))

# Plot
a <- ggseasonplot(DengueTS)+ scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                                         labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()  + xlab("Months") + ylab("Number of dengue cases") + ggtitle("") +  
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 18), legend.position = c(0.1, 0.8),
         text = element_text(size = 18)) 

a

tiff("DC.tiff", units="in", width=12, height=12, res=300)
gridExtra::grid.arrange(a)
dev.off()


t <- (Dengue$DC +1)/(lag(Dengue$DC)+1)
DengueGF <- ts(t[2:276], frequency=12, start=c(2000,1), end=c(2022,12))
# Plot
b <- ggseasonplot(DengueGF) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                              labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()  + xlab("Months") + ylab("Monthly growth factor") + ggtitle("") +   
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 18), legend.position = c(0.1, 0.75),
         text = element_text(size = 18) ) 

b

#GF
Dengue$Values_GF <- log((Dengue$DC +1)/(lag(Dengue$DC)+1))

Dengue_mean <- aggregate(Dengue$Values_GF, by=list(Category=Dengue$Month), FUN=mean, na.rm=T)
Dengue_mean
mean(Dengue_mean$x)
sd(Dengue_mean$x)

Dengue_sd <- aggregate(Dengue$Values_GF, by=list(Category=Dengue$Month), FUN=SD, na.rm=T)

 
margin <- qt(0.975,df=11-1)*Dengue_sd$x / sqrt(11)

Dengue_sd$lower.ci <- Dengue_mean$x - margin
Dengue_sd$lower.ci
Dengue_sd$upper.ci = Dengue_mean$x + margin
Dengue_sd$upper.ci

my.data <- data.frame(time     = c(1, 2,3, 4,5, 6,
                                   7, 8,9, 10,11, 12,
                                   1, 2,3, 4,5, 6,
                                   7, 8,9, 10,11, 12,
                                   1, 2,3, 4,5, 6,
                                   7, 8,9, 10,11, 12),
                      means    = c(-1.88228924,
                                   -0.41273480,
                                   0.11089769,
                                   0.08781648,
                                   0.58480374,
                                   1.53387538,
                                   2.08559404,
                                   0.92596613,
                                   -0.11126596,
                                   -0.60954292,
                                   -0.55857540,
                                   -1.46585422),
                      lowerCI  = c(-2.986834689, -0.808638654, -0.241364864, -0.309106201,  -0.001155349,  
                                   0.400767119,  0.968619389,  0.241435698, -0.557014633,
                                   -1.431431253, -1.829895986, -2.197912657),
                      upperCI  = c(-0.77774379, -0.01683095,  0.46316025, 0.48473916,  1.17076284,  
                                   2.66698364,  3.20256870,  1.61049656, 0.33448272,
                                   0.21234540,  0.71274519, -0.73379578),
                      scenario = rep(c("Mean monthly growth factor"), each=3))


c <- ggplot(my.data, aes(x = factor(time), y = exp(means), group = scenario))+
  geom_line(aes(colour = scenario))+
  geom_line(aes(y = exp(lowerCI), colour = paste(scenario, '95% Confidence interval')),
            linetype = 'dashed')+
  geom_line(aes(y = exp(upperCI), colour = paste(scenario, '95% Confidence interval')),
            linetype = 'dashed')+
  scale_colour_manual(values = c('Mean monthly growth factor' = 'black',
                                 '95% Confidence interval' = 'black'),
                      breaks = c( 'Mean monthly growth factor', '95% Confidence interval'))+  ylab("Monthly growth factor") + 
  xlab("Months") + ggtitle("") +  
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 18),
         legend.position = c(0.2, 0.9),
         text = element_text(size = 18)) +
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))

c


tiff("GF.tiff", units="in", width=12, height=16, res=300)
gridExtra::grid.arrange(c,b)
dev.off()


#ARIMA
setwd('E:\\Dengue')
Dengue <- read.csv("WeatherData.csv")
View(Dengue)
DengueTS <- ts(Dengue$DC, frequency=12, start=c(2000,1))

auto.arima(DengueTS)

Fit<-Arima(DengueTS,order=c(0,1,0))
summary(Fit)

Fit<-Arima(DengueTS,order=c(1,0,0))
summary(Fit)

Fit<-Arima(DengueTS,order=c(0,0,1))
summary(Fit)

Fit<-Arima(DengueTS,order=c(1,0,1))
summary(Fit)

Fit<-Arima(DengueTS,order=c(1,1,1))
summary(Fit)

fcast <- forecast(Fit, h=12)

z <- autoplot(fcast)  +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
  fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18))
z

rn <- nrow(Dengue)
#R2
SSE <- sum((resid(Fit[1:nrow(Dengue)]))^2)
SST <- sum((Dengue$DC[1:nrow(Dengue)] - mean(Dengue$DC[1:nrow(Dengue)]))^2)
R_square <- 1 - SSE / SST
R_square


library(vars)
#ARIMAX



a <- autoplot(ccf(Dengue$DC[37:276],Dengue$Rainfall[37:276], 12), main = "Dengue Cases vs Rainfall", ylab = "CCF")+
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18)) #1/2
c <- autoplot(ccf(Dengue$DC[37:276],Dengue$MaxT[37:276], 12), main = "Dengue Cases vs Tmax", ylab = "CCF")+
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18)) #2/3/4
d <- autoplot(ccf(Dengue$DC[37:276],Dengue$MinT[37:276], 12), main = "Dengue Cases vs Tmin", ylab = "CCF")+
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18)) #0/1/2/3/12
e <- autoplot(ccf(Dengue$DC[37:276],Dengue$AvgT[37:276], 12), main = "Dengue Cases vs Tavg", ylab = "CCF")+
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18)) #0/1/2/3/12


tiff("ccf.tiff", units="in", width=18, height=12, res=300)
gridExtra::grid.arrange(a,c,d,e)
dev.off()


#######Count GLM

AvgT <- Dengue$AvgT[37:276]
Rainfall <- Dengue$Rainfall[37:276]

lag1AvgT <- lag(AvgT)[2:240]
lag1Rainfall <- lag(Rainfall)[2:240]

lag2AvgT <- lag(lag1AvgT)[2:239]
lag2Rainfall <- lag(lag1Rainfall)[2:239]

deng <- data.frame(AvgT[1:238], Rainfall[1:238], lag1AvgT[1:238], lag2AvgT[1:238], lag1Rainfall[1:238], lag2Rainfall[1:238])
deng

deng <- data.frame(AvgT[1:240], Rainfall[1:240])
deng
#Dengue$DC[37:274]
library(tscount)
fit_pois <- tsglm(Dengue$DC[37:276], model = list(past_obs = 1, past_mean = 1), xreg = deng,  distr = "poisson")

summary(fit_pois)
coeftest(fit_pois)

exp(fit_pois$coefficients)

round(exp(confint(fit_pois)),3)

#SARIMA

auto.arima(DengueTS, D = T)

Fit<-Arima(DengueTS, order=c(0,0,2), seasonal=list(order=c(2,1,0),period=12))
summary(Fit)

Fit<-Arima(DengueTS, order=c(0,1,1), seasonal=list(order=c(1,1,0),period=12))
summary(Fit)

Fit<-Arima(DengueTS, order=c(1,1,1), seasonal=list(order=c(0,1,0),period=12))
summary(Fit)

Fit<-Arima(DengueTS, order=c(1,0,1), seasonal=list(order=c(1,1,0),period=12))
summary(Fit)

Fit<-Arima(DengueTS, order=c(0,0,1), seasonal=list(order=c(1,1,1),period=12))
summary(Fit)

fcast <- forecast(Fit, h=12)

n <- autoplot(fcast)  +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("SARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18))
n

rn <- nrow(Dengue)
#R2
SSE <- sum((resid(Fit[1:nrow(Dengue)]))^2)
SST <- sum((Dengue$DC[1:nrow(Dengue)] - mean(Dengue$DC[1:nrow(Dengue)]))^2)
R_square <- 1 - SSE / SST
R_square






####SES########

library(tidyverse) 
library(fpp2) 
library(ggfortify)
ses.goog <- ses(DengueTS,  
                h = 12) 
summary(ses.goog)

fcast <- forecast(ses.goog, h=10)

x <- autoplot(ses.goog)  +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("SES Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18))
x

rn <- nrow(Dengue)
#R2
SSE <- sum((resid(fcast[1:nrow(Dengue)]))^2)
SST <- sum((Dengue$DC[1:nrow(Dengue)] - mean(Dengue$DC[1:nrow(Dengue)]))^2)
R_square <- 1 - SSE / SST
R_square

#Prophet

history <- data.frame(ds = seq(as.Date('2000-01-01'), as.Date('2022-12-31'), by = 'm'),
                      y = Dengue$DC)  


lower = quantile(history$y, .05)
upper = quantile(history$y, .95)

history <- history %>% mutate(floor = lower, cap = upper)

m3 <- prophet(history, changepoint.prior.scale=0.01, 
              growth = 'logistic')
future <- make_future_dataframe(m3, periods = 12) %>% mutate(floor = lower, cap = upper)
fcst3 <- predict(m3, future)
y <-plot(m3, fcst3, xlab="Years", ylab="Number of dengue cases") + ggtitle("Prophet Model") + theme(
  plot.title = element_text(size=18),
  legend.text = element_text(color = "Black", size = 18),
  text = element_text(size = 18))
plot(y)

SSE <- sum((history$y[1:rn] - fcst3$yhat[c(1:rn)])^2)
SST <- sum((history$y[1:rn] - mean(history$y[1:rn]))^2)
R_square <- 1 - SSE / SST
R_square

last_fcst3 <- fcst3[rn,]
rmse <- sqrt(mean((history$y - fcst3$yhat[c(1:rn)])^2))
mae <- mean(abs((history$y - fcst3$yhat[c(1:rn)])))
final <- cbind(last_fcst3, rmse, mae)
final


tiff("TS.tiff", units="in", width=16, height=10, res=300)
gridExtra::grid.arrange(x,z,n,y)
dev.off()

#Menn kendal
library(Kendall)
library(trend)

myts <- ts(Dengue$DC)
t.test(Dengue$DC)$"conf.int"
mean(Dengue$DC)

MannKendall(myts)
sens.slope(myts, conf.level = 0.95)


#Seasonal

SMK = SeasonalMannKendall(DengueTS)

summary(SMK)

library(trend)

DengueTS <- ts(Dengue$DC, frequency=12, start=c(2003,1))
sea.sens.slope(DengueTS)
library(kendallSeasonalTrendTest)

kendallSeasonalTrendTest(DengueTS)
