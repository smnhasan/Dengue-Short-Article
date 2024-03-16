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


setwd('E:\\Dengue')
Dengue <- read.csv("DengueDataCases.csv")

#Descriptive
describe.by(Dengue$Values, Dengue$Years)
describe(Dengue$Values)

fyearwise <- aggregate(Dengue$Values, by=list(Category=Dengue$Years), FUN=sum)
fyearwise <- fyearwise[which(fyearwise$Category<='2010'), ]
fyearwise
mean(fyearwise$x)

syearwise <- aggregate(Dengue$Values, by=list(Category=Dengue$Years), FUN=sum)
syearwise <- syearwise[which(syearwise$Category>'2010'), ]
syearwise
mean(syearwise$x)

t.test(fyearwise$x, syearwise$x[1:11], paired = TRUE, alternative = "two.sided")


monthwise <- aggregate(Dengue$Values, by=list(Category=Dengue$Months), FUN=min)
monthwise

monthwise <- aggregate(Dengue$Values, by=list(Category=Dengue$Months), FUN=mean)
monthwise

monthwise <- aggregate(Dengue$Values, by=list(Category=Dengue$Months), FUN=sd)
monthwise

monthwise <- aggregate(Dengue$Values, by=list(Category=Dengue$Months), FUN=max)
monthwise

#Monthly
library(ggplot2)
library(forecast)
theme_set(theme_classic())

DengueTS <- ts(Dengue$Values, frequency=12, start=c(2000,1), end=c(2022,9))

# Plot
a <- ggseasonplot(DengueTS)+ scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                                         labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()  + xlab("Months") + ylab("Number of dengue cases") + ggtitle("") +  
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 12), ) 

a

#GF
Dengue$Values_GF <- (Dengue$Values +1)/(lag(Dengue$Values)+1)

Dengue_mean <- aggregate(Dengue$Values_GF, by=list(Category=Dengue$Months), FUN=mean, na.rm=T)

Dengue_sd <- aggregate(Dengue$Values_GF, by=list(Category=Dengue$Months), FUN=SD, na.rm=T)

 
margin <- qt(0.975,df=12-1)*Dengue_sd$x / sqrt(12)

Dengue_sd$lower.ci <- Dengue_mean$x - margin
Dengue_sd$upper.ci = Dengue_mean$x + margin

df2 <- data.frame(month=c(4, 8,12, 2,1, 7,
                         6, 3,5, 11,10, 9,
                         4, 8,12, 2,1, 7,
                         6, 3,5, 11,10, 9,
                         4, 8,12, 2,1, 7,
                         6, 3,5, 11,10, 9),
                  cat=c("low", "low","low", "low","low", "low",
                          "low", "low","low", "low","low", "low",
                          "mean", "mean","mean", "mean","mean", "mean",
                          "mean", "mean","mean", "mean","mean", "mean",
                          "up", "up","up", "up","up", "up",
                          "up", "up","up", "up","up", "up"),
                  value=c(0.9489651, 0.9473850, 0.2201475, 0.6163374, 0.195421, -10.9609168,
                         2.9783958, 0.9087754, 1.4596879, -16.7374857, 0.4658711, 0.7984997,
                         1.2812284, 5.1857539, 0.3463382,0.7566080, 0.3740667, 58.7525127,
                         20.4959093,1.3025297, 2.7040933, 17.5909212,0.7559118, 1.0867931,
                         1.6134918, 9.4241228, 0.4725290, 0.8968786, 0.5527118, 128.4659422,
                         38.0134227, 1.6962839, 3.9484986, 51.9193281, 1.0459524, 1.3750864))
df2

ggplot(df2, aes(x=factor(month), y=value, group=cat)) +
  geom_line(aes(linetype=cat))+ scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()  + xlab("Months") + ylab("Number of dengue cases") + ggtitle("") +  
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 12) ) 

#ARIMA
setwd('E:\\Dengue')
Dengue <- read.csv("DengueDataCases.csv")
DengueTS <- ts(Dengue$Values, frequency=12, start=c(2000,1))

auto.arima(DengueTS)
Fit<-Arima(DengueTS,order=c(1,1,2))
summary(Fit)
fcast <- forecast(Fit, h=10)

z <- autoplot(fcast)  +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
  fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8))
z

rn <- nrow(Dengue)
#R2
SSE <- sum((resid(Fit[1:273]))^2)
SST <- sum((Dengue$Values[1:273] - mean(Dengue$Values[1:273]))^2)
R_square <- 1 - SSE / SST
R_square






####SES########

library(tidyverse) 
library(fpp2) 
library(ggfortify)
ses.goog <- ses(DengueTS,  
                h = 10) 
summary(ses.goog)

fcast <- forecast(ses.goog, h=10)

x <- autoplot(ses.goog)  +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8))
x

rn <- nrow(Dengue)
#R2
SSE <- sum((resid(fcast[1:273]))^2)
SST <- sum((Dengue$Values[1:273] - mean(Dengue$Values[1:273]))^2)
R_square <- 1 - SSE / SST
R_square





#Prophet

history <- data.frame(ds = seq(as.Date('2000-01-01'), as.Date('2022-09-30'), by = 'm'),
                      y = Dengue$Values)  


lower = quantile(history$y, .05)
upper = quantile(history$y, .95)

history <- history %>% mutate(floor = lower, cap = upper)

m3 <- prophet(history, changepoint.prior.scale=0.01, 
              growth = 'logistic')
future <- make_future_dataframe(m3, periods = 10) %>% mutate(floor = lower, cap = upper)
fcst3 <- predict(m3, future)
y <-plot(m3, fcst3, xlab="Years", ylab="Number of dengue cases") + ggtitle("Prophet Model") + theme(
  plot.title = element_text(size=12))
plot(y)


SSE <- sum((history$y[1:273] - fcst3$yhat[c(1:273)])^2)
SST <- sum((history$y[1:273] - mean(history$y[1:273]))^2)
R_square <- 1 - SSE / SST
R_square

last_fcst3 <- fcst3[273,]
rmse <- sqrt(mean((history$y - fcst3$yhat[c(1:273)])^2))
mae <- mean(abs((history$y - fcst3$yhat[c(1:273)])))
final <- cbind(last_fcst3, rmse, mae)
final

