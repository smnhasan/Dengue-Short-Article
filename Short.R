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
library(dplyr)
library(tidyr)
library(ggplot2)

setwd('E:\\Dengue')
Dengue <- read.csv("WeatherData.csv")
# View(Dengue)
Dengue_03_21 <- Dengue[which(Dengue$Year >= 2003 & Dengue$Year <= 2021),]
Dengue_03_21$Year

Dengue_03_21_rainfall <- aggregate(Dengue_03_21$Rainfall, list(Dengue_03_21$Month), FUN=mean)
colnames(Dengue_03_21_rainfall) <- c("Month", "Rainfall 2003-2021")
Dengue_03_21_rainfall

Dengue_03_21_AvgT <- aggregate(Dengue_03_21$AvgT, list(Dengue_03_21$Month), FUN=mean)
colnames(Dengue_03_21_AvgT) <- c("Month", "Average Temperature 2003-2021")
Dengue_03_21_AvgT

Dengue_03_21_DC <- aggregate(Dengue_03_21$DC, list(Dengue_03_21$Month), FUN=mean)
colnames(Dengue_03_21_DC) <- c("Month", "Dengue Case 2003-2021")
Dengue_03_21_DC

Dengue_03_21_DD <- aggregate(Dengue_03_21$DD, list(Dengue_03_21$Month), FUN=mean)
colnames(Dengue_03_21_DD) <- c("Month", "Dengue Death 2003-2021")
Dengue_03_21_DD


Dengue_22 <- Dengue[which(Dengue$Year > 2021),]
colnames(Dengue_22) <- c("Year","Month","Rainfall 2022", "RH", "MaxT", "MinT", "Average Temperature 2022", "Dengue Case 2022","Dengue Death 2022")
Dengue_22

Dengue_03_21_rainfall_september <- Dengue_03_21_rainfall[which(Dengue_03_21_rainfall$Month == 9),]
Dengue_03_21_rainfall_september

Dengue_03_21_rainfall_oct <- Dengue_03_21_rainfall[which(Dengue_03_21_rainfall$Month == 10),]
Dengue_03_21_rainfall_oct


Dengue_03_21_AvgT_july <- Dengue_03_21_AvgT[which(Dengue_03_21_AvgT$Month == 7),]
Dengue_03_21_AvgT_july

Dengue_03_21_AvgT_aug <- Dengue_03_21_AvgT[which(Dengue_03_21_AvgT$Month == 8),]
Dengue_03_21_AvgT_aug

Dengue_22_september <- Dengue_22[which(Dengue_22$Month == 9),]
Dengue_22_september

Dengue_22_oct <- Dengue_22[which(Dengue_22$Month == 10),]
Dengue_22_oct

Dengue_22_july <- Dengue_22[which(Dengue_22$Month == 7),]
Dengue_22_july

Dengue_22_aug <- Dengue_22[which(Dengue_22$Month == 8),]
Dengue_22_aug

Rainfall_sep_diff <- Dengue_03_21_rainfall_september[2] - Dengue_22_september[3]
Rainfall_sep_diff

Rainfall_oct_diff <- Dengue_03_21_rainfall_oct[2] - Dengue_22_oct[3]
Rainfall_oct_diff


AvgT_july_diff <- Dengue_03_21_AvgT_july[2] - Dengue_22_july[7]
AvgT_july_diff

AvgT_aug_diff <- Dengue_03_21_AvgT_aug[2] - Dengue_22_aug[7]
AvgT_aug_diff

x = merge(Dengue_22, Dengue_03_21_rainfall, by = "Month")
x
y = merge(x, Dengue_03_21_AvgT, by = "Month")
y
z = merge(y, Dengue_03_21_DC, by = "Month")
z

a = merge(z, Dengue_03_21_DD, by = "Month")
a


# convert this to long format, use BA as id
Data <- a %>% pivot_longer(-Month)

Data_rainfall <- Data[which(Data$name == "Rainfall 2022" | Data$name == "Rainfall 2003-2021"),]
Data_rainfall

#define colors
COLS = c("red","orange")
names(COLS) = c("Rainfall 2022","Rainfall 2003-2021")
###
x <- ggplot(Data_rainfall) + 
  geom_line(aes(Month,value,colour=name,fill=name),size=1) +
  # change name of legend here 
  scale_fill_manual(name="group",values=COLS)+
  scale_color_manual(name="",values=COLS) + scale_x_discrete(limits = c("1", "2", "3", 
                                                                            "4", "5", "6", 
                                                                            "7", "8", "9", 
                                                                            "10", "11", "12"),
                                                                 labels = c("Jan", "Feb", 
                                                                            "Mar", "Apr", "May", 
                                                                            "Jun", "Jul",
                                                                            "Aug", "Sep", 
                                                                            "Oct", "Nov", "Dec")) +  ylab("Rainfall") + 
  xlab("Months") + ggtitle("") + theme(plot.title = element_text(size = 18, face = "bold"),
                                       legend.title=element_text(size=18), 
                                       legend.text=element_text(size=18),
                                       text = element_text(size = 20)) #+ theme(legend.position = c(0.15, 0.9))

x

Data_temp <- Data[which(Data$name == "Average Temperature 2022" | Data$name == "Average Temperature 2003-2021"),]
Data_temp

#define colors
COLS = c("red","orange")
names(COLS) = c("Average Temperature 2022","Average Temperature 2003-2021")
###
y <- ggplot(Data_temp) + 
  geom_line(aes(Month,value,colour=name,fill=name),size=1) +
  # change name of legend here 
  scale_fill_manual(name="group",values=COLS)+
  scale_color_manual(name="",values=COLS) + scale_x_discrete(limits = c("1", "2", "3", 
                                                                             "4", "5", "6", 
                                                                             "7", "8", "9", 
                                                                             "10", "11", "12"),
                                                                  labels = c("Jan", "Feb", 
                                                                             "Mar", "Apr", "May", 
                                                                             "Jun", "Jul",
                                                                             "Aug", "Sep", 
                                                                             "Oct", "Nov", "Dec"))+  ylab("Average Temperature") + 
  xlab("Months") + ggtitle("")  + theme(plot.title = element_text(size = 18, face = "bold"),
                                        legend.title=element_text(size=18), 
                                        legend.text=element_text(size=18),
                                        text = element_text(size = 20)) #+ theme(legend.position = c(0.15, 0.9))
y

Data_dngcase <- Data[which(Data$name == "Dengue Case 2022" | Data$name == "Dengue Case 2003-2021"),]
Data_dngcase

#define colors
COLS = c("red","orange")
names(COLS) = c("Dengue Case 2022","Dengue Case 2003-2021")
###
z <- ggplot(Data_dngcase) + 
  geom_line(aes(Month,value,colour=name,fill=name),size=1) +
  # change name of legend here 
  scale_fill_manual(name="group",values=COLS)+
  scale_color_manual(name="",values=COLS) + scale_x_discrete(limits = c("1", "2", "3", 
                                                                             "4", "5", "6", 
                                                                             "7", "8", "9", 
                                                                             "10", "11", "12"),
                                                                  labels = c("Jan", "Feb", 
                                                                             "Mar", "Apr", "May", 
                                                                             "Jun", "Jul",
                                                                             "Aug", "Sep", 
                                                                             "Oct", "Nov", "Dec"))+  ylab("Dengue Cases") + 
  xlab("Months") + ggtitle("")  + theme(plot.title = element_text(size = 18, face = "bold"),
                                        legend.title=element_text(size=18), 
                                        legend.text=element_text(size=18),
                                        text = element_text(size = 20)) #+ theme(legend.position = c(0.15, 0.9))

z


Data_dngdeath <- Data[which(Data$name == "Dengue Death 2022" | Data$name == "Dengue Death 2003-2021"),]
Data_dngdeath

#define colors
COLS = c("red","orange")
names(COLS) = c("Dengue Death 2022","Dengue Death 2003-2021")
###
a <- ggplot(Data_dngdeath) + 
  geom_line(aes(Month,value,colour=name,fill=name),size=1) +
  # change name of legend here 
  scale_fill_manual(name="group",values=COLS)+
  scale_color_manual(name="",values=COLS) + scale_x_discrete(limits = c("1", "2", "3", 
                                                                             "4", "5", "6", 
                                                                             "7", "8", "9", 
                                                                             "10", "11", "12"),
                                                                  labels = c("Jan", "Feb", 
                                                                             "Mar", "Apr", "May", 
                                                                             "Jun", "Jul",
                                                                             "Aug", "Sep", 
                                                                             "Oct", "Nov", "Dec"))+  ylab("Dengue Deaths") + 
  xlab("Months") + ggtitle("") + theme(plot.title = element_text(size = 18, face = "bold"),
                                       legend.title=element_text(size=18), 
                                       legend.text=element_text(size=18),
                                       text = element_text(size = 20)) #+ theme(legend.position = c(0.15, 0.9))

a

tiff("plot.tiff", units="in", width=12, height=12, res=300)
gridExtra::grid.arrange(x,z,a)
dev.off()

g <- gridExtra::arrangeGrob(x,z,a) #generates g
ggsave(file="Dengue.pdf", g,width = 12, height = 12, dpi = 300, units = "in") #saves g

Dengue_03_21_DC <- aggregate(Dengue_03_21$DC, list(Dengue_03_21$Month), FUN=mean)
colnames(Dengue_03_21_DC) <- c("Month", "Dengue Case 2003-2021")
Dengue_03_21_DC
