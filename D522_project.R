
# FHFA HPI
# Aug 2021


# import data

# hpi <- read.csv("D:/0_0 Careers/2019/1910 UoNA/2107 Data 522/Project/HPI_master.csv")

# Clean all objects from the current workspace (R memory) 
rm(list=ls())

library(tidyverse)
#library(readxl)
urlfile= "https://raw.githubusercontent.com/wx2123/FHFA_HPI/master/HPI_master.csv"
hpi <- read_csv(url(urlfile))
#hpi <- read.csv("D:/Data/HPI_master.csv")

# explore the data
names(hpi)
dim(hpi)
head(hpi)
tail(hpi)
anyNA(hpi)
str(hpi)

summary(hpi)

library(dplyr)
hpi1 <- hpi %>%
  # mutate(year_quarter = paste(yr,'Q',period))
  mutate(year_quarter = yr + period/4.001)

# visualization
hpi2 <- hpi1 %>%
  filter(!is.na(index_sa), 
    hpi_flavor == 'purchase-only',
    place_name == 'New York-Jersey City-White Plains, NY-NJ (MSAD)')

plot(hpi2$year_quarter,hpi2$index_sa, type ='l',xlab = ' year & quarter', ylab = 'HPI',
     main = 'New York-Jersey City-White Plains, NY-NJ (MSAD)')
grid(NA, 6, lwd = 1) # grid only in y-direction


hpi3 <- hpi1 %>%
  filter(!is.na(index_sa), 
         hpi_flavor == 'purchase-only',
         place_name == 'Washington-Arlington-Alexandria, DC-VA-MD-WV (MSAD)')

plot(hpi3$year_quarter,hpi3$index_sa, type ='l',xlab = ' year & quarter', ylab = 'HPI',
     main = 'Washington-Arlington-Alexandria, DC-VA-MD-WV (MSAD)')
grid(NA, 6, lwd = 1) # grid only in y-direction


hpi4 <- hpi1 %>%
  filter(!is.na(index_sa), 
         hpi_flavor == 'purchase-only',
         place_name == 'Charlotte-Concord-Gastonia, NC-SC')

plot(hpi4$year_quarter,hpi4$index_sa, type ='l',xlab = ' year & quarter', ylab = 'HPI',
     main = 'Charlotte-Concord-Gastonia, NC-SC')
grid(NA, 6, lwd = 1) # grid only in y-direction


hpi5 <- hpi1 %>%
  filter(!is.na(index_sa), 
         hpi_flavor == 'purchase-only',
         hpi_type   == 'traditional',
         place_name == 'San Francisco-San Mateo-Redwood City, CA (MSAD)')

plot(hpi5$year_quarter,hpi5$index_sa, type ='l',xlab = ' year & quarter', ylab = 'HPI',
     main = 'San Francisco-San Mateo-Redwood City, CA (MSAD)')
grid(NA, 6, lwd = 1) # grid only in y-direction


# multiple lines in one chart
fhfa_hpi <-data.frame(hpi2$year_quarter,hpi2$index_sa,hpi3$index_sa,hpi4$index_sa,hpi5$index_sa)
                 
#dat <- matrix(runif(40,1,20),ncol=4) # make data
matplot(fhfa_hpi[1],fhfa_hpi[2:5], type = c("l"),lwd = 2,col = 1:4, xlab='Year', ylab='FHFA HPI') #plot
legend("topleft", legend = c('New York','Washington D.C.','Charlotte','San Fransisico'), col=1:4, pch=1) # optional legend
grid(NA, 6, lwd = 1) # grid only in y-direction

matplot(fhfa_hpi[1],fhfa_hpi[2:5], type = c("l"),lwd = 2,col = 1:4, xlab='日期', ylab='FHFA 房价指数') #plot
legend("topleft", legend = c('纽约','华盛顿','夏洛特','旧金山'), col=1:4, pch=1) # optional legend

install.packages("forecast")
library(forecast)

barplot(diff(fhfa_hpi[,2]), type = c("l"),ylab='', main='New York HPI Difference (Lag = 1)')
abline (a = 0 , b = 0)

barplot(diff(fhfa_hpi[,2],2,2), type = c("l"),ylab='', main='New York HPI Difference (Lag = 2)')
abline (a = 0 , b = 0)

# examine ACF and PACF of difftrenced ser ~s 
acf(diff (fhfa_hpi[,2],2,2), xaxp = c(0, 120, 4), lag.max=120, main= " " ) 
pacf(diff (fhfa_hpi[,2],2,2) , xaxp = c(0,120,4) , lag.max=120 , main='' )

arima_1 <- arima(fhfa_hpi[,2],order=c(0,2,0), seasonal=list(order=c(1,0,0),period=12))
arima_1

arima_2 <- arima(fhfa_hpi[,2],order=c(0,2,1), seasonal=list(order=c(1,0,0),period=12))
arima_2

barplot(arima_2$residuals,ylab = "Residuals")
abline(a = 0, b = 0)

hist(arima_2$residuals, xlab = "Residuals", xlim = c(-15,15))

qqnorm(arima_2$residuals, main = "")
qqline(arima_2$residuals)

# predict the next 3 years(12 quarters)
arima_2.predict <- predict(arima_2,n.ahead=12)
m <- matrix(c(arima_2.predict$pred - 1.96 * arima_2.predict$se,
         arima_2.predict$pred,
         arima_2.predict$pred + 1.96 * arima_2.predict$se),12,3,
        dimnames = list(c(121:132), c("Low_Bound","Predict","Upper_Bound")))
m

plot(fhfa_hpi[,2], xlim=c(1,132),  xlab = "Time (quarters)", 
                   ylim=c(100,400),ylab='', main = "New York FHFA HPI",type = c("l") )

#plot(fhfa_hpi[,2],  xlab = "Time (quarters)", ylab = "NYC_FHFA_HPI" )
lines(arima_2.predict$pred)
lines(arima_2.predict$pred + 1.96 * arima_2.predict$se, col = 4, lty = 2)
lines(arima_2.predict$pred - 1.96 * arima_2.predict$se, col = 4, lty = 2)



         






matplot(fhfa_hpi[1],diff(fhfa_hpi[,2]))
legend("topleft", legend = c('New York')) # optional legend
grid(NA, 6, lwd = 1) # grid only in y-direction
