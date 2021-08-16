
# FHFA HPI
# Aug 2021
# new change

# import data
library(readxl)
#hpi <- read.csv("D:/0_0 Careers/2019/1910 UoNA/2107 Data 522/Project/HPI_master.csv")
hpi <- read.csv("D:/Data/HPI_master.csv")

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
  mutate(year_quarter = yr + 3 * period/10)

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

