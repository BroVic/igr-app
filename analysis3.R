# analysis3.R
library(zoo)

data2014 <- readRDS("Revenue2014_cleaned.rds")
data2015 <- readRDS("Revenue2015_cleaned.rds")

lapply(list(data2014, data2015), colnames)

data2yr <- rbind(data2014, data2015[-1, ])
rm(data2014, data2015)

dim(data2yr)
str(data2yr)
summary(data2yr)

#######################################################################
# Data for the 2 years are made a time-series with monthly intervals. #
# This is done after creating a new column for month-year, and the    #
# removal of missing values.                                          #
#######################################################################
data2yr$month <- as.yearmon(data2yr$date)
data2yr$month <- as.factor(data2yr$month)
str(data2yr)
igr.time <- data2yr[, c(4, 7)]

apply(igr.time, 2, function(x) sum(is.na(x)))
Amelia::missmap(igr.time)
igr.time <- igr.time[complete.cases(igr.time), ]
apply(igr.time, 2, anyNA)                         # F == no more NAs

igr.time <- aggregate(igr.time, list(igr.time$month), mean)
colnames(igr.time)[1] <- "Month"
# igr.time$Month <- as.Date.ts(igr.time$Month)
# igr_ts <- read.zoo(igr.time[, 1:2], index.column = 1)
# index(igr_ts)
