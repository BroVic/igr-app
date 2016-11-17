# analysis3.R

#######################################################################
# Data for the 2 years are made a time-series with monthly intervals. #
# This is done after creating a new column for month-year, and the    #
# removal of missing values.                                          #
#######################################################################

library(zoo)

# Combine both data frames
data2014 <- readRDS("Revenue2014_cleaned.rds")
data2015 <- readRDS("Revenue2015_cleaned.rds")
lapply(list(data2014, data2015), colnames)      # check column names
data2yr <- rbind(data2014, data2015[-1, ])
rm(data2014, data2015)
dim(data2yr)
str(data2yr)
summary(data2yr)

igr.time <- data2yr[, c(1, 4)]                 # extract figures & months

# process missing values
apply(igr.time, 2, function(x) sum(is.na(x)))
igr.time <- igr.time[complete.cases(igr.time), ]
apply(igr.time, 2, anyNA)                         # F == no more NAs

igr.time$month <- as.factor(as.yearmon(igr.time$date))
aggregate(igr.time, by = list(igr.time$month), sum)
read.zoo(igr.time, index.column = "date")


# igr.time <- aggregate(igr.time, list(igr.time$month), mean) # average
# colnames(igr.time)[1] <- "Month"
# igr.time$Month <- as.Date.ts(igr.time$Month)
# igr_ts <- read.zoo(igr.time[, 1:2], index.column = 1)
# index(igr_ts)
