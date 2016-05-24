# Exploratory Analysis

# load packages and data
library(dplyr)
mydata <- readRDS("igr-app/data/igr-tidy.rds")

# View dataset
tbl_df(mydata)

# Descriptive statistics
plot(mydata)
summary(mydata)
hist(mydata$amount, breaks = 500)
mean(mydata$amount, trim = .05)
median(mydata$amount)
median(mydata$amount[mydata$amount != 0]) # zero values removed

# examine outliers/extreme values
max(mydata$amount)
which(mydata$amount == 79420000)
(maxamount <- mydata[c(1484, 1498), ])

# Review distribution without effect of outliers 
mydata_x <- mydata[-c(1484, 1498), ]
hist(mydata_x$amount, breaks = 500)
boxplot(mydata$amount)

# Take another look at large values
tail(sort(mydata$amount), n = 10L)
tail(sort(mydata$amount), n = 20L)

# introduce NAs
mydata$amount[mydata$amount == 0] <- NA

# Checking again
plot(mydata)
summary(mydata)
hist(mydata$amount, breaks = 500)
mean(mydata$amount, na.rm = TRUE, trim = .05)
median(mydata$amount, na.rm = TRUE)
boxplot(mydata$amount, main = "Amount Collected")
boxplot(log(mydata$amount),
            main = "log of Amount collected") # log distribution
hist(mydata_x$amount, breaks = 500)
table(mydata$amount)
