# sample script for Shiny app

library(tidyr)
library(dplyr)

# Load data
igrbank <- readRDS(file = "igr-app/data/igr-tidy.rds")
tbl_df(igrbank)


# Filtering "Month" and "Office"
igrbank2 <- igrbank %>%
  filter(off == "HQ", mth == "March")

# Plot a bar chart
names.arg <- levels(igrbank2$revenue.cat)
barplot(igrbank2$amount/1000,
        horiz = TRUE,
        names.arg = names.arg,
        axisnames = TRUE,
        xlab = "Amount Collected (NGN '000)")

# Filtering by "Office" for all months
igrbank3 <- igrbank %>%
  filter(off == "HQ", revenue.cat == "Bids")

names.arg <- levels(igrbank3$mth)
barplot(igrbank3$amount/1000,
        horiz = TRUE,
        names.arg = names.arg,
        axisnames = TRUE,
        xlab = "Amount Collected (NGN '000)")

#####################################################
## Build code that will allow you to total payments for an office per month

# Trying it out for one station
igr.per.station <- igrbank %>%
  filter(off == "HQ", mth == "January")
sum(igr.per.station$amount)

# Write a function that allows you to add up the payments at an office/month
local.rev <- function (x, office = character()){
  df <- x[x$off == office, ]
  ans <- rep(NA, 12)
  
  levels(df$mth) <- 1:12
  for (i in 1:12) {
    dm <- df[df$mth == i, ]
    ans[i] <- sum(dm$amount)
  }
  ans
}
#####################################################

