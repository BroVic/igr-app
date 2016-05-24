############################################
# NESREA Internally Generated Revenue 2015 #
############################################

# DATA CLEANING
library(dplyr)
library(tidyr)
library(stringr)

# Load data
igr.raw <- read.csv("igr2015.csv", na.strings = "", stringsAsFactors = TRUE)
tbl_df(igr.raw)

# transform variable names
colnames(igr.raw) <- c("month", "date", "payer", "purpose",
                       "amount", "paymode", "source")

# fix date column
igr.raw$date <- lubridate::dmy(as.character(igr.raw$date))

# refine attributes of categorical variables
igr.raw$month <- factor(igr.raw$month, levels = c("January", "February", "March",
                        "April", "May", "June", "July", "August", "September",
                        "October", "November", "December"), ordered = TRUE)

igr.raw$payer <- as.character(igr.raw$payer)

# carry out corrections on 'purpose' variable
# first convert to character types
igr.raw$purpose <- as.character(igr.raw$purpose)

# replace NAs
igr.raw$purpose[is.na(igr.raw$purpose)] <- "NO DATA"
igr.raw$purpose
  
  #------------------------------------------------------
  # NOTE: Accepted revenue categories are: 
    # “Consultant Renewal Fees",
    # “Sales of National Guidelines",
    # “Sales of Regulations",
    # “Non-Compliance",
    # “Audit Certification",
    # “Air Quality & Waste & Toxic Permit",
    # "Lab Analysis",
    # "EMP Permit",
    # “Registration/Renewal for Importation of UEEE",
    # “In Lieu for Resignation/ Refunds",
    # “Consultant Workshop",
    # "Bids",
    # "BTS",
    # "EPR Manual"
  #------------------------------------------------------

## igr.raw$purpose[which(str_detect(igr.raw$purpose, "SALE"))]

## apply(igr.raw, 2, function(x) sum(is.na(x)))
