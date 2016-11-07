# cleaning.R
# Another pass at cleaning 2 datasets from 2014 and 2015 only
# and create a single dataset to be used for further analysis

library(stringr)
library(lubridate)

rev14 <- read.csv("revenue2014.csv", stringsAsFactors = FALSE,
                  na.strings = " ")

# 2014 data
str(rev14)
rev14 <- rev14[, -c(7:8)]
colnames(rev14) <- c("date", "payer", "purpose",
                     "amount", "pay.mode", "remark")
rev14 <- rev14[-1, ]
str(rev14)

# ----------
# CLEANING THE "DATE" FIELD IN THIS DATASET:
# Expand all single digit fields into double digits. 
# Then switch fields were the month-day field arrangement didn't fit 
# the formatting.
# Year entries that were wrong entries or use of 2-digits fixed.
rev14$date <- str_trim(rev14$date)
rev14$date
rev14$date <- gsub("^([0-9]?)/", "0\\1/", rev14$date) # single to double digit
rev14$date <- gsub("/([0-9]?)/", "/0\\1/", rev14$date) # ditto
head(rev14$date, 100)
rev14$date <- gsub("/14$|/2015|/2016|/2017|/2018", "/2014", rev14$date)
rev14$date

needReformat <- is.na(as.Date(rev14$date, format = "%m/%d/%Y")) 
rev14$date[needReformat]
rev14$date[needReformat] <- 
  gsub("^([0-9]{2})/([0-9]{2})", "\\2/\\1", rev14$date[needReformat]) # swapped
rev14$date <- as.Date(rev14$date, format = "%m/%d/%Y")
rev14$date
# ----------

rev14$purpose
rev14$purpose[rev14$purpose == "CONSULTANT WORKSHOP"] <-
  "Consultant Workshop"
rev14$purpose[grep("CONSULTANT", rev14$purpose)] <- "Consultant Fee"
rev14$purpose[grep("GUIDELINE", rev14$purpose)] <- "EPR Guideline"
rev14$purpose[grep("REGULATIONS", rev14$purpose)] <- "Regulations"
rev14$purpose[grep("COMP", rev14$purpose)] <- "Non-compliance"
rev14$purpose[grep("AUDIT", rev14$purpose)] <- "Audit Cert."
rev14$purpose[grep("QUALITY", rev14$purpose)] <- "AQMT Permit"
rev14$purpose[grep("LAB", rev14$purpose)] <- "Lab Analysis"
rev14$purpose[grep("EMP", rev14$purpose)] <- "EMP"
rev14$purpose[grep("UEE", rev14$purpose)] <- "UEEE"
rev14$purpose[grep("LIEU", rev14$purpose)] <- "In Lieu"
rev14$purpose[grep("BID", rev14$purpose)] <- "Bids"
levels(as.factor(rev14$purpose))
rev14$purpose[grep("uality", rev14$purpose)] <- "AQMT Permit"
rev14$purpose[grep("Registration", rev14$purpose)] <- "Consultant Fee"
rev14$purpose[grep("Audit Cert", rev14$purpose)] <- "Audit Cert."
rev14$purpose[grep("^PEN", rev14$purpose)] <- "Non-compliance"
rev14$purpose[grep("^Pen", rev14$purpose)] <- "Non-compliance"
rev14$purpose[grep("^Perm", rev14$purpose)] <- "AQMT Permit"
rev14$purpose[grep("^PERM", rev14$purpose)] <- "AQMT Permit"
rev14$purpose[grep("^Non", rev14$purpose)] <- "Non-compliance"
rev14$purpose[grep("^WASTE", rev14$purpose)] <- "AQMT Permit"
rev14$purpose[grep("^Waste", rev14$purpose)] <- "AQMT Permit"
rev14$purpose[grep("^VIO", rev14$purpose)] <- "Non-compliance"
rev14$purpose[grep("^VCIO", rev14$purpose)] <- "Non-compliance"
rev14$purpose[grep("REGU", rev14$purpose)] <- "Regulations"
rev14$purpose[grep("EAR", rev14$purpose)] <- "Audit Cert."
rev14$purpose[grep("NOISE", rev14$purpose)] <- "Regulations"
rev14$purpose[grep("ENVIRONMENTAL", rev14$purpose)] <- "EMP"
rev14$purpose[grep("gui", rev14$purpose)] <- "Audit Guideline"
rev14$purpose[grep("^EIA", rev14$purpose)] <- "Non-compliance"
rev14$purpose[grep("CERI", rev14$purpose)] <- "Consultant Fee"
rev14$purpose[grep("RENEWAL OF ACCR", rev14$purpose)] <- "Consultant Fee"
rev14$purpose[grep("^Environmental", rev14$purpose)] <- "EMP"
rev14$purpose[grep("^Renewal", rev14$purpose)] <- "Consultant Fee"
rev14$purpose[grep("REC", rev14$purpose)] <- "Miscellaneous"
rev14$purpose[grep("WASTE", rev14$purpose)] <- "AQMT"
rev14$purpose[rev14$purpose == "Consultant reg."] <- "Consultant Fee"
rev14$purpose[rev14$purpose == "Consultant Accredidation"] <- "Consultant Fee"
rev14$purpose[grep("^ADMIN", rev14$purpose)] <- "Non-compliance"
rev14$purpose[rev14$purpose == "BREAKAGE OF GOCT. SEAL"] <- "Non-compliance"
rev14$purpose[grep("lan$", rev14$purpose)] <- "EMP"
rev14$purpose[grep("EPR", rev14$purpose)] <- "EPR Guideline"
rev14$purpose[grep("^Sale", rev14$purpose)] <- "Regulations"
rev14$purpose[grep("RENEWAL", rev14$purpose)] <- "Consultant Fee"
rev14$purpose[grep("VIOLATION", rev14$purpose)] <- "Non-compliance"
rev14$purpose[grep("INE$", rev14$purpose)] <- "EPR Guideline"
rev14$purpose[rev14$purpose == "APPROVAL FEE"] <- "Miscellaneous"
rev14$purpose[rev14$purpose == "AQMT"] <- "AQMT Permit"
rev14$purpose[rev14$purpose == "CERT OF ACCREDITATION"] <- "Consultant Fee"
rev14$purpose[rev14$purpose == "ENV. MGT PLAN"] <- "EMP"
rev14$purpose[grep("CARD$", rev14$purpose)] <- "Miscellaneous"
rev14$purpose[rev14$purpose == "Replacement of ID card"] <- "Miscellaneous"
rev14$purpose[rev14$purpose == "REISSUANCE OF CERT."] <- "Miscellaneous"
rev14$purpose[rev14$purpose == "One month in liue"] <- "In Lieu"
rev14$purpose[rev14$purpose == "ONE MONTH IN LIUE"] <- "In Lieu"
rev14$purpose[grep("REG", rev14$purpose)] <- "Regulations"
rev14$purpose[rev14$purpose == "PET"] <- "Miscellaneous"
rev14$purpose[rev14$purpose == ""] <- "Unknown"

rev14$purpose <- as.factor(rev14$purpose)
str(rev14)

head(rev14$amount)
rev14$amount <- str_trim(rev14$amount)
rev14$amount <- gsub(pattern = ",+", replacement = "", rev14$amount)
rev14$amount <- gsub(".00$", replacement = "", rev14$amount)
rev14$amount <- round(as.numeric(rev14$amount))
str(rev14)

levels(as.factor(rev14$pay.mode))
rev14$pay.mode[grep("DRAFT", rev14$pay.mode)] <- "Bank Draft"
rev14$pay.mode[grep("draft", rev14$pay.mode)] <- "Bank Draft"
rev14$pay.mode[grep("TELL", rev14$pay.mode)] <- "Bank Teller"
rev14$pay.mode[grep("eller", rev14$pay.mode)] <- "Bank Teller"
rev14$pay.mode[rev14$pay.mode == ""] <- "Unknown"
str(rev14)

rev14$pay.mode <- as.factor(rev14$pay.mode)
str(rev14)

head(rev14$remark, n = 100)
levels(as.factor(rev14$remark))
rev14$remark[grep("^A", rev14$remark)] <- "Anambra"
rev14$remark[rev14$remark == "BENUE"] <- "Benue"
rev14$remark[rev14$remark == "BORNO"] <- "Borno"
rev14$remark[grep("^C", rev14$remark)] <- "Cross River"
rev14$remark[grep("^Eb|^EB", rev14$remark)] <- "Ebonyi"
rev14$remark[grep("^EK|^Ek|^EKITTI", rev14$remark)] <- "Ekiti"
rev14$remark[grep("^En|^EN|^EU", rev14$remark)] <- "Enugu"
rev14$remark[grep("^G", rev14$remark)] <- "Gombe"
rev14$remark[grep("^I", rev14$remark)] <- "Imo"
rev14$remark[grep("^KAD", rev14$remark)] <- "Kaduna"
rev14$remark[grep("^KAN|^Kan", rev14$remark)] <- "Kano"
rev14$remark[rev14$remark == "KATSINA"] <- "Katsina"
rev14$remark[grep("^KE|^ke", rev14$remark)] <- "Kebbi"
rev14$remark[grep("^kw|^KW|^Kw", rev14$remark)] <- "Kwara"
rev14$remark[grep("^L", rev14$remark)] <- "Lagos"
rev14$remark[grep("^NA|^Na", rev14$remark)] <- "Nassarawa"
rev14$remark[rev14$remark == "NIGER"] <- "Niger"
rev14$remark[grep("^ON|^On", rev14$remark)] <- "Ondo"
rev14$remark[grep("^os|^OS|^Os", rev14$remark)] <- "Osun"
rev14$remark[grep("^oy|^OY|^Oy", rev14$remark)] <- "Oyo"
rev14$remark[grep("^PL|^Pl", rev14$remark)] <- "Plateau"
rev14$remark[grep("^RI|^Ri", rev14$remark)] <- "Rivers"
rev14$remark[grep("^SO|^So", rev14$remark)] <- "Sokoto"
rev14$remark[grep("^SOUTH E|^South E|^SOUTHE", rev14$remark)] <- "SOUTH EAST"
rev14$remark[grep("SOUTH$", rev14$remark)] <- "SOUTH SOUTH"
rev14$remark[rev14$remark == ""] <- NA
rev14$remark[rev14$remark == "BANK DRAFT"] <- NA

rev14$remark <- as.factor(rev14$remark)
str(rev14)

# done cleaning 2014 data... now save
saveRDS(rev14, "Revenue2014_cleaned.rds")
dir()
rm(list = ls())

# 2015 data
rev15 <- read.csv("revenue2015.csv", stringsAsFactors = FALSE,
                  na.strings = " ")

str(rev15)
dim(rev15)
colnames(rev15)[1:10]
head(rev15$X.1)
head(rev15$X.1, n = 100)
rev15 <- rev15[, 1:7]
str(rev15)
rev15 <- rev15[, -1]
str(rev15)

colnames(rev15) <- c("date", "payer", "purpose",
                     "amount", "pay.mode", "remark")

rev15$date
rev15$date <- gsub("^([0-9]?)/", "0\\1/", rev15$date)
rev15$date <- gsub("2014$|2667$|215$", "2015", rev15$date) # fix bad entries
needReformat <- is.na(as.Date(rev15$date, format = "%m/%d/%Y"))
rev15$date[needReformat] <- 
  gsub("^([0-9]{2})/([0-9]{2})", "\\2/\\1", rev15$date[needReformat]) # swap
rev15$date <- as.Date(rev15$date, format = "%m/%d/%Y")
rev15$date

levels(as.factor(rev15$purpose))
rev15$purpose[grep("^W|^PERMIT", rev15$purpose)] <- "AQMT Permit"
rev15$purpose[grep("^REN|^ENV|^CONSULTANT REG", rev15$purpose)] <-
  "Consultant Fee"
rev15$purpose[grep("REG", rev15$purpose)] <- "Regulations"
rev15$purpose[grep("UEEE", rev15$purpose)] <- "UEEE"
rev15$purpose[grep("AIR", rev15$purpose)] <- "AQMT Permit"
rev15$purpose[grep("AUDIT", rev15$purpose)] <- "Audit Cert."
rev15$purpose[grep("^NON|^PEN", rev15$purpose)] <- "Non-compliance"
rev15$purpose[grep("^EMP", rev15$purpose)] <- "EMP"
rev15$purpose[grep("NVECP", rev15$purpose)]  <- "Miscellaneous"
rev15$purpose[grep("^ACC|WORK", rev15$purpose)] <- "Consultant Fee"

rev15[rev15$purpose == "FEES", ]
rev15$purpose[rev15$purpose == "FEES"] <- "AQMT Permit"

rev15$purpose[grep("PERMIT", rev15$purpose)] <- "AQMT Permit"
rev15$purpose[grep("^ID", rev15$purpose)] <- "Miscellaneous"
rev15$purpose[grep("^OA|^SE|^TR|^VE", rev15$purpose)] <- "Miscellaneous"
rev15$purpose[grep("^SALE", rev15$purpose)] <- "EPR Guideline"
rev15$purpose[grep("^RE", rev15$purpose)] <- "Regulations"

rev15[rev15$purpose == "PROCESSING FEE", ]
rev15$purpose[rev15$purpose == "PROCESSING FEE"] <- "UEEE"

rev15$purpose[rev15$purpose == "" | rev15$purpose == "TELLER"] <- NA

rev15$purpose <- as.factor(rev15$purpose)
str(rev15)

head(rev15$amount)
rev15$amount <- gsub(",+", replacement = "", rev15$amount)
head(rev15$amount); tail(rev15$amount)
rev15$amount <- gsub(".00$", replacement = "", rev15$amount)
head(rev15$amount); tail(rev15$amount)
rev15$amount <- str_trim(rev15$amount)
rev15$amount <- as.numeric(rev15$amount)
str(rev15)

levels(as.factor(rev15$pay.mode))
rev15$pay.mode[grep("\\bDR", rev15$pay.mode)] <- "Bank Draft"
rev15$pay.mode[grep("\\bTE", rev15$pay.mode)] <- "Bank Teller"
rev15$pay.mode[grep("[[:digit:]]", rev15$pay.mode)] <- "TSA"
rev15$pay.mode[rev15$pay.mode == "" | rev15$pay.mode == "EMP"] <- NA

rev15$pay.mode <- as.factor(rev15$pay.mode)
str(rev15)

levels(as.factor(rev15$remark))
rev15$remark[-grep("NORTH|SOUTH|HQ", rev15$remark)] <-
  str_to_title(rev15$remark[-grep("NORTH|SOUTH|HQ", rev15$remark)])
levels(as.factor(rev15$remark))

rev15$remark[grep("^C", rev15$remark)] <- "Cross River"
rev15$remark[rev15$remark == "Jos"] <- "Plateau"
rev15$remark[grep("^KAN|THW", rev15$remark)] <- "NORTH WEST"

rev15$remark <- str_trim(rev15$remark)
levels(as.factor(rev15$remark))

rev15$remark[rev15$remark == "Nass"] <- "Nassarawa"
rev15$remark[grep("^Ph", rev15$remark)] <- "Rivers"
levels(as.factor(rev15$remark))

rev15$remark[rev15$remark == "" | rev15$remark == "Ear"] <- NA
levels(as.factor(rev15$remark))

rev15$remark <- as.factor(rev15$remark)
str(rev15)

# cleaning done... save dataset
saveRDS(rev15, "Revenue2015_cleaned.rds")
dir()
rm(list = ls())
# END
