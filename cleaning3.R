# cleaning.R
# Another pass at cleaning 2 datasets from 2014 and 2015 only
# and create a single dataset to be used for further analysis

rev14 <- read.csv("revenue2014.csv", stringsAsFactors = FALSE, na.strings = " ")
rev15 <- read.csv("revenue2015.csv", stringsAsFactors = FALSE, na.strings = " ")

# 2014 data
str(rev14)
rev14 <- rev14[, -c(7:8)]
colnames(rev14) <- c("date", "payer", "purpose", "amount", "pay.mode", "remark")
rev14 <- rev14[-1, ]
rev14$date <- as.Date(rev14$date, format = "%m/%d/%Y")

rev14$purpose
rev14$purpose[rev14$purpose == "CONSULTANT WORKSHOP"] <- "Consultant Workshop"
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
rev14$amount <- stringr::str_trim(rev14$amount)
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
