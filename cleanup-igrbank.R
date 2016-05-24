# 

# Data Cleaning for Consolidated IGR 
library(tidyr)
library(dplyr)

raw_igrbank <- read.csv("igrbank.csv", stringsAsFactors = TRUE)
tbl_df(raw_igrbank)

# change column names
colnames(raw_igrbank) <- c("off", "mth", "cons.renew", "sale.guidl",
                             "sale.reg", "non.comp", "aud.cert",
                             "air.waste.tox", "lab", "emp.permit",  
                             "ueee", "refund", "no.wksp", "bids", "bts", "epr")
glimpse(raw_igrbank)


# reshape the dataframe to gather non-variable columns
igrbank <-  raw_igrbank %>%
  gather(revenue.cat, amount, cons.renew:epr)

tbl_df(igrbank)
rm(raw_igrbank)

# Change case of Offices where appropriate and reorder
head(igrbank$off)
igrbank$off <- factor(igrbank$off, labels =  c("Anambra", "Benue", "Borno",
                       "Cross River", "Ebonyi", "Ekiti", "Enugu", "Gombe",
                       "HQ", "Imo", "Kaduna", "Kano", "Katsina", "Kebbi",
                       "Kwara", "Lagos", "Nassarawa", "NCZH", "NEZH", "Niger",
                       "Ondo", "Osun", "Oyo", "Plateau", "Rivers", "SEZH",
                       "Sokoto", "SSZH"))
igrbank$off <- factor(igrbank$off, levels = c("HQ", "NCZH", "NEZH", "SEZH",
                                              "SSZH", "Anambra", "Benue",
                                              "Borno", "Cross River", "Ebonyi",
                                              "Ekiti","Enugu", "Gombe", "Imo",
                                              "Kaduna", "Kano", "Katsina",
                                              "Kebbi", "Kwara", "Lagos",
                                              "Nassarawa", "Niger", "Ondo",
                                              "Osun", "Oyo", "Plateau",
                                              "Rivers", "Sokoto"))
head(igrbank$off)

# Change some variable types and/or re-order
# Month
head(igrbank$mth)
igrbank$mth <- factor(igrbank$mth,
                        levels = c("January", "February", "March", "April",
                        "May", "June", "July", "August", "September",
                        "October", "November", "December"),
                        ordered = TRUE)
head(igrbank$mth)

# Revenue Category
str(igrbank$revenue.cat)
igrbank$revenue.cat <- factor(igrbank$revenue.cat,
                              labels = c("Consultant Renewal Fees",
                                         "Sales of National Guidelines",
                                         "Sales of Regulations",
                                         "Non-Compliance",
                                         "Audit Certification",
                                         "Air Quality & Waste & Toxic Permit",
                                         "Lab Analysis",
                                         "EMP Permit",
                                         "Registration/Renewal for Importation of UEEE",
                                         "In Lieu for Resignation/Refunds",
                                         "Consultant Workshop",
                                         "Bids",
                                         "BTS",
                                         "EPR Manual"))

glimpse(igrbank)

# Change 'amount' to integer
igrbank$amount <- as.integer(igrbank$amount)
typeof(igrbank$amount)

tbl_df(igrbank)

# save data for use with Shiny app
saveRDS(igrbank, file = "igr-app/data/igr-tidy.rds")
