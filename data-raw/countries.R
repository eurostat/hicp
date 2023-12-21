# START

# Title:    Country metadata and EA/EU compositions
# Author:   Sebastian Weinand
# Date:     21 December 2023

# load packages:
library(openxlsx)
library(data.table)

# countries:
countries <- as.data.table(read.xlsx("data-raw/countries.xlsx", detectDates=TRUE))
vars <- c("is_eu","is_ea","is_efta","is_candidate")
countries[, (vars) := lapply(.SD, as.logical), .SDcols=vars]

# subset to countries producing or produced the HICP:
countries <- countries[!is.na(index_decimals), ]

# # EU composition of the reference period:
# EU <- list(
#   "code"="EU",
#   "name_en"="European Union",
#   "name_fr"="Union européenne",
#   "name_de"="Europäische Union",
#   "index_decimals"=2L,
#   "composition"=countries[is_eu==TRUE, code])
# 
# # euro area composition of the reference period:
# EA <- list(
#   "code"="EA",
#   "name_en"="Euro area",
#   "name_fr"="Zone euro",
#   "name_de"="Euroraum",
#   "index_decimals"=2L,
#   "composition"=countries[is_ea==TRUE, code])

usethis::use_data(countries, overwrite=TRUE)

# END