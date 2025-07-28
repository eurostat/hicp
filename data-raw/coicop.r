# START

# Title:    COICOP codes
# Author:   Sebastian Weinand
# Date:     22 July 2025

# see:
# https://op.europa.eu/de/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/ecoicop
# https://op.europa.eu/de/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/ecoicop-hicp
# https://op.europa.eu/de/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/ecoicop2
# https://showvoc.op.europa.eu/#/datasets/ESTAT_European_Classification_of_Individual_Consumption_according_to_Purpose_%28ECOICOP%29/data
# https://unstats.un.org/unsd/classifications/Econ

# load packages:
library(xml2)
library(openxlsx)
library(data.table)

# ECOICOP (version 1):
doc1 <- read_xml("http://data.europa.eu/ed1/ecoicop/ecoicop")
ecoicop <- xml_attr(x=xml_find_all(doc1, xpath="//rdf:Description[skos:inScheme]"), attr="about")
ecoicop <- sort(sapply(X=strsplit(x=ecoicop, split="/", fixed=TRUE), FUN=function(z) z[length(z)]))

# ECOICOP-HICP (version 1):
doc2 <- read_xml("http://data.europa.eu/ed1/ecoicop/ecoicop-hicp")
ecoicop.hicp <- xml_attr(x=xml_find_all(doc2, xpath="//rdf:Description[skos:inScheme]"), attr="about")
ecoicop.hicp <- sort(sapply(X=strsplit(x=ecoicop.hicp, split="/", fixed=TRUE), FUN=function(z) z[length(z)]))

# COICOP-1999
doc3 <- read.csv(
  file="https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/COICOP_english_structure.txt",
  header=TRUE, check.names=FALSE, encoding="latin1", sep=";")
doc3 <- strsplit(doc3[,1], "\\s{2,}")
doc3 <- doc3[-1] # remove code "01-12"
dt1999 <- data.table("code"=sapply(doc3, "[[", 1L), "title"=sapply(doc3, "[[", 2L))
dt1999[, "code":=gsub(pattern="\\.", replacement="", x=code)]
setorderv(x=dt1999, cols="code")
coicop1999 <- dt1999$code

# COICOP-2018:
dt2018 <- as.data.table(openxlsx::read.xlsx("https://unstats.un.org/unsd/classifications/Econ/Download/COICOP_2018_English_structure.xlsx"))
dt2018[, "code":=gsub(pattern="\\.", replacement="", x=code)]
dt2018 <- subset(dt2018, select=c("code","title"))
setorderv(x=dt2018, cols="code")
coicop2018 <- dt2018$code

# ECOICOP (version 2):
doc5 <- read_xml("http://data.europa.eu/ed1/ecoicop2/ecoicop2")
ecoicop2 <- xml_attr(x=xml_find_all(doc5, xpath="//rdf:Description[skos:inScheme]"), attr="about")
ecoicop2 <- sort(sapply(X=strsplit(x=ecoicop2, split="/", fixed=TRUE), FUN=function(z) z[length(z)]))

# no differences:
setdiff(ecoicop2, grep(pattern="^(0|10|11|12|13)", x=coicop2018[nchar(coicop2018)<=5], value=TRUE))

# combine:
coicop <- list("ecoicop"=ecoicop, 
               "ecoicop.hicp"=ecoicop.hicp,
               "ecoicop2"=ecoicop2,
               "coicop1999"=coicop1999,
               "coicop2018"=coicop2018)

# COICOP bundle codes (on Eurobase) and their components:
coicop.bundles <- list(
  "0531_0532" = c("0531", "0532"),
  "0531_2" = c("0531", "0532"),
  "0612_0613" = c("0612", "0613"),
  "0612_3" = c("0612", "0613"),
  "0621_0623" = c("0621", "0623"),
  "0621_3" = c("0621", "0623"),
  "0712-0714" = c("0712", "0713", "0714"),
  "0712_34" = c("0712", "0713", "0714"),
  "08X" = c("082", "083"),
  "082_083" = c("082", "083"),
  "0921_0922" = c("0921", "0922"),
  "0921_2" = c("0921", "0922"),
  "0934_0935" = c("0934", "0935"),
  "0934_5" = c("0934", "0935"),
  "0953_0954" = c("0953", "0954"),
  "0953_4" = c("0953", "0954"),
  "1212_1213" = c("1212", "1213"),
  "1212_3" = c("1212", "1213")
)

# write to internal data:
usethis::use_data(coicop, coicop.bundles, overwrite=TRUE, internal=TRUE)

# END