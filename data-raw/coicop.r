# START

# Title:    COICOP codes
# Author:   Sebastian Weinand
# Date:     19 July 2024

# see:
# https://op.europa.eu/de/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/ecoicop
# https://op.europa.eu/de/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/ecoicop-hicp
# https://showvoc.op.europa.eu/#/datasets/ESTAT_European_Classification_of_Individual_Consumption_according_to_Purpose_%28ECOICOP%29/data
# https://unstats.un.org/unsd/classifications/Econ

# load packages:
library(xml2)
library(openxlsx)
library(data.table)

# ECOICOP:
doc1 <- read_xml("http://data.europa.eu/ed1/ecoicop/ecoicop")
ecoicop <- xml_attr(x=xml_find_all(doc1, xpath="//rdf:Description[skos:inScheme]"), attr="about")
ecoicop <- sort(sapply(X=strsplit(x=ecoicop, split="/", fixed=TRUE), FUN=function(z) z[length(z)]))

# ECOICOP-HICP:
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

# combine:
coicop <- list("ecoicop"=ecoicop, 
               "ecoicop-hicp"=ecoicop.hicp,
               "coicop-1999"=coicop1999,
               "coicop-2018"=coicop2018)

# write to internal data:
usethis::use_data(coicop, overwrite=TRUE, internal=TRUE)

# END