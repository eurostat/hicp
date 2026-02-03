# START

# Title:    Internal package data
# Author:   Sebastian Weinand
# Date:     31 January 2026

# load packages:
library(xml2)
library(openxlsx)
library(data.table)


# COICOP codes ------------------------------------------------------------


# sources:
# https://op.europa.eu/de/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/ecoicop
# https://op.europa.eu/de/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/ecoicop-hicp
# https://op.europa.eu/de/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/ecoicop2
# https://showvoc.op.europa.eu/#/datasets/ESTAT_European_Classification_of_Individual_Consumption_according_to_Purpose_%28ECOICOP%29/data
# https://unstats.un.org/unsd/classifications/Econ

# ECOICOP (version 1):
doc1 <- read_xml("http://data.europa.eu/ed1/ecoicop/ecoicop")
ids1 <- xml_attr(x=xml_find_all(doc1, xpath="//rdf:Description[skos:inScheme]"), attr="about")
# ids1 <- sort(sapply(X=strsplit(x=ids1, split="/", fixed=TRUE), FUN=function(z) z[length(z)]))
out1 <- vector(mode="list", length=length(ids1))
for(j in seq_along(out1)){
  cat("processing url", j, "/", length(out1), "\r")
  doctmp <- read_xml(x=ids1[j])
  out1[[j]] <- c(
    "id"=xml_text(xml_find_all(x=doctmp, xpath="//rdf:Description/dc:identifier")),
    "name"=xml_text(xml_find_all(x=doctmp, xpath="//rdf:Description/skos:altLabel[@xml:lang='en']")))
  Sys.sleep(runif(n=1, min=0.1, max=0.3))
}
dtecoicop1 <- data.table(do.call("rbind", out1))
Sys.sleep(time=5) # wait 5 seconds

# ECOICOP-HICP (version 1):
doc2 <- read_xml("http://data.europa.eu/ed1/ecoicop/ecoicop-hicp")
ids2 <- xml_attr(x=xml_find_all(doc2, xpath="//rdf:Description[skos:inScheme]"), attr="about")
add2 <- setdiff(ids2, ids1)
ids2 <- sort(sapply(X=strsplit(x=ids2, split="/", fixed=TRUE), FUN=function(z) z[length(z)]))
dtecoicop1.hicp <- dtecoicop1[id%in%ids2,]
if(length(add2)>0L){
  out2 <- vector(mode="list", length=length(add2))
  for(j in seq_along(out2)){
    cat("processing url", j, "/", length(out2), "\r")
    doctmp <- read_xml(x=add2[j])
    out2[[j]] <- c(
      "id"=xml_text(xml_find_all(x=doctmp, xpath="//rdf:Description/dc:identifier")),
      "name"=xml_text(xml_find_all(x=doctmp, xpath="//rdf:Description/skos:altLabel[@xml:lang='en']")))
    Sys.sleep(runif(n=1, min=0.1, max=0.3))
  }
  dtecoicop1.hicp <- rbind(dtecoicop1.hicp, do.call("rbind", out2))
}

# COICOP-1999
doc3 <- read.csv(
  file="https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/COICOP_english_structure.txt",
  header=TRUE, check.names=FALSE, encoding="latin1", sep=";")
doc3 <- strsplit(doc3[,1], "\\s{2,}")
doc3 <- doc3[-1] # remove code "01-12"
dtcoicop1999 <- data.table(
  "id"=sapply(doc3, "[[", 1L),
  "name"=sapply(doc3, "[[", 2L))
dtcoicop1999[, "id":=gsub(pattern="\\.", replacement="", x=id)]

# COICOP-2018:
doc4 <- as.data.table(openxlsx::read.xlsx("https://unstats.un.org/unsd/classifications/Econ/Download/COICOP_2018_English_structure.xlsx"))
doc4[, "code":=gsub(pattern="\\.", replacement="", x=code)]
dtcoicop2018 <- doc4[, list("id"=code,"name"=title)]

# ECOICOP (version 2):
doc5 <- read_xml("http://data.europa.eu/ed1/ecoicop2/ecoicop2")
ids5 <- xml_attr(x=xml_find_all(doc5, xpath="//rdf:Description[skos:inScheme]"), attr="about")
# ids5 <- sort(sapply(X=strsplit(x=ids5, split="/", fixed=TRUE), FUN=function(z) z[length(z)]))
out5 <- vector(mode="list", length=length(ids5))
for(j in seq_along(out5)){
  cat("processing url", j, "/", length(out5), "\r")
  doctmp <- read_xml(x=ids5[j])
  out5[[j]] <- c(
    "id"=xml_text(xml_find_all(x=doctmp, xpath="//rdf:Description/dc:identifier")),
    "name"=xml_text(xml_find_all(x=doctmp, xpath="//rdf:Description/skos:altLabel[@xml:lang='en']")))
  Sys.sleep(runif(n=1, min=0.1, max=0.3))
}
dtecoicop2 <- data.table(do.call("rbind", out5))
Sys.sleep(time=5) # wait 5 seconds

# no differences:
setdiff(dtecoicop2$id, grep(pattern="^(0|10|11|12|13)", x=dtcoicop2018[nchar(id)<=5, id], value=TRUE))

# ECOICOP-HICP (version 2):
doc6 <- read_xml("http://data.europa.eu/ed1/ecoicop2/ecoicop2-hicp")
ids6 <- xml_attr(x=xml_find_all(doc6, xpath="//rdf:Description[skos:inScheme]"), attr="about")
add6 <- setdiff(ids6, ids5)
ids6 <- sort(sapply(X=strsplit(x=ids6, split="/", fixed=TRUE), FUN=function(z) z[length(z)]))
dtecoicop2.hicp <- dtecoicop2[id%in%ids6,]
if(length(add6)>0L){
  out6 <- vector(mode="list", length=length(add6))
  for(j in seq_along(out6)){
    cat("processing url", j, "/", length(out6), "\r")
    doctmp <- read_xml(x=add6[j])
    out6[[j]] <- c(
      "id"=xml_text(xml_find_all(x=doctmp, xpath="//rdf:Description/dc:identifier")),
      "name"=xml_text(xml_find_all(x=doctmp, xpath="//rdf:Description/skos:altLabel[@xml:lang='en']")))
    Sys.sleep(runif(n=1, min=0.1, max=0.3))
  }
  dtecoicop2.hicp <- rbind(dtecoicop2.hicp, do.call("rbind", out6))
}

# combine:
dict.coicop <- rbindlist(
  l=list("coicop1999"=dtcoicop1999,
         "coicop2018"=dtcoicop2018,
         "ecoicop1"=dtecoicop1,
         "ecoicop1.hicp"=dtecoicop1.hicp,
         "ecoicop2"=dtecoicop2,
         "ecoicop2.hicp"=dtecoicop2.hicp),
  fill=TRUE, use.names=TRUE, idcol="version")
setcolorder(dict.coicop, neworder=c("version","id","name"))
setkeyv(dict.coicop, cols=c("version","id"))


# COICOP bundle codes -----------------------------------------------------


# bundle codes in ECOICOP ver. 1 (for HICP):
ecoicop1.bundle.defs <- list(
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

# get labels:
dtbdl1nm <- hicp::datafilters(id="prc_hicp_inw")
dtbdl1nm <- dtbdl1nm[concept=="coicop" & grepl(pattern="^CP.*[[:punct:]]", code), 
                     list("id"=gsub(pattern="^CP", replacement="", x=code), name)]
nm <- dtbdl1nm$name[match(x=names(ecoicop1.bundle.defs), table=dtbdl1nm$id)]
dict.bundles <- data.table(
  "version" = "ecoicop1.hicp",
  "id" = names(ecoicop1.bundle.defs),
  "name" = rep(x=nm[!is.na(nm)], each=2),
  "def" = ecoicop1.bundle.defs)
setcolorder(dict.bundles, neworder=c("version","id","name","def"))
setkeyv(dict.bundles, cols=c("version","id"))


# Special aggregates ------------------------------------------------------


# get names and codes of special aggregates:
doc <- read_xml("https://publications.europa.eu/resource/authority/x29/ecoicop_aggregates/sa")
aggurls <- xml_attr(x=xml_find_all(doc, xpath="//skos:hasTopConcept"), attr="resource")
aggid <- aggnm <- vector(mode="character", length=length(aggurls))
for(j in seq_along(aggurls)){
  
  cat("processing url", j, "/", length(aggurls), "\r")
  doctmp <- read_xml(x=aggurls[j])
  aggid[j] <- xml_text(xml_find_all(doctmp, xpath="//skos:notation"))
  aggnm[j] <- xml_text(xml_find_all(doctmp, xpath="//skos:prefLabel[@xml:lang='en']"))
  Sys.sleep(runif(n=1, min=0.1, max=0.3))
  
}

# get composition in ECOICOP ver. 1:
doc <- read_xml("http://data.europa.eu/x29/ecoicop_aggregates/SA_ECOICOP1")
aggurls <- xml_attr(x=xml_find_all(doc, xpath="//ns5:madeOf"), attr="resource")
agg1 <- vector(mode="list", length=length(aggurls))
agg1nm <- vector(mode="character", length(aggurls))
for(j in seq_along(aggurls)){
  
  cat("processing url", j, "/", length(aggurls), "\r")
  doctmp <- read_xml(x=aggurls[j])
  idtop <- xml_attr(xml_find_all(doctmp, xpath="//rdf:Description/ns2:sourceConcept"), attr="resource")
  agg1nm[j] <- sapply(X=strsplit(x=idtop, split="/", fixed=TRUE), FUN=function(z) z[length(z)])
  ids <- xml_attr(xml_find_all(doctmp, xpath="//rdf:Description/ns2:targetConcept"), attr="resource")
  agg1[[j]] <- sort(sapply(X=strsplit(x=ids, split="/", fixed=TRUE), FUN=function(z) z[length(z)]))
  Sys.sleep(runif(n=1, min=0.1, max=0.3))
  
}
names(agg1) <- agg1nm

# get composition in ECOICOP ver. 2:
doc <- read_xml("http://data.europa.eu/x29/ecoicop_aggregates/SA_ECOICOP2")
aggurls <- xml_attr(x=xml_find_all(doc, xpath="//ns5:madeOf"), attr="resource")
agg2 <- vector(mode="list", length=length(aggurls))
agg2nm <- vector(mode="character", length(aggurls))
for(j in seq_along(aggurls)){
  
  cat("processing url", j, "/", length(aggurls), "\r")
  doctmp <- read_xml(x=aggurls[j])
  idtop <- xml_attr(xml_find_all(doctmp, xpath="//rdf:Description/ns2:sourceConcept"), attr="resource")
  agg2nm[j] <- sapply(X=strsplit(x=idtop, split="/", fixed=TRUE), FUN=function(z) z[length(z)])
  ids <- xml_attr(xml_find_all(doctmp, xpath="//rdf:Description/ns2:targetConcept"), attr="resource")
  agg2[[j]] <- sort(sapply(X=strsplit(x=ids, split="/", fixed=TRUE), FUN=function(z) z[length(z)]))
  Sys.sleep(runif(n=1, min=0.1, max=0.3))
  
}
names(agg2) <- agg2nm

# combine:
dict.spec.aggs <- rbindlist(
  l=list(
    "ecoicop1.hicp"=data.table("id"=aggid, "name"=aggnm, "def"=agg1[match(x=aggid, table=names(agg1))]),
    "ecoicop2.hicp"=data.table("id"=aggid, "name"=aggnm, "def"=agg2[match(x=aggid, table=names(agg2))])),
  use.names=TRUE, 
  fill=TRUE, 
  idcol="version")
setcolorder(x=dict.spec.aggs, neworder=c("version","id","name","def"))
setkeyv(x=dict.spec.aggs, cols=c("version","id"))

# # special aggregates names:
# dtnm <- as.data.table(openxlsx::read.xlsx("ECOICOP_Aggregates_EN_FR_DE.xlsx"))
# 
# # special aggregates compositions in ECOICOP ver. 1:
# dtsa1 <- as.data.table(openxlsx::read.xlsx("ECOICOP1_Aggregates.xlsx"))
# dtsa1[, "Label_EN":=NULL]
# dtsa1[, "CODE":=gsub(pattern="\\.", replacement="", x=CODE)]
# dtsa1 <- melt(data=dtsa1, id.vars="CODE", variable.name="id", variable.factor=FALSE)
# dtsa1 <- dtsa1[value>0, list("def"=list(CODE)), by="id"]
# dtsa1[, "name" := dtnm$Name_EN[match(x=id, table=dtnm$Aggregate_Code)]]
# 
# # special aggregates compositions in ECOICOP ver. 2:
# dtsa2 <- as.data.table(openxlsx::read.xlsx("ECOICOP2_Aggregates.xlsx"))
# dtsa2[, "Label_EN":=NULL]
# dtsa2[, "CODE":=gsub(pattern="\\.", replacement="", x=CODE)]
# dtsa2 <- melt(data=dtsa2, id.vars="CODE", variable.name="id", variable.factor=FALSE)
# dtsa2 <- dtsa2[value>0, list("def"=list(CODE)), by="id"]
# dtsa2[, "name" := dtnm$Name_EN[match(x=id, table=dtnm$Aggregate_Code)]]
# 
# # combine:
# dict.spec.aggs <- rbindlist(l=list("ecoicop1.hicp"=dtsa1, "ecoicop2.hicp"=dtsa2), idcol="version")
# setcolorder(dict.spec.aggs, neworder=c("version","id","name","def"))
# setkeyv(dict.spec.aggs, cols=c("version","id"))


# Administered prices -----------------------------------------------------


# administered prices for ECOICOP ver. 1 (HICP):
dtap1 <- hicp::datafilters(id="prc_hicp_inw")
dtap1 <- dtap1[concept=="coicop" & grepl(pattern="AP", x=code), list("id"=code, name)]

# administered prices for ECOICOP ver. 2 (HICP):
dtap2 <- hicp::datafilters(id="prc_hicp_iw")
dtap2 <- dtap2[concept=="coicop18" & grepl(pattern="AP", x=code), list("id"=code, name)]

# combine:
dict.ap <- rbindlist(l=list("ecoicop1.hicp"=dtap1, "ecoicop2.hicp"=dtap2), idcol="version") 
setcolorder(dict.ap, neworder=c("version","id","name"))
setkeyv(dict.ap, cols=c("version","id"))


# European aggregates and countries ---------------------------------------


# see Annex 11.2 of the HICP Manual at
# https://ec.europa.eu/eurostat/de/web/products-manuals-and-guidelines/w/ks-gq-24-003

# countries with HICP data in protocol order:
dtcountries <- hicp::datafilters(id="prc_hicp_iw")
dtcountries <- dtcountries[concept=="geo" & !grepl(pattern=c("(EA|EU|EEA)(\\d{1,2})?"), x=code), list(code,name)]
dict.countries <- stats::setNames(dtcountries$name, dtcountries$code)

# Euro area floating composition:
dtea <- data.table(
  "code"="EA",
  "name"="Euro area",
  "country"=c(
    "BE","DE","FR","IT","LU",
    "NL","IE","ES","AT","PT",
    "FI","EL","SI","CY","MT",
    "SK","EE","LV","LT","HR",
    "BG"),
  "from"=as.Date(c(
    NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,
    NA,"2001-01-01","2007-01-01","2008-01-01","2008-01-01",
    "2009-01-01","2011-01-01","2014-01-01","2015-01-01","2023-01-01",
    "2026-01-01")),
  "to"=as.Date(rep(x=NA, times=21))
)

# for foverlap(), we have to set upper and lower bounds to Inf:
dtea[is.na(from), "from":=as.Date(-Inf)]
dtea[is.na(to), "to":=as.Date(Inf)]
setkeyv(x=dtea, cols=c("country","from","to"))

# European Union floating composition:
dteu <- data.table(
  "code"="EU",
  "name"="European Union",
  "country"=c(
    "BE","DE","FR","IT","LU",
    "NL","DK","IE","UK","EL",
    "ES","PT","AT","FI","SE",
    "CY","CZ","EE","HU","LT",
    "LV","MT","PL","SK","SI",
    "BG","RO","HR"),
  "from"=as.Date(c(
    NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,
    NA,NA,"1995-01-01","1995-01-01","1995-01-01",
    "2004-05-01","2004-05-01","2004-05-01","2004-05-01","2004-05-01",
    "2004-05-01","2004-05-01","2004-05-01","2004-05-01","2004-05-01",
    "2007-01-01","2007-01-01","2013-07-01")),
  "to"=as.Date(c(
    NA,NA,NA,NA,NA,
    NA,NA,NA,"2020-01-31",NA,
    NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,
    NA,NA,NA))
)

# for foverlap(), we have to set upper and lower bounds to Inf:
dteu[is.na(from), "from":=as.Date(-Inf)]
dteu[is.na(to), "to":=as.Date(Inf)]
setkeyv(x=dteu, cols=c("country","from","to"))

# European Economic Area floating composition:
dteea <- data.table(
  "code"="EEA",
  "name"="European Economic Area",
  "country"=c(
    "BE","DE","FR","IT","LU",
    "NL","DK","IE","UK","EL",
    "ES","PT","AT","FI","SE",
    "IS","NO","CY","CZ","EE",
    "HU","LT","LV","MT","PL",
    "SK","SI","BG","RO","HR"
  ),
  "from"=as.Date(c(
    NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,
    NA,NA,"2004-05-01","2004-05-01","2004-05-01",
    "2004-05-01","2004-05-01","2004-05-01","2004-05-01","2004-05-01",
    "2004-05-01","2004-05-01","2007-01-01","2007-01-01","2013-07-01")),
  "to"=as.Date(c(
    NA,NA,NA,NA,NA,
    NA,NA,NA,"2020-01-31",NA,
    NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA))
)

# for foverlap(), we have to set upper and lower bounds to Inf:
dteea[is.na(from), "from":=as.Date(-Inf)]
dteea[is.na(to), "to":=as.Date(Inf)]
setkeyv(x=dteea, cols=c("country","from","to"))

# create dictionary:
dict.country.aggs <- list(
  "EA" = dtea,
  "EU" = dteu,
  "EEA" = dteea
)


# Data export -------------------------------------------------------------


# write to internal data:
usethis::use_data(
  dict.coicop, 
  dict.bundles, 
  dict.spec.aggs, 
  dict.ap,
  dict.countries, 
  dict.country.aggs, 
  overwrite=TRUE, 
  internal=TRUE)

# END