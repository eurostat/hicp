# START

# Title:  Contributions to inflation - Comparison to example in HICP Manual
# Author: Sebastian Weinand
# Date:   2023-04-07

# load packages:
library(data.table)
library(openxlsx)
library(hicp)

# url to hicp manual example:
url <- "https://circabc.europa.eu/sd/a/588ac82c-abad-40c1-82e9-aeeabc0e99bb/HICP-Manual-Chapter-8-Numerical-example.xlsx"

# price indices:
prices <- as.data.table(read.xlsx(xlsxFile=url, sheet="Index", rows=5:42, cols=c(1,2,4:8)))
setnames(prices, c("time","index_total","proc_food", "unproc_food","nrg_goods","energy","services"))
prices[, "time":= as.Date(paste(time, "01", sep="-"), format="%YM%m-%d")]
prices$year <- format(prices$time, "%Y")
prices <- melt(data=prices, id.vars=c("time","year","index_total"), value.name="index", variable.name="coicop")

# item weights:
weights <- as.data.table(read.xlsx(xlsxFile=url, sheet="Item weights", rows = 5:8, cols=c(1,2,4:8)))
setnames(weights, c("year","weight_total","proc_food", "unproc_food","nrg_goods","energy","services"))
weights <- melt(data=weights, id.vars=c("year","weight_total"), value.name="weight", variable.name="coicop")

# merge data:
hicp.data <- merge(x=prices, y=weights, all.x=TRUE, by=c("year","coicop"), sort=FALSE)

# ribe decomposition for energy:
ribe.manual <- as.data.table(read.xlsx(xlsxFile=url, sheet="Annual rate", rows=5:42, cols=c(1,2,11:13), na.strings=":"))
setnames(ribe.manual, c("time","ar_total","this_year","last_year","ribe"))
ribe.manual[, "time":= as.Date(paste(time, "01", sep="-"), format="%YM%m-%d")]

# kirchner decomposition for energy:
kirchner.manual <- as.data.table(read.xlsx(xlsxFile=url, sheet="Annual rate", rows=5:42, cols=c(1,2,14:17), na.strings=":"))
setnames(kirchner.manual, c("time","ar_total","term1","term2","term3","kirchner"))
kirchner.manual[, "time":= as.Date(paste(time, "01", sep="-"), format="%YM%m-%d")]

# ribe decomposition:
hicp.data[, "ribe" := contrib(x=index, w=weight, t=time, x.all=index_total, w.all=weight_total, method="ribe"), by="coicop"]

# ribe decomposition:
hicp.data[, "kirchner" := contrib(x=index, w=weight, t=time, x.all=index_total, w.all=weight_total, method="kirchner"), by="coicop"]

# compare results:
ribe.comp <- merge(
  x=ribe.manual,
  y=hicp.data[coicop=="energy", list(time, ribe)],
  by="time",
  all=TRUE,
  suffixes=c("_manual","_own"))

ribe.comp[abs(ribe_manual-ribe_own)>0.0001,]

hicp.data[, sum(ribe), by="time"]

# compare results:
kirchner.comp <- merge(
  x=kirchner.manual,
  y=hicp.data[coicop=="energy", list(time, kirchner)],
  by="time",
  all=TRUE,
  suffixes=c("_manual","_own"))

kirchner.comp[abs(kirchner_manual-kirchner_own)>0.0001,]

hicp.data[, sum(kirchner), by="time"]

# END
