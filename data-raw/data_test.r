# START

# Title:  Create and export test data
# Date:   24 October 2026
# Author: Sebastian Weinand

# load packages:
library(data.table)
library(hicp)
library(testthat)

# create path to store test data:
fp <- "testdata"
dir.create(path=test_path(fp), showWarnings=FALSE)

# set filters:
GEO1 <- "EA" # country code
GEO2 <- c("EA", european.agg("EA")) # country codes for EA aggregate
COICOP <- c("TOTAL", paste0("CP", formatC(1:13, width=2, flag="0"))) # COICOP codes for EA aggregate
TIME1 <- c("2014-12","2019-12") # time range for HICP (ECOICOP ver. 1)
TIME2 <- c("2019-12","2026-06") # time range for HICP (ECOICOP ver. 2)
TIME3 <- c("2014-10","2026-06") # time range for OOHPI


# HICP (ECOICOP ver. 1) ---------------------------------------------------


# set options:
options(hicp.coicop.version="ecoicop1.hicp")
options(hicp.coicop.prefix="CP")
options(hicp.all.items.code="CP00")

# load data: 
dtmp <- hicp::data(id="prc_hicp_midx", date.range=TIME1, filters=list(unit="I15", geo=GEO1)) # monthly price indices
dtmr <- hicp::data(id="prc_hicp_mmor", date.range=TIME1, filters=list(geo=GEO1)) # monthly rates of change
dtar <- hicp::data(id="prc_hicp_manr", date.range=TIME1, filters=list(geo=GEO1)) # annual rates of change
dtar12m <- hicp::data(id="prc_hicp_mv12r", date.range=TIME1, filters=list(geo=GEO1)) # 12-month average rates of change
dtctrb <- hicp::data(id="prc_hicp_ctrb", date.range=TIME1, filters=list(geo=GEO1)) # contributions to EA inflation
dta <- hicp::data(id="prc_hicp_aind", date.range=substr(TIME1, 1, 4), filters=list(geo=GEO1)) # annual average indices and rates of change
dtiw <- hicp::data(id="prc_hicp_inw", date.range=substr(TIME1, 1, 4), filters=list(geo=GEO1)) # annual item weights

# merge all monthly data sets:
dthicp1m <- CJ("geo"=unique(GEO1), "coicop"=unique(dtiw$coicop), "time"=unique(dtmp$time))
dthicp1m[dtmp, "index":=i.values, on=c("geo","coicop","time")]
dthicp1m[dtmr, "mr":=i.values, on=c("geo","coicop","time")]
dthicp1m[dtar, "ar":=i.values, on=c("geo","coicop","time")]
dthicp1m[dtar12m, "12mar":=i.values, on=c("geo","coicop","time")]
dthicp1m[dtctrb, "ribe":=i.values, on=c("geo","coicop","time")]
dthicp1m[, "time":=as.Date(paste0(time, "-01"))]
dthicp1m[, "year":=data.table::year(time)]

# add item weights:
dtiw[, "year":=as.integer(time)]
dthicp1m[dtiw, "coicop_weight":=i.values, on=c("geo","coicop","year")]

# subset data:
dthicp1m <- dthicp1m[level(coicop)<=4L | coicop%in%unlist(special.agg(c("FOOD","NRG"))) | coicop%in%c("FOOD","NRG","FUEL","TOT_X_NRG_FOOD","TOT_X_FUEL"), ]
setcolorder(x=dthicp1m, neworder=c("geo","year","coicop","coicop_weight","time","index","ar","mr","12mar"))
setkeyv(x=dthicp1m, cols=c("geo","coicop","time"))

# annual average indices and change rates:
dthicp1a <- dcast(data=dta, formula=coicop+geo+time~unit, value.var="values")
setnames(x=dthicp1a, old=c("INX_A_AVG","RCH_A_AVG"), new=c("index","ar"))
dthicp1a[, "time":=as.Date(paste0(time, "-01-01"))]
dthicp1a <- dthicp1a[level(coicop)<=4L | coicop%in%unlist(special.agg(c("FOOD","NRG"))) | coicop%in%c("FOOD","NRG","FUEL","TOT_X_NRG_FOOD","TOT_X_FUEL"),]
setcolorder(x=dthicp1a, neworder=c("geo","coicop","time"))
setkeyv(x=dthicp1a, cols=c("geo","coicop","time"))

# export data:
save(dthicp1m, file=test_path(fp, "dthicp1m.RData"))
save(dthicp1a, file=test_path(fp, "dthicp1a.RData"))
rm(list=setdiff(ls(), c("fp","GEO1","GEO2","COICOP","TIME1","TIME2","TIME3")))


# HICP (ECOICOP ver. 2) ---------------------------------------------------


# set options:
options(hicp.coicop.version="ecoicop2.hicp")
options(hicp.coicop.prefix="CP")
options(hicp.all.items.code="TOTAL")

# load data: 
dtm <- hicp::data(id="prc_hicp_minr", date.range=TIME2, filters=list(unit=c("I25","RCH_M","RCH_A","RCH_MV12MAVR"), geo=GEO1)) # monthly data
dtctrb <- hicp::data(id="prc_hicp_ctr", date.range=TIME2, filter=list(geo=GEO1)) # monthly contributions
dta <- hicp::data(id="prc_hicp_ainr", date.range=substr(TIME2, 1, 4), filters=list(unit=c("RCH_A_AVG","INX_A_AVG"), geo=GEO1)) # annual data
dtiw <- hicp::data(id="prc_hicp_iw", date.range=substr(TIME2, 1, 4), filters=list(geo=GEO1)) # annual item weights

# merge all monthly data sets:
dthicp2m <- CJ("geo"=unique(GEO1), "coicop18"=unique(dtiw$coicop18), "time"=unique(dtm$time))
dtm <- dcast(data=dtm, formula=geo+coicop18+time~unit, value.var="values")
setnames(x=dtm, old=c("I25","RCH_A","RCH_M","RCH_MV12MAVR"), new=c("index","ar","mr","12mar"))
dthicp2m[dtm, c("index","ar","mr","12mar"):=list(i.index,i.ar,i.mr,i.12mar), on=c("geo","coicop18","time")]
dthicp2m[dtctrb, "ribe":=i.values, on=c("geo","coicop18","time")]
dthicp2m[, "time":=as.Date(paste0(time, "-01"))]
dthicp2m[, "year":=data.table::year(time)]

# add item weights:
dtiw[, "year":=as.integer(time)]
dthicp2m[dtiw, "coicop18_weight":=i.values, on=c("geo","coicop18","year")]

# subset data:
dthicp2m <- dthicp2m[level(coicop18)<=4L | coicop18%in%unlist(special.agg(c("FOOD","NRG"))) | coicop18%in%c("FOOD","NRG","FUEL","TOT_X_NRG_FOOD","TOT_X_FUEL"), ]
setcolorder(x=dthicp2m, neworder=c("geo","year","coicop18","coicop18_weight","time","index","ar","mr","12mar"))
setkeyv(x=dthicp2m, cols=c("geo","coicop18","time"))

# annual average indices and change rates:
dthicp2a <- dcast(data=dta, formula=geo+coicop18+time~unit, value.var="values")
setnames(x=dthicp2a, old=c("INX_A_AVG","RCH_A_AVG"), new=c("index","ar"))
dthicp2a[, "time":=as.Date(paste0(time, "-01-01"))]
dthicp2a <- dthicp2a[level(coicop18)<=4L | coicop18%in%unlist(special.agg(c("FOOD","NRG"))) | coicop18%in%c("FOOD","NRG","FUEL","TOT_X_NRG_FOOD","TOT_X_FUEL"), ]
setcolorder(x=dthicp2a, neworder=c("geo","coicop18","time"))
setkeyv(x=dthicp2a, cols=c("geo","coicop18","time"))

# export data:
save(dthicp2m, file=test_path(fp, "dthicp2m.RData"))
save(dthicp2a, file=test_path(fp, "dthicp2a.RData"))
rm(list=setdiff(ls(), c("fp","GEO1","GEO2","COICOP","TIME1","TIME2","TIME3")))


# HICP (ECOICOP ver. 2) - Euro area aggregate -----------------------------


# import monthly price indices and annual item and country weights:
dtp <- hicp::data(id="prc_hicp_minr", date.range=c("2019-12", "2026-06"), filters=list(unit="I25", coicop18=COICOP, geo=GEO2))
dtiw <- hicp::data(id="prc_hicp_iw", date.range=substr(TIME2, 1, 4), filters=list(geo=GEO2, coicop18=COICOP))
dtcw <- hicp::data(id="prc_hicp_cw", date.range=substr(TIME2, 1, 4), filters=list(geo=GEO2, statinfo=c("COWEA")))

# combine data:
dthicp2mEA <- CJ("geo"=unique(GEO2), "coicop18"=unique(COICOP), "time"=unique(dtp$time))
dthicp2mEA[dtp, "index":=i.values, on=c("geo","coicop18","time")]
dthicp2mEA[, "time":=as.Date(paste0(time, "-01"))]
dthicp2mEA[, "year":=year(time)]
dtiw[, "year":=as.integer(time)]
dtcw[, "year":=as.integer(time)]
dthicp2mEA[dtiw, "coicop18_weight":=i.values, on=c("geo","coicop18","year")]
dthicp2mEA[dtcw, "geo_weight":=i.values, on=c("geo","year")]
setkeyv(x=dthicp2mEA, cols=c("geo","coicop18","time"))

# export data:
save(dthicp2mEA, file=test_path(fp, "dthicp2mEA.RData"))
rm(list=setdiff(ls(), c("fp","GEO1","GEO2","COICOP","TIME1","TIME2","TIME3")))


# OOHPI -------------------------------------------------------------------


# load data: 
dtq <- hicp::data(id="prc_hpi_ooq", date.range=TIME3, filters=list(unit=c("I25_Q","RCH_A","RCH_Q"), geo=GEO1)) # quarterly indices
dtiw <- hicp::data(id="prc_hpi_ooinw", date.range=substr(TIME3, 1, 4), filters=list(geo=GEO1)) # annual item weights

# merge all quarterly data sets:
dtooh <- CJ("geo"=unique(GEO1), "expend"=unique(dtiw$expend), "time"=unique(dtq$time))
dtq <- dcast(data=dtq, formula=geo+expend+time~unit, value.var="values")
setnames(x=dtq, old=c("I25_Q","RCH_A","RCH_Q"), new=c("index","ar","qr"))
dtooh[dtq, c("index","ar","qr"):=list(i.index,i.ar,i.qr), on=c("geo","expend","time")]
dtooh[, c("year","quarter") := tstrsplit(x=time, split="-Q", fixed=TRUE)]
dtooh[, "year":=as.integer(year)]
dtooh[, "quarter":=as.integer(quarter)]
dtooh[, "time":=as.Date(paste(year, quarter*3, "01", sep="-"), format="%Y-%m-%d")]
dtooh[, "quarter":=NULL]

# add item weights:
dtiw[, "year":=as.integer(time)]
dtooh[dtiw, "expend_weight":=i.values, on=c("geo","expend","year")]

# subset data:
setcolorder(x=dtooh, neworder=c("geo","expend","year","expend_weight","time"))
setkeyv(x=dtooh, cols=c("geo","expend","time"))

# export data:
save(dtooh, file=test_path(fp, "dtooh.RData"))

# END