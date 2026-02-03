# START

# Title:  Create and export test data
# Date:   1 February 2026
# Author: Sebastian Weinand

# load packages:
library(data.table)
library(hicp)
library(testthat)

# create path to store test data:
fp <- "testdata"
dir.create(path=test_path(fp), showWarnings=FALSE)

# set filters:
GEO <- "EA" # country code
TIME1 <- c("2014-12","2019-12") # time range for HICP (ECOICOP ver. 1)
TIME2 <- c("2019-12","2025-12") # time range for HICP (ECOICOP ver. 2)
TIME3 <- c("2014-10","2024-12") # time range for OOH


# HICP (ECOICOP ver. 1) ---------------------------------------------------


# set options:
options(hicp.coicop.version="ecoicop1.hicp")
options(hicp.coicop.prefix="CP")
options(hicp.all.items.code="CP00")

# load data: 
dtmp <- hicp::data(id="prc_hicp_midx", date.range=TIME1, filters=list(unit="I15", geo=GEO)) # monthly price indices
dtmr <- hicp::data(id="prc_hicp_mmor", date.range=TIME1, filters=list(geo=GEO)) # monthly rates of change
dtar <- hicp::data(id="prc_hicp_manr", date.range=TIME1, filters=list(geo=GEO)) # annual rates of change
dtar12m <- hicp::data(id="prc_hicp_mv12r", date.range=TIME1, filters=list(geo=GEO)) # 12-month average rates of change
dtctrb <- hicp::data(id="prc_hicp_ctrb", date.range=TIME1, filters=list(geo=GEO)) # contributions to EA inflation
dta <- hicp::data(id="prc_hicp_aind", date.range=c("2014","2019"), filters=list(geo=GEO)) # annual average indices and rates of change
dtiw <- hicp::data(id="prc_hicp_inw", date.range=c("2014","2019"), filters=list(geo=GEO)) # annual item weights

# merge all monthly data sets:
dtm1 <- merge(x=dtmp[, list(coicop,geo,time,"index"=values)],
              y=dtmr[, list(coicop,geo,time,"mr"=values)],
              by=c("geo","coicop","time"),
              all=TRUE)

dtm1 <- merge(x=dtm1, 
              y=dtar[, list(coicop,geo,time,"ar"=values)], 
              by=c("geo","coicop","time"),
              all=TRUE)

dtm1 <- merge(x=dtm1, 
              y=dtar12m[, list(coicop,geo,time,"12mar"=values)], 
              by=c("geo","coicop","time"),
              all=TRUE)

dtm1 <- merge(x=dtm1, 
              y=dtctrb[, list(coicop,geo,time,"ribe"=values)], 
              by=c("geo","coicop","time"),
              all=TRUE)

# monthly indices, change rates and contributions:
dtm1[, "time":=as.Date(paste0(time, "-01"))]
dtm1[, "year":=data.table::year(time)]

# item weights:
dtiw[, "time":=as.integer(time)]
setnames(x=dtiw, old=c("time","values"), new=c("year","coicop_weight"))

# merge indices and weights:
dthicp1m <- merge(x=dtm1, y=dtiw, by=c("geo","coicop","year"), all.x=TRUE)
dthicp1m <- dthicp1m[level(coicop)<=4L | coicop%in%unlist(spec.agg(c("FOOD","NRG"))) | coicop%in%c("FOOD","NRG","FUEL","TOT_X_NRG_FOOD","TOT_X_FUEL"), ]
setcolorder(x=dthicp1m, neworder=c("geo","year","coicop","coicop_weight","time","index","ar","mr","12mar"))
setkeyv(x=dthicp1m, cols=c("geo","coicop","time"))

# annual average indices and change rates:
dthicp1a <- dcast(data=dta, formula=coicop+geo+time~unit, value.var="values")
setnames(x=dthicp1a, old=c("INX_A_AVG","RCH_A_AVG"), new=c("index","ar"))
dthicp1a[, "time":=as.Date(paste0(time, "-01-01"))]
dthicp1a <- dthicp1a[level(coicop)<=4L | coicop%in%unlist(spec.agg(c("FOOD","NRG"))) | coicop%in%c("FOOD","NRG","FUEL","TOT_X_NRG_FOOD","TOT_X_FUEL"),]
setcolorder(x=dthicp1a, neworder=c("geo","coicop","time"))
setkeyv(x=dthicp1a, cols=c("geo","coicop","time"))

# export data:
save(dthicp1m, file=test_path(fp, "dthicp1m.RData"))
save(dthicp1a, file=test_path(fp, "dthicp1a.RData"))
rm(list=setdiff(ls(), c("fp","GEO","TIME1","TIME2","TIME3")))


# HICP (ECOICOP ver. 2) ---------------------------------------------------


# set options:
options(hicp.coicop.version="ecoicop2.hicp")
options(hicp.coicop.prefix="CP")
options(hicp.all.items.code="TOTAL")

# load data: 
dtm <- hicp::data(id="prc_hicp_minr", date.range=TIME2, filters=list(unit=c("I25","RCH_M","RCH_A","RCH_MV12MAVR"), geo=GEO)) # monthly data
dtctrb <- hicp::data(id="prc_hicp_ctr", date.range=TIME2, filter=list(geo=GEO)) # monthly contributions
dta <- hicp::data(id="prc_hicp_ainr", date.range=c("2019","2025"), filters=list(unit=c("RCH_A_AVG","INX_A_AVG"), geo=GEO)) # annual data
dtiw <- hicp::data(id="prc_hicp_iw", date.range=c("2019","2025"), filters=list(geo=GEO)) # annual item weights

# merge all monthly data sets:
dtm <- dcast(data=dtm, formula=geo+coicop18+time~unit, value.var="values")
setnames(x=dtm, old=c("I25","RCH_A","RCH_M","RCH_MV12MAVR"), new=c("index","ar","mr","12mar"))
dtm <- merge(x=dtm, y=dtctrb[, list(geo,coicop18,time,"ribe"=values)], by=c("geo","coicop18","time"), all=TRUE)
dtm[, "time":=as.Date(paste0(time, "-01"))]
dtm[, "year":=data.table::year(time)]

# item weights:
dtiw[, "time":=as.integer(time)]
dtiw[, "statinfo":=NULL]
setnames(x=dtiw, old=c("time","values"), new=c("year","coicop18_weight"))

# merge indices and weights:
dthicp2m <- merge(x=dtm, y=dtiw, by=c("geo","coicop18","year"), all.x=TRUE)
dthicp2m <- dthicp2m[level(coicop18)<=4L | coicop18%in%unlist(spec.agg(c("FOOD","NRG"))) | coicop18%in%c("FOOD","NRG","FUEL","TOT_X_NRG_FOOD","TOT_X_FUEL"), ]
setcolorder(x=dthicp2m, neworder=c("geo","year","coicop18","coicop18_weight","time","index","ar","mr","12mar"))
setkeyv(x=dthicp2m, cols=c("geo","coicop18","time"))

# annual average indices and change rates:
dthicp2a <- dcast(data=dta, formula=geo+coicop18+time~unit, value.var="values")
setnames(x=dthicp2a, old=c("INX_A_AVG","RCH_A_AVG"), new=c("index","ar"))
dthicp2a[, "time":=as.Date(paste0(time, "-01-01"))]
dthicp2a <- dthicp2a[level(coicop18)<=4L | coicop18%in%unlist(spec.agg(c("FOOD","NRG"))) | coicop18%in%c("FOOD","NRG","FUEL","TOT_X_NRG_FOOD","TOT_X_FUEL"), ]
setcolorder(x=dthicp2a, neworder=c("geo","coicop18","time"))
setkeyv(x=dthicp2a, cols=c("geo","coicop18","time"))

# export data:
save(dthicp2m, file=test_path(fp, "dthicp2m.RData"))
save(dthicp2a, file=test_path(fp, "dthicp2a.RData"))
rm(list=setdiff(ls(), c("fp","GEO","TIME1","TIME2","TIME3")))


# OOHPI -------------------------------------------------------------------


# load data: 
dtq <- hicp::data(id="prc_hpi_ooq", date.range=TIME3, filters=list(unit=c("I15_Q","RCH_A","RCH_Q"), geo=GEO)) # quarterly indices
dtiw <- hicp::data(id="prc_hpi_ooinw", date.range=c("2014","2024"), filters=list(geo=GEO)) # annual item weights

# merge all quarterly data sets:
dtq <- dcast(data=dtq, formula=geo+expend+time~unit, value.var="values")
setnames(x=dtq, old=c("I15_Q","RCH_A","RCH_Q"), new=c("index","ar","qr"))
dtq[, c("year","quarter") := tstrsplit(x=time, split="-Q", fixed=TRUE)]
dtq[, "year":=as.integer(year)]
dtq[, "quarter":=as.integer(quarter)]
dtq[, "time":=as.Date(paste(year, quarter*3, "01", sep="-"), format="%Y-%m-%d")]
dtq[, "quarter":=NULL]

# item weights:
dtiw[, "year":=as.integer(time)]
dtiw[, c("unit","time"):=NULL]
setnames(x=dtiw, old="values", new="expend_weight")

# merge price indices and item weights:
dtooh <- merge(x=dtq, y=dtiw, by=c("geo","expend","year"), all.x=TRUE)
setcolorder(x=dtooh, neworder=c("geo","expend","year","expend_weight","time"))
setkeyv(x=dtooh, cols=c("geo","expend","time"))

# export data:
save(dtooh, file=test_path(fp, "dtooh.RData"))

# END