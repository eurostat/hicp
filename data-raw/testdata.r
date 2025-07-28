# START

# Title:  Create and export test data
# Date:   22 July 2025
# Author: Sebastian Weinand

# load packages:
library(data.table)
library(hicp)
library(testthat)

# set country code for filtering:
GEO <- "EA"


# HICP --------------------------------------------------------------------


# load data: 
dtmp <- hicp::data(id="prc_hicp_midx", filter=list(unit="I15", geo=GEO)) # monthly price indices
dtmr <- hicp::data(id="prc_hicp_mmor", filter=list(geo=GEO)) # monthly rates of change
dtar <- hicp::data(id="prc_hicp_manr", filter=list(geo=GEO)) # annual rates of change
dtar12m <- hicp::data(id="prc_hicp_mv12r", filter=list(geo=GEO)) # 12-month average rates of change
dtctrb <- hicp::data(id="prc_hicp_ctrb") # contributions to EA inflation
dta <- hicp::data(id="prc_hicp_aind", filter=list(geo=GEO)) # annual average indices and rates of change
dtw <- hicp::data(id="prc_hicp_inw", filter=list(geo=GEO)) # annual item weights

# merge all monthly data sets:
dtm <- merge(x=dtmp[, list(coicop,geo,time,"index"=values)],
             y=dtmr[, list(coicop,geo,time,"mr"=values)],
             by=c("geo","coicop","time"),
             all=TRUE)

dtm <- merge(x=dtm, 
             y=dtar[, list(coicop,geo,time,"ar"=values)], 
             by=c("geo","coicop","time"),
             all=TRUE)

dtm <- merge(x=dtm, 
             y=dtar12m[, list(coicop,geo,time,"12mar"=values)], 
             by=c("geo","coicop","time"),
             all=TRUE)

dtm <- merge(x=dtm, 
             y=dtctrb[, list(coicop,geo,time,"ribe"=values)], 
             by=c("geo","coicop","time"),
             all=TRUE)

# monthly indices, change rates and contributions:
dtm[, "time":=as.Date(paste0(time, "-01"))]
dtm[, "year":=data.table::year(time)]
setcolorder(x=dtm, neworder=c("geo","coicop","year","time"))
setkeyv(x=dtm, cols=c("geo","coicop","time"))

# item weights:
dtw[, "time":=as.integer(time)]
setnames(x=dtw, old=c("time","values"), new=c("year","weight"))
setcolorder(x=dtw, neworder=c("geo","coicop","year"))
setkeyv(x=dtw, cols=c("geo","coicop","year"))

# annual average indices and change rates:
dta <- dcast(data=dta, formula=coicop+geo+time~unit, value.var="values")
setnames(x=dta, old=c("INX_A_AVG","RCH_A_AVG"), new=c("index","rate"))
dta[, "time":=as.Date(paste0(time, "-01-01"))]
setcolorder(x=dta, neworder=c("geo","coicop","time"))
setkeyv(x=dta, cols=c("geo","coicop","time"))

# export data:
fp <- "testdata"
dir.create(path=test_path(fp), showWarnings=FALSE)
save(dtm, file=test_path(fp, "dtm.RData"))
save(dta, file=test_path(fp, "dta.RData"))
save(dtw, file=test_path(fp, "dtw.RData"))


# OOHPI -------------------------------------------------------------------


# clear workspace:
rm(list=c("dtm","dta","dtw"))

# load data: 
dtp <- hicp::data(id="prc_hpi_ooq", filter=list(unit="I15_Q", geo=GEO)) # quarterly indices
dtqr <- hicp::data(id="prc_hpi_ooq", filter=list(unit="RCH_Q", geo=GEO)) # quarterly rates of change
dtar <- hicp::data(id="prc_hpi_ooq", filter=list(unit="RCH_A", geo=GEO)) # annual rates of change
dtw <- hicp::data(id="prc_hpi_ooinw", filter=list(geo=GEO)) # annual item weights

# merge all quarterly data sets:
dtq <- merge(x=dtp, 
             y=dtqr[, list(expend,geo,time,"qr"=values)], 
             by=c("geo","expend","time"),
             all=TRUE)

dtq <- merge(x=dtq, 
             y=dtar[, list(expend,geo,time,"ar"=values)], 
             by=c("geo","expend","time"),
             all=TRUE)

# indices:
dtq[, c("year","quarter") := tstrsplit(x=time, split="-Q", fixed=TRUE)]
dtq[, "year":=as.integer(year)]
dtq[, "quarter":=as.integer(quarter)]
dtq[, "time":=as.Date(paste(year, quarter*3, "01", sep="-"), format="%Y-%m-%d")]
dtq[, c("unit","quarter"):=NULL]
setnames(x=dtq, old="values", new="index")
setcolorder(x=dtq, neworder=c("geo","expend","year","time"))
setkeyv(x=dtq, cols=c("geo","expend","time"))

# item weights:
dtw[, "year":=as.integer(time)]
dtw[, c("unit","time"):=NULL]
setnames(x=dtw, old="values", new="weight")
setcolorder(x=dtw, neworder=c("geo","expend","year"))
setkeyv(x=dtw, cols=c("geo","expend","year"))

# merge price indices and item weights:
dtooh <- merge(x=dtq, y=dtw, by=c("geo","expend","year"), all.x=TRUE)

# export data:
save(dtooh, file=test_path(fp, "dtooh.RData"))

# END