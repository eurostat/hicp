# START

# set global options:
options(hicp.chatty=FALSE)


# Functions rate() and rates() --------------------------------------------


# 1) Monthly rates -------------------------------------------------------


# chronological time period:
expect_equal(
  rates(x=c(104,100,102,105,107),
        t=as.Date(c("2021-12-01","2022-01-01","2022-02-01","2022-03-01","2022-04-01")),
        type="month"),
  100*(c(NA, 100/104, 102/100, 105/102, 107/105)-1)
)

# with NAs:
expect_equal(
  rates(x=c(104,100,102,NA,107),
        t=as.Date(c("2021-12-01","2022-01-01","2022-02-01","2022-03-01","2022-04-01")),
        type="month"),
  100*(c(NA, 100/104, 102/100, NA, NA)-1)
)

# with gaps:
expect_equal(
  rates(x=c(104,100,102,107),
        t=as.Date(c("2021-12-01","2022-01-01","2022-02-01","2022-04-01")),
        type="month"),
  100*(c(NA, 100/104, 102/100, NA)-1)
)

# false time ordering:
expect_equal(
  rates(x=c(100,104,105,102,107),
        t=as.Date(c("2022-01-01","2021-12-01","2022-03-01","2022-02-01","2022-04-01")),
        type="month"),
  100*(c(100/104, NA, 105/102, 102/100, 107/105)-1)
)

# quarterly time frequency:
expect_equal(
  rates(x=c(104,100,102,105,107),
        t=as.Date(c("2022-01-01","2022-04-01","2022-07-01","2022-10-01","2023-01-01")),
        type="month"),
  rep(NA_real_,5)
)

# annual time frequency:
expect_equal(
  rates(x=c(104,100,102,105,107),
        t=as.Date(c("2021-12-01","2022-12-01","2023-12-01","2024-12-01","2025-12-01")),
        type="month"),
  rep(NA_real_, 5)
)


# 2) Quarterly rates ------------------------------------------------------


# chronological time period:
expect_equal(
  rates(x=c(104,100,102,105,107),
        t=as.Date(c("2022-01-01","2022-04-01","2022-07-01","2022-10-01","2023-01-01")),
        type="quarter"),
  100*(c(NA, 100/104, 102/100, 105/102, 107/105)-1)
)

# with NAs:
expect_equal(
  rates(x=c(104,100,102,NA,107),
        t=as.Date(c("2022-01-01","2022-04-01","2022-07-01","2022-10-01","2023-01-01")),
        type="quarter"),
  100*(c(NA, 100/104, 102/100, NA, NA)-1)
)

# with gaps:
expect_equal(
  rates(x=c(104,100,102,107),
        t=as.Date(c("2022-01-01","2022-04-01","2022-07-01","2023-01-01")),
        type="quarter"),
  100*(c(NA, 100/104, 102/100, NA)-1)
)

# false time ordering:
expect_equal(
  rates(x=c(100,104,105,102,107),
        t=as.Date(c("2022-04-01","2022-01-01","2022-10-01","2022-07-01","2023-01-01")),
        type="quarter"),
  100*(c(100/104, NA, 105/102, 102/100, 107/105)-1)
)

# monthly time frequency:
expect_equal(
  rates(x=c(104,100,102,105,107),
        t=as.Date(c("2021-12-01","2022-01-01","2022-02-01","2022-03-01","2022-04-01")),
        type="quarter"),
  100*(c(NA,NA,NA,105/104,107/100)-1)
)

# annual time frequency:
expect_equal(
  rates(x=c(104,100,102,105,107),
        t=as.Date(c("2021-12-01","2022-12-01","2023-12-01","2024-12-01","2025-12-01")),
        type="quarter"),
  rep(NA_real_, 5)
)


# 3) Annual rates ---------------------------------------------------------


# chronological time period:
expect_equal(
  rates(x=c(104,100,102,105,107),
        t=as.Date(c("2021-12-01","2022-12-01","2023-12-01","2024-12-01","2025-12-01")),
        type="year"),
  100*(c(NA, 100/104, 102/100, 105/102, 107/105)-1)
)

# with NAs:
expect_equal(
  rates(x=c(104,100,102,NA,107),
        t=as.Date(c("2021-12-01","2022-12-01","2023-12-01","2024-12-01","2025-12-01")),
        type="year"),
  100*(c(NA, 100/104, 102/100, NA, NA)-1)
)

# with gaps:
expect_equal(
  rates(x=c(104,100,102,107),
        t=as.Date(c("2021-12-01","2022-12-01","2023-12-01","2025-12-01")),
        type="year"),
  100*(c(NA, 100/104, 102/100, NA)-1)
)

# false time ordering:
expect_equal(
  rates(x=c(100,104,105,102,107),
        t=as.Date(c("2022-12-01","2021-12-01","2024-12-01","2023-12-01","2025-12-01")),
        type="year"),
  100*(c(100/104, NA, 105/102, 102/100, 107/105)-1)
)

# monthly time frequency:
expect_equal(
  rates(x=1:25,
        t=seq.Date(from=as.Date("2019-12-01"), to=as.Date("2021-12-01"), by="1 month"),
        type="year"),
  100*(c(rep(NA,12), (13:25)/(1:13))-1)
)

# quarterly time frequency:
expect_equal(
  rates(x=1:9,
        t=seq.Date(from=as.Date("2019-12-01"), to=as.Date("2021-12-01"), by="3 month"),
        type="year"),
  100*(c(rep(NA,4), (5:9)/(1:5))-1)
)


# 4) Change rate for specific period --------------------------------------


# chronological time period:
expect_equal(
  rate(x=c(104,100,102,105,107),
       t=as.Date(c("2021-12-01","2022-01-01","2022-02-01","2022-03-01","2022-04-01")),
       start=as.Date("2022-03-01"), end=as.Date("2022-04-01")),
  100*(107/105-1)
)

# with NAs:
expect_equal(
  rate(x=c(104,100,102,NA,107),
       t=as.Date(c("2021-12-01","2022-01-01","2022-02-01","2022-03-01","2022-04-01")),
       start=as.Date("2022-03-01"), end=as.Date("2022-04-01")),
  NA_real_
)

# with NAs:
expect_equal(
  rate(x=c(104,100,102,105,NA),
       t=as.Date(c("2021-12-01","2022-01-01","2022-02-01","2022-03-01","2022-04-01")),
       start=as.Date("2022-03-01"), end=as.Date("2022-04-01")),
  NA_real_
)

# with gaps:
expect_equal(
  rate(x=c(104,100,NA,105,107),
       t=as.Date(c("2021-12-01","2022-01-01","2022-02-01","2022-03-01","2022-04-01")),
       start=as.Date("2022-03-01"), end=as.Date("2022-04-01")),
  100*(107/105-1)
)

# with gaps:
expect_equal(
  rate(x=c(104,100,102,107),
       t=as.Date(c("2021-12-01","2022-01-01","2022-02-01","2022-04-01")),
       start=as.Date("2022-03-01"), end=as.Date("2022-04-01")),
  NA_real_
)

# false time ordering:
expect_equal(
  rate(x=c(100,104,105,102,107),
       t=as.Date(c("2022-01-01","2021-12-01","2022-03-01","2022-02-01","2022-04-01")),
       start=as.Date("2022-03-01"), end=as.Date("2022-04-01")),
  100*(107/105-1)
)

# start date outside time:
expect_equal(
  rate(x=c(104,100,102,105,107),
       t=as.Date(c("2021-12-01","2022-01-01","2022-02-01","2022-03-01","2022-04-01")),
       start=as.Date("2022-03-01"), end=as.Date("2099-04-01")),
  NA_real_
)

# end date outside time:
expect_equal(
  rate(x=c(104,100,102,105,107),
       t=as.Date(c("2021-12-01","2022-01-01","2022-02-01","2022-03-01","2022-04-01")),
       start=as.Date("1999-03-01"), end=as.Date("2022-04-01")),
  NA_real_
)


# Functions contribution() and contributions() ----------------------------


# 1)  HICP manual example -------------------------------------------------


# input:
t <- structure(c(16040, 16071, 16102, 16130, 16161, 16191, 16222,
                 16252, 16283, 16314, 16344, 16375, 16405, 16436, 16467, 16495,
                 16526, 16556, 16587, 16617, 16648, 16679, 16709, 16740, 16770,
                 16801, 16832, 16861, 16892, 16922, 16953, 16983, 17014, 17045,
                 17075, 17106, 17136), class = "Date")
x <- c(108.7, 108.67, 108.74, 108.36, 108.27, 108.2, 108.44, 108.19,
       107.59, 107.75, 106.79, 105.33, 101.85, 98.62, 100.17, 101.91,
       102.01, 102.96, 102.86, 102.14, 99.89, 98.2, 97.68, 97.67, 95.9,
       93.3, 92.07, 93.03, 93.1, 94.64, 96.25, 95.29, 94.29, 95.26,
       96.78, 96.61, 98.35)
x.all <- c(100.11, 98.99, 99.3, 100.23, 100.38, 100.27, 100.38, 99.72,
           99.84, 100.28, 100.22, 100.04, 99.94, 98.4, 99.03, 100.15, 100.39,
           100.61, 100.6, 99.96, 99.97, 100.19, 100.34, 100.19, 100.17,
           98.72, 98.88, 100.11, 100.15, 100.51, 100.68, 100.12, 100.21,
           100.6, 100.85, 100.76, 101.31)
w <- c(NA, 108.07, 108.07, 108.07, 108.07, 108.07, 108.07, 108.07,
       108.07, 108.07, 108.07, 108.07, 108.07, 106.06, 106.06, 106.06,
       106.06, 106.06, 106.06, 106.06, 106.06, 106.06, 106.06, 106.06,
       106.06, 97.4, 97.4, 97.4, 97.4, 97.4, 97.4, 97.4, 97.4, 97.4,
       97.4, 97.4, 97.4)
w.all <- c(NA, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000,
           1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000,
           1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000,
           1000, 1000, 1000, 1000)

# expected results:
ribe.expec <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -1.02529813948425,
                -0.866666250168824, -0.640222130696651, -0.619973954901979, -0.515104491704027,
                -0.548704450433906, -0.602525307493953, -0.776522663065849, -0.964384788894606,
                -0.923621450672416, -0.781068214027195, -0.619594501718212, -0.556492789126052,
                -0.84220466625092, -0.916077474262503, -0.917160195487289, -0.857698005505091,
                -0.684618915689184, -0.711747489267791, -0.579213481076454, -0.303897553214695,
                -0.095393755765013, -0.111760293495743, 0.248832116788319)

# kirchner.expec <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -1.01703815303061,
#                     -0.861586224084169, -0.641540104650217, -0.622734144979741, -0.518601552711475,
#                     -0.552332368477231, -0.602460846904482, -0.777862334822627, -0.968075371900026,
#                     -0.927715376510117, -0.783441339089728, -0.619594501718212, -0.553088222076527,
#                     -0.838171335760115, -0.91752382926534, -0.918835000771934, -0.861111320893986,
#                     -0.688028810561959, -0.711825702057316, -0.580058548982564, -0.305078414197261,
#                     -0.0964820271246105, -0.112710625635746, 0.248832116788319)

mr.expec <- c(NA, NA, 0.00703817051762693, -0.0380879340850519, -0.00893712532013638, -0.00694071029627297,
              0.023822826922964, -0.0247882510581222, -0.0598855509317528, 0.0159502861616301,
              -0.0952818051681585, -0.144994499148793, -0.346225163991441, -0.336351300932743, 
              0.163933055745136, 0.182857342496143, 0.0103915176817525, 0.098483411797569, 
              -0.0103440065185095, -0.0744842501983668, -0.234253562986313, -0.175932853650891, 
              -0.0540143185777169, -0.00103718406998985, -0.18385643054338, 
              -0.264066736183525, -0.126758761785896, 0.0987735808943386, 0.00711375005741807, 
              0.156439994023563, 0.162965107150613, -0.0970076646685763, -0.101614850500567, 
              0.0984778811211783, 0.153717612572737, -0.0171494834056512, 0.175686792059182)

# chronological time periods:
dt0 <- data.table(t, x, w, x.all, w.all)

expect_equal(
  dt0[, contributions(x=x, w=w, t=t, xagg=x.all, wagg=w.all, type="year")][14:37],
  ribe.expec[14:37]
)

expect_equal(
  dt0[, contributions(x=x, w=w, t=t, xagg=x.all, wagg=w.all, type="month")][3:37],
  mr.expec[3:37]
)

# false time ordering:
dt1 <- copy(dt0)
idx <- sample(1:length(t))
dt1 <- dt1[idx,]

expect_equal(
  dt1[, contributions(x=x, w=w, t=t, xagg=x.all, wagg=w.all, type="year")][order(idx)][14:37],
  ribe.expec[14:37]
)

expect_equal(
  dt1[, contributions(x=x, w=w, t=t, xagg=x.all, wagg=w.all, type="month")][order(idx)][3:37],
  mr.expec[3:37]
)

# with NAs:
dt2 <- copy(dt0)
idx <- c(3,34)
dt2[idx, "x":=NA]

expect_equal(
  dt2[, contributions(x=x, w=w, t=t, xagg=x.all, wagg=w.all, type="year")][14:37],
  ifelse(1:nrow(dt2)%in%c(idx,idx+12), NA, ribe.expec)[14:37]
)

expect_equal(
  dt2[, contributions(x=x, w=w, t=t, xagg=x.all, wagg=w.all, type="month")][3:37],
  ifelse(1:nrow(dt2)%in%c(idx,idx+1), NA, mr.expec)[3:37]
)

# with gaps:
dt3 <- copy(dt0)
idx <- c(3,34)
dt3 <- dt3[-idx]

expect_equal(
  dt3[, contributions(x=x, w=w, t=t, xagg=x.all, wagg=w.all, type="year")][14:37],
  replace(x=ribe.expec[!1:nrow(dt2)%in%idx], list=14, values=NA)[14:37]
)

expect_equal(
  dt3[, contributions(x=x, w=w, t=t, xagg=x.all, wagg=w.all, type="month")][3:37],
  replace(x=mr.expec[!1:nrow(dt2)%in%idx], list=c(3,33), values=NA)[3:37]
)


# 2) Check against own test data ------------------------------------------


# function to sample test data:
sample.data <- function(freq="month"){
  
  # frequency to integer:
  freq <- match.arg(arg=freq, choices=c("month","quarter","year","quarter_avg","year_avg"))
  if(freq%in%c("month","quarter_avg","year_avg")) k <- 12L
  if(freq=="quarter") k <- 4L
  if(freq=="year") k <- 1L
  np <- 12L/k
  
  # setup:
  Y <- 5 # number of years
  t <- seq.Date(from=as.Date("2020-12-07"), by=paste(np, "month"), length.out=Y*k+1)
  N <- c(3,2,2,1)
  Nall <- sum(N)
  
  # data:
  dt <- data.table(
    "time"=rep(x=t, times=Nall),
    "coicop"=rep(1:Nall, each=length(t)),
    "index"=runif(n=Nall*length(t), min=80, max=130))
  
  # introduce gaps in indices:
  # dt[coicop==1L & t==as.Date("2023-02-07"), "index":=NA]
  dt[coicop==4L & t<as.Date("2022-12-01"), "index":=NA]
  dt[coicop==5L & t<as.Date("2023-12-01"), "index":=NA]
  dt[coicop==6L & t>=as.Date("2022-01-01"), "index":=NA]
  dt[coicop==7L & t>=as.Date("2023-01-01"), "index":=NA]
  dt[coicop==8L & t>=as.Date("2022-01-01") & t<as.Date("2023-12-01"), "index":=NA]
  
  # rebase:
  dt[, "index" := rebase(x=index, t=time, t.ref="first", settings=list(chatty=FALSE)), by="coicop"]
  
  # unchain indices:
  dt[, "price_ratio" := unchain(x=index, t=time), by="coicop"]
  
  # weights:
  dt[, "year":=year(time)]
  dtw <- dt[month(time)==12L & !is.na(price_ratio), list(coicop, year)]
  dtw[, "weight":=runif(n=1, min=1, max=30), by=c("coicop","year")]
  dtw[, "weight" := weight/sum(weight), by="year"]
  dt <- merge(x=dt, y=dtw, by=c("coicop","year"), all.x=TRUE, sort=FALSE)
  dt[is.na(price_ratio) & time>="2021-01-01", "weight":=0]
  # dt[, sum(weight, na.rm=TRUE), by="time"]
  
  # aggregate, chain and rebase indices:
  dtagg <- dt[, list(
    "year"=year[1],
    "coicop"=0L, 
    "weight"=if(all(is.na(weight))){NA_real_}else{sum(weight, na.rm=TRUE)}, 
    "price_ratio"=laspeyres(x=price_ratio, w0=weight)), 
    by="time"]
  dtagg[, "index":=chain(x=price_ratio, t=time)]
  dtagg[, "index":=rebase(x=index, t=time, t.ref="first", settings=list(chatty=FALSE))]
  
  # add index values of price reference period and overall index:
  dtall <- rbindlist(l=list(dt,dtagg), use.names=TRUE)
  dtall[, "price_ratio" := NULL]
  dtall <- merge(x=dtall, y=dtall[month(time)==12L, list("index0"=index), by=list(coicop, "year"=year+1)], by=c("coicop","year"), all.x=TRUE)
  dtall <- merge(x=dtall, y=dtall[coicop==0L, list(time,index,index0,weight)], by="time", all.x=TRUE, suffixes=c("","_all"))
  
  if(freq=="quarter_avg"){
    
    # quarterly average indices:
    dtall <- dtall[, lapply(.SD, FUN=function(z) mean(x=z, na.rm=FALSE)), 
                   by=list(coicop,"time"=hicp:::pin.date(t=time, freq=4L)), 
                   .SDcols=c("year","index","weight","index0","index_all","index0_all","weight_all")]
    
  }
  
  if(freq=="year_avg"){
    
    # annual average indices:
    dtall <- dtall[, lapply(.SD, FUN=function(z) mean(x=z, na.rm=TRUE)), 
                   by=list(coicop,"time"=hicp:::pin.date(t=time, freq=1L)), 
                   .SDcols=c("year","index","weight","index0","index_all","index0_all","weight_all")]
    
  }
  
  # return data:
  dtall[, "year" := as.integer(year)]
  setcolorder(x=dtall, neworder=c("coicop","time","year","index","index0","weight","index_all","index0_all","weight_all"))
  setkeyv(x=dtall, cols=c("coicop","time"))
  return(dtall[])
  
}


# 2.1) Monthly data -------------------------------------------------------
dtm <- sample.data(freq="month")
# use 
# dtm[(is.na(index) | is.na(index0)) & time>="2021-01-01", "weight":=NA]
# to set zero weights to NA

# calculate contributions to annual change rates:
dtm[, "car1" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time), by="coicop"]
dtm[!is.na(index), "car2" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time), by="coicop"]
dtm[sample(1:.N), "car3" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time), by="coicop"]

expect_equal(
  dtm[coicop==0L, rates(index, time, type="year")][-c(1:12)],
  dtm[coicop>0L, sum(car1, na.rm=TRUE), by="time"]$V1[-c(1:12)]
)
expect_equal(dtm$car1, dtm$car3)
expect_true(all(abs(dtm[, car1-car2])<0.0001, na.rm=TRUE)) # except for NAs
# if we remove the rows containing missing indices from the data, there is 
# no place to store the contributions. hence, we can not expect that 
# the sum of contributions equals the overall change rate

# calculate contributions to quarterly change rates:
dtm[, "cqr1" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, type="quarter"), by="coicop"]
dtm[!is.na(index), "cqr2" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, type="quarter"), by="coicop"]
dtm[sample(1:.N), "cqr3" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, type="quarter"), by="coicop"]

expect_equal(
  dtm[coicop==0L, rates(index, time, type="quarter")][-c(1:3)],
  dtm[coicop>0L, sum(cqr1, na.rm=TRUE), by="time"]$V1[-c(1:3)]
)
expect_equal(dtm$cqr1, dtm$cqr3)
expect_true(all(abs(dtm[, cqr1-cqr2])<0.0001, na.rm=TRUE)) # except for NAs

# calculate contributions to monthly change rates:
dtm[, "cmr1" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, type="month"), by="coicop"]
dtm[!is.na(index), "cmr2" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, type="month"), by="coicop"]
dtm[sample(1:.N), "cmr3" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, type="month"), by="coicop"]

expect_equal(
  dtm[coicop==0L, rates(index, time, type="month")][-c(1)],
  dtm[coicop>0L, sum(cmr1, na.rm=TRUE), by="time"]$V1[-c(1)]
)
expect_equal(dtm$cmr1, dtm$cmr3)
expect_true(all(abs(dtm[, cmr1-cmr2])<0.0001, na.rm=TRUE)) # except for NAs

# calculate contributions to arbitrary time periods:
expect_equal(
  dtm[coicop==0L, rate(x=index, t=time, start=as.Date("2021-01-07"), end=as.Date("2023-05-07"))],
  sum(dtm[coicop>0L, contribution(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01")), by="coicop"]$V1, na.rm=TRUE)
)

expect_equal(
  dtm[coicop==0L, rate(x=index, t=time, start=as.Date("2021-01-07"), end=as.Date("2023-05-07"))],
  sum(dtm[sample(1:.N), ][coicop>0L, contribution(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01")), by="coicop"]$V1, na.rm=TRUE)
)

expect_equal(
  dtm[coicop>0L, as.list(contribution(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01"), settings=list(details=TRUE))), by="coicop"][order(coicop),],
  dtm[sample(1:.N),][coicop>0L, as.list(contribution(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01"), settings=list(details=TRUE))), by="coicop"][order(coicop),],
  ignore_attr=TRUE
)

# adjust all weights proportionally (contributions to aggregate below total)
af <- 4

# aggregate, chain and rebase indices:
dtagg <- dtm[coicop>0L, list("coicop"=0, "weight"=sum(weight/af, na.rm=TRUE), "ratio"=laspeyres(x=index/index0, w0=weight/af)), by="time"]
dtagg[, "index":=chain(x=ratio, t=time)]
# -> only difference is the sum of weights

# add all-items hicp:
dtm2 <- merge(x=dtm[, list(coicop,time,weight,index)], dtagg, by="time", all.x=TRUE, suffixes=c("","_all"))
dtm2[, "car" := contributions(x=index, w=weight/af, t=time, xagg=index_all, wagg=weight_all, type="year"), by="coicop"]

# check results:
expect_equal(
  dtm2[coicop==0L, rates(index, time, type="year")][-c(1:12)],
  dtm2[coicop>0L, sum(car, na.rm=TRUE), by="time"]$V1[-c(1:12)]
)

dtcomp <- merge(dtm[, list(coicop,time,car1)], dtm2, by=c("coicop","time"), all=TRUE)
expect_equal(dtcomp$car1, dtcomp$car)
rm(dtcomp)


# 2.2) Quarterly averages of monthly data ---------------------------------
dtqm <- sample.data(freq="quarter_avg")

# calculate contributions to annual average change rates:
dtqm[, "car1" := contributions(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time), by="coicop"]
dtqm[!is.na(index), "car2" := contributions(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time), by="coicop"]
dtqm[sample(1:.N), "car3" := contributions(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time), by="coicop"]

expect_equal(
  dtqm[coicop==0L, rates(index, time, type="year")][-c(1:4)],
  dtqm[coicop>0L, sum(car1, na.rm=TRUE), by="time"]$V1[-c(1:4)]
)
expect_equal(dtqm$car1, dtqm$car3)
expect_true(all(abs(dtqm[, car1-car2])<0.0001, na.rm=TRUE)) # except for NAs
# if we remove the rows containing missing indices from the data, there is 
# no place to store the contributions. hence, we can not expect that 
# the sum of contributions equals the overall change rate

# calculate contributions to quarterly average change rates:
dtqm[, "cqr1" := contributions(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time, type="quarter"), by="coicop"]
dtqm[!is.na(index), "cqr2" := contributions(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time, type="quarter"), by="coicop"]
dtqm[sample(1:.N), "cqr3" := contributions(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time, type="quarter"), by="coicop"]

expect_equal(
  dtqm[coicop==0L, rates(index, time, type="quarter")][-c(1)],
  dtqm[coicop>0L, sum(cqr1, na.rm=TRUE), by="time"]$V1[-c(1)]
)
expect_equal(dtqm$cqr1, dtqm$cqr3)
expect_true(all(abs(dtqm[, cqr1-cqr2])<0.0001, na.rm=TRUE)) # except for NAs

# calculate contributions to arbitrary time periods:
expect_equal(
  dtqm[coicop==0L, rate(x=index, t=time, start=as.Date("2021-03-01"), end=as.Date("2023-06-01"))],
  sum(dtqm[coicop>0L, contribution(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01")), by="coicop"]$V1, na.rm=TRUE)
)

expect_equal(
  dtqm[coicop==0L, rate(x=index, t=time, start=as.Date("2021-03-01"), end=as.Date("2023-06-01"))],
  sum(dtqm[sample(1:.N), ][coicop>0L, contribution(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01")), by="coicop"]$V1, na.rm=TRUE)
)

expect_equal(
  dtqm[coicop>0L, as.list(contribution(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01"), settings=list(details=TRUE))), by="coicop"][order(coicop),],
  dtqm[sample(1:.N),][coicop>0L, as.list(contribution(x=index, x0=index0, xagg=index_all,xagg0=index0_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01"), settings=list(details=TRUE))), by="coicop"][order(coicop),],
  ignore_attr=TRUE
)


# 2.3) Annual averages of monthly data ------------------------------------
dtam <- sample.data(freq="year_avg")

# calculate contributions to annual average change rates:
dtam[, "car1" := contributions(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time), by="coicop"]
dtam[!is.na(index), "car2" := contributions(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time), by="coicop"]
dtam[sample(1:.N), "car3" := contributions(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time), by="coicop"]

expect_equal(
  dtam[coicop==0L, rates(index, time, type="year")][-c(1)],
  dtam[coicop>0L, sum(car1, na.rm=TRUE), by="time"]$V1[-c(1)]
)
expect_equal(dtam$car1, dtam$car3)
expect_true(all(abs(dtam[, car1-car2])<0.0001, na.rm=TRUE)) # except for NAs
# if we remove the rows containing missing indices from the data, there is 
# no place to store the contributions. hence, we can not expect that 
# the sum of contributions equals the overall change rate

# calculate contributions to arbitrary time periods:
expect_equal(
  dtam[coicop==0L, rate(x=index, t=time, start=as.Date("2021-12-01"), end=as.Date("2023-12-01"))],
  sum(dtam[coicop>0L, contribution(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01")), by="coicop"]$V1, na.rm=TRUE)
)

expect_equal(
  dtam[coicop==0L, rate(x=index, t=time, start=as.Date("2021-12-01"), end=as.Date("2023-12-01"))],
  sum(dtam[sample(1:.N), ][coicop>0L, contribution(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01")), by="coicop"]$V1, na.rm=TRUE)
)

expect_equal(
  dtam[coicop>0L, as.list(contribution(x=index, x0=index0, xagg=index_all, xagg0=index0_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01"), settings=list(details=TRUE))), by="coicop"][order(coicop),],
  dtam[sample(1:.N),][coicop>0L, as.list(contribution(x=index, x0=index0, xagg=index_all,xagg0=index0_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01"), settings=list(details=TRUE))), by="coicop"][order(coicop),],
  ignore_attr=TRUE
)


# 2.4) Quarterly data -----------------------------------------------------
dtq <- sample.data(freq="quarter")

# calculate contributions to annual change rates:
dtq[, "car1" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time), by="coicop"]
dtq[!is.na(index), "car2" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time), by="coicop"]
dtq[sample(1:.N), "car3" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time), by="coicop"]

expect_equal(
  dtq[coicop==0L, rates(index, time, type="year")][-c(1:4)],
  dtq[coicop>0L, sum(car1, na.rm=TRUE), by="time"]$V1[-c(1:4)]
)
expect_equal(dtq$car1, dtq$car3)
expect_true(all(abs(dtq[, car1-car2])<0.0001, na.rm=TRUE)) # except for NAs
# if we remove the rows containing missing indices from the data, there is 
# no place to store the contributions. hence, we can not expect that 
# the sum of contributions equals the overall change rate

# calculate contributions to quarterly change rates:
dtq[, "cqr1" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, type="quarter"), by="coicop"]
dtq[!is.na(index), "cqr2" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, type="quarter"), by="coicop"]
dtq[sample(1:.N), "cqr3" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, type="quarter"), by="coicop"]

expect_equal(
  dtq[coicop==0L, rates(index, time, type="quarter")][-c(1)],
  dtq[coicop>0L, sum(cqr1, na.rm=TRUE), by="time"]$V1[-c(1)]
)
expect_equal(dtq$cqr1, dtq$cqr3)
expect_true(all(abs(dtq[, cqr1-cqr2])<0.0001, na.rm=TRUE)) # except for NAs

# calculate contributions to arbitrary time periods:
expect_equal(
  dtq[coicop==0L, rate(x=index, t=time, start=as.Date("2021-03-07"), end=as.Date("2023-06-07"))],
  sum(dtq[coicop>0L, contribution(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01")), by="coicop"]$V1, na.rm=TRUE)
)

expect_equal(
  dtq[coicop==0L, rate(x=index, t=time, start=as.Date("2021-03-07"), end=as.Date("2023-06-07"))],
  sum(dtq[sample(1:.N), ][coicop>0L, contribution(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01")), by="coicop"]$V1, na.rm=TRUE)
)

expect_equal(
  dtq[coicop>0L, as.list(contribution(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01"), settings=list(details=TRUE))), by="coicop"][order(coicop),],
  dtq[sample(1:.N),][coicop>0L, as.list(contribution(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01"), settings=list(details=TRUE))), by="coicop"][order(coicop),],
  ignore_attr=TRUE
)


# 2.5) Annual data --------------------------------------------------------
dta <- sample.data(freq="year")

# calculate contributions to annual change rates:
dta[, "car1" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time), by="coicop"]
dta[!is.na(index), "car2" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time), by="coicop"]
dta[sample(1:.N), "car3" := contributions(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time), by="coicop"]

expect_equal(
  dta[coicop==0L, rates(index, time, type="year")][-c(1)],
  dta[coicop>0L, sum(car1, na.rm=TRUE), by="time"]$V1[-c(1)]
)
expect_equal(dta$car1, dta$car3)
expect_true(all(abs(dta[, car1-car2])<0.0001, na.rm=TRUE)) # except for NAs
# if we remove the rows containing missing indices from the data, there is 
# no place to store the contributions. hence, we can not expect that 
# the sum of contributions equals the overall change rate

# calculate contributions to arbitrary time periods:
expect_equal(
  dta[coicop==0L, rate(x=index, t=time, start=as.Date("2021-12-07"), end=as.Date("2023-12-07"))],
  sum(dta[coicop>0L, contribution(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01")), by="coicop"]$V1, na.rm=TRUE)
)

expect_equal(
  dta[coicop==0L, rate(x=index, t=time, start=as.Date("2021-12-07"), end=as.Date("2023-12-07"))],
  sum(dta[sample(1:.N), ][coicop>0L, contribution(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01")), by="coicop"]$V1, na.rm=TRUE)
)

expect_equal(
  dta[coicop>0L, as.list(contribution(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01"), settings=list(details=TRUE))), by="coicop"][order(coicop),],
  dta[sample(1:.N),][coicop>0L, as.list(contribution(x=index, xagg=index_all, w=weight, wagg=weight_all, t=time, start=as.Date("2021-01-01"), end=as.Date("2023-05-01"), settings=list(details=TRUE))), by="coicop"][order(coicop),],
  ignore_attr=TRUE
)


# Comparison to published HICP ECOICOP ver. 1 data ------------------------


# load data:
load(test_path("testdata","dthicp1m.RData"))
load(test_path("testdata","dthicp1a.RData"))

# monthly change rates:
dthicp1m[, "mr_own":=rates(x=index, t=time, type="month"), by="coicop"]
dtcomp <- dthicp1m[!(is.na(mr_own) & !is.na(mr)),]
expect_equal(dtcomp$mr, dtcomp$mr_own, tolerance=0.1)

# annual change rates:
dthicp1m[, "ar_own":=rates(x=index, t=time, type="year"), by="coicop"]
dtcomp <- dthicp1m[!(is.na(ar_own) & !is.na(ar)),]
expect_equal(dtcomp$ar, dtcomp$ar_own, tolerance=0.1)

# 12-month moving average change rates:
dthicp1m[, "index_12mar":=convert(x=index, t=time, type="12mavg"), by="coicop"]
dthicp1m[, "12mar_own":=rates(x=index_12mar, t=time, type="year"), by="coicop"]
dtcomp <- dthicp1m[!(is.na(`12mar_own`) & !is.na(`12mar`)),]
expect_equal(dtcomp$`12mar`, dtcomp$`12mar_own`, tolerance=0.1)

# annual average rate of change:
dthicp1a[, "ar_own":=rates(x=index, t=time, type="year"), by=c("geo","coicop")]
dtcomp <- dthicp1a[!(is.na(ar_own) & !is.na(ar)),]
expect_equal(dtcomp$ar, dtcomp$ar_own, tolerance=0.1)

# Ribe contributions:
dthicp1m[dthicp1m[coicop=="CP00",], c("index_all","coicop_weight_all"):=list(i.index,i.coicop_weight), on=c("geo","time")]
dthicp1m[, "ribe_own" := contributions(x=index, w=coicop_weight, t=time, 
                                       xagg=index_all, wagg=coicop_weight_all,
                                       type="year"), 
         by=c("geo","coicop")]
dtcomp <- dthicp1m[!is.na(ribe) & !is.na(ribe_own), ]
expect_equal(dtcomp$ribe, dtcomp$ribe_own, tolerance=0.1)
# dthicp1m[!is.na(ribe) & is.na(ribe_own), ] # all in first 12 months
# dthicp1m[is.na(ribe) & !is.na(ribe_own), ] # only coicops that are not published


# Comparison to published HICP ECOICOP ver. 2 data ------------------------


# load data:
load(test_path("testdata","dthicp2m.RData"))
load(test_path("testdata","dthicp2a.RData"))

# monthly change rates:
dthicp2m[, "mr_own":=rates(x=index, t=time, type="month"), by="coicop18"]
dtcomp <- dthicp2m[!(is.na(mr_own) & !is.na(mr)),]
expect_equal(dtcomp$mr, dtcomp$mr_own, tolerance=0.1)

# annual change rates:
dthicp2m[, "ar_own":=rates(x=index, t=time, type="year"), by="coicop18"]
dtcomp <- dthicp2m[!(is.na(ar_own) & !is.na(ar)),]
expect_equal(dtcomp$ar, dtcomp$ar_own, tolerance=0.1)

# 12-month moving average change rates:
dthicp2m[, "index_12mar":=convert(x=index, t=time, type="12mavg"), by="coicop18"]
dthicp2m[, "12mar_own":=rates(x=index_12mar, t=time, type="year"), by="coicop18"]
dtcomp <- dthicp2m[!(is.na(`12mar_own`) & !is.na(`12mar`)),]
expect_equal(dtcomp$`12mar`, dtcomp$`12mar_own`, tolerance=0.1)

# annual average rate of change:
dthicp2a[, "ar_own":=rates(x=index, t=time, type="year"), by=c("geo","coicop18")]
dtcomp <- dthicp2a[!(is.na(ar_own) & !is.na(ar)),]
expect_equal(dtcomp$ar, dtcomp$ar_own, tolerance=0.1)

# Ribe contributions:
dthicp2m[dthicp2m[coicop18=="TOTAL",], c("index_all","coicop18_weight_all"):=list(i.index,i.coicop18_weight), on=c("geo","time")]
dthicp2m[, "ribe_own" := contributions(x=index, w=coicop18_weight, t=time, 
                                       xagg=index_all, wagg=coicop18_weight_all,
                                       type="year"), 
         by=c("geo","coicop18")]
dtcomp <- dthicp2m[!is.na(ribe) & !is.na(ribe_own), ]
expect_equal(dtcomp$ribe, dtcomp$ribe_own, tolerance=0.1)
# dthicp2m[!is.na(ribe) & is.na(ribe_own), ] # all in first 12 months
# dthicp2m[is.na(ribe) & !is.na(ribe_own), ] # only coicops that are not published


# Comparison to published OOH data ----------------------------------------


# load data:
load(test_path("testdata","dtooh.RData"))

# quarterly change rates:
dtooh[, "qr_own":=rates(x=index, t=time, type="month"), by="expend"]
dtcomp <- dtooh[!(is.na(qr_own) & !is.na(qr)),]
expect_equal(dtcomp$mr, dtcomp$mr_own, tolerance=0.1)

# annual change rates:
dtooh[, "ar_own":=rates(x=index, t=time, type="year"), by="expend"]
dtcomp <- dtooh[!(is.na(ar_own) & !is.na(ar)),]
expect_equal(dtcomp$ar, dtcomp$ar_own, tolerance=0.1)

# Ribe contributions:
dtooh[dtooh[expend=="TOTAL",], c("index_all","expend_weight_all"):=list(i.index,i.expend_weight), on=c("geo","time")]
dtooh[, "ribe_ar" := contributions(x=index, w=expend_weight, t=time, xagg=index_all, wagg=expend_weight_all, type="year"), by=c("geo","expend")]
dtooh[, "ribe_qr" := contributions(x=index, w=expend_weight, t=time, xagg=index_all, wagg=expend_weight_all, type="quarter"), by=c("geo","expend")]

# check sum of contributions for quarterly change rates:
dtcomp <- merge(x=dtooh[expend%in%c("DW_ACQ","DW_OWN"), list("qr_own"=sum(ribe_qr)), by="time"], y=dtooh[expend=="TOTAL", list(time,qr)], by="time")
dtcomp <- dtcomp[!is.na(qr_own), ]
expect_equal(dtcomp$qr, dtcomp$qr_own, tolerance=0.1)

# check sum of contributions for annual change rates:
dtcomp <- merge(x=dtooh[expend%in%c("DW_ACQ","DW_OWN"), list("ar_own"=sum(ribe_ar)), by="time"], y=dtooh[expend=="TOTAL", list(time,ar)], by="time")
dtcomp <- dtcomp[!is.na(ar_own), ]
expect_equal(dtcomp$ar, dtcomp$ar_own, tolerance=0.1)

# END
