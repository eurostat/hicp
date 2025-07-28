# START

options("hicp.chatty"=FALSE)


# Function rates() --------------------------------------------------------


# (1) monthly rates

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

# (2) quarterly rates

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

# (3) annual rates

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


# Function contrib() ------------------------------------------------------


## check against example in hicp manual

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

kirchner.expec <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -1.01703815303061,
                    -0.861586224084169, -0.641540104650217, -0.622734144979741, -0.518601552711475,
                    -0.552332368477231, -0.602460846904482, -0.777862334822627, -0.968075371900026,
                    -0.927715376510117, -0.783441339089728, -0.619594501718212, -0.553088222076527,
                    -0.838171335760115, -0.91752382926534, -0.918835000771934, -0.861111320893986,
                    -0.688028810561959, -0.711825702057316, -0.580058548982564, -0.305078414197261,
                    -0.0964820271246105, -0.112710625635746, 0.248832116788319)

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
  dt0[, contrib(x=x, w=w, t=t, x.all=x.all, w.all=w.all, type="year", settings=list(method="ribe"))][14:37],
  ribe.expec[14:37]
)

expect_equal(
  dt0[, contrib(x=x, w=w, t=t, x.all=x.all, w.all=w.all, type="year", settings=list(method="kirchner"))][14:37],
  kirchner.expec[14:37]
)

expect_equal(
  dt0[, contrib(x=x, w=w, t=t, x.all=x.all, w.all=w.all, type="month")][3:37],
  mr.expec[3:37]
)

# false time ordering:
dt1 <- copy(dt0)
idx <- sample(1:length(t))
dt1 <- dt1[idx,]

expect_equal(
  dt1[, contrib(x=x, w=w, t=t, x.all=x.all, w.all=w.all, type="year", settings=list(method="ribe"))][order(idx)][14:37],
  ribe.expec[14:37]
)

expect_equal(
  dt1[, contrib(x=x, w=w, t=t, x.all=x.all, w.all=w.all, type="year", settings=list(method="kirchner"))][order(idx)][14:37],
  kirchner.expec[14:37]
)

expect_equal(
  dt1[, contrib(x=x, w=w, t=t, x.all=x.all, w.all=w.all, type="month")][order(idx)][3:37],
  mr.expec[3:37]
)

# with NAs:
dt2 <- copy(dt0)
idx <- c(3,34)
dt2[idx, "x":=NA]

expect_equal(
  dt2[, contrib(x=x, w=w, t=t, x.all=x.all, w.all=w.all, type="year", settings=list(method="ribe"))][14:37],
  ifelse(1:nrow(dt2)%in%c(idx,idx+12), NA, ribe.expec)[14:37]
)

expect_equal(
  dt2[, contrib(x=x, w=w, t=t, x.all=x.all, w.all=w.all, type="year", settings=list(method="kirchner"))][14:37],
  ifelse(1:nrow(dt2)%in%c(idx,idx+12), NA, kirchner.expec)[14:37]
)

expect_equal(
  dt2[, contrib(x=x, w=w, t=t, x.all=x.all, w.all=w.all, type="month")][3:37],
  ifelse(1:nrow(dt2)%in%c(idx,idx+1), NA, mr.expec)[3:37]
)

# with gaps:
dt3 <- copy(dt0)
idx <- c(3,34)
dt3 <- dt3[-idx]

expect_equal(
  dt3[, contrib(x=x, w=w, t=t, x.all=x.all, w.all=w.all, type="year", settings=list(method="ribe"))][14:37],
  replace(x=ribe.expec[!1:nrow(dt2)%in%idx], list=14, values=NA)[14:37]
)

expect_equal(
  dt3[, contrib(x=x, w=w, t=t, x.all=x.all, w.all=w.all, type="year", settings=list(method="kirchner"))][14:37],
  replace(x=kirchner.expec[!1:nrow(dt2)%in%idx], list=14, values=NA)[14:37]
)

expect_equal(
  dt3[, contrib(x=x, w=w, t=t, x.all=x.all, w.all=w.all, type="month")][3:37],
  replace(x=mr.expec[!1:nrow(dt2)%in%idx], list=c(3,33), values=NA)[3:37]
)


## make own example

# example with monthly frequency:
dt <- data.table(
  "time"=rep(seq.Date(from=as.Date("2019-12-01"), to=as.Date("2023-12-01"), by="1 month"), times=3),
  "coicop"=rep(c("011","012","013"), each=49),
  "index"=runif(n=3*49, min=80, max=130))

# adjust weights to sum to 1 in each period:
dt[, "weight":=runif(n=1, min=1, max=30), by=list(year(time), coicop)]
dt[, "weight" := weight/sum(weight), by="time"]

# unchain indices:
dt[, "price_ratio" := unchain(x=index, t=time), by="coicop"]

# aggregate, chain and rebase indices:
dtagg1 <- dt[, list("coicop"="01", "weight"=sum(weight), "price_ratio"=laspeyres(x=price_ratio, w0=weight)), by="time"]
dtagg1[, "index":=chain(x=price_ratio, t=time)]
dtagg1[, "index":=rebase(x=index, t=time, t.ref="2019-12")]

# add all-items hicp:
dt1 <- merge(x=dt,
             y=dtagg1[, list(time,index,weight)],
             by="time", all.x=TRUE, suffixes=c("","_all"))

# compute contributions:
dt1[, "ar" := contrib(x=index, w=weight, t=time, x.all=index_all, w.all=weight_all, type="year"), by="coicop"]
dt1[, "mr" := contrib(x=index, w=weight, t=time, x.all=index_all, w.all=weight_all, type="month"), by="coicop"]
dt1[, "qr" := contrib(x=index, w=weight, t=time, x.all=index_all, w.all=weight_all, type="quarter"), by="coicop"]

# annual rates:
expect_equal(
  dt1[, sum(ar), by="time"]$V1[13:49],
  dtagg1[, rates(index,time,"year")][13:49]
)

# monthly rates:
expect_equal(
  dt1[, sum(mr), by="time"]$V1[2:49],
  dtagg1[, rates(index,time,"month")][2:49]
)

# quarterly rates:
expect_equal(
  dt1[, sum(qr), by="time"]$V1[4:49],
  dtagg1[, rates(index,time,"quarter")][4:49]
)

# example data with quarterly frequency:
dt <- data.table(
  "time"=rep(seq.Date(from=as.Date("2019-12-01"), to=as.Date("2023-12-01"), by="3 months"), times=3),
  "coicop"=rep(c("011","012","013"), each=17),
  "index"=runif(n=3*17, min=80, max=130))

# adjust weights to sum to 1 in each period:
dt[, "weight":=runif(n=1, min=1, max=30), by=list(year(time), coicop)]
dt[, "weight" := weight/sum(weight), by="time"]

# unchain indices:
dt[, "price_ratio" := unchain(x=index, t=time), by="coicop"]

# aggregate, chain and rebase indices:
dtagg1 <- dt[, list("coicop"="01", "weight"=sum(weight), "price_ratio"=laspeyres(x=price_ratio, w0=weight)), by="time"]
dtagg1[, "index":=chain(x=price_ratio, t=time)]
dtagg1[, "index":=rebase(x=index, t=time, t.ref="2019-12")]

# add all-items hicp:
dt1 <- merge(x=dt,
             y=dtagg1[, list(time,index,weight)],
             by="time", all.x=TRUE, suffixes=c("","_all"))

dt1[, "ar" := contrib(x=index, w=weight, t=time, x.all=index_all, w.all=weight_all, type="year"), by="coicop"]
dt1[, "qr" := contrib(x=index, w=weight, t=time, x.all=index_all, w.all=weight_all, type="quarter"), by="coicop"]

# annual rates:
expect_equal(
  dt1[, sum(ar), by="time"]$V1[5:17],
  dtagg1[, rates(index,time,"year")][5:17]
)

# quarterly rates:
expect_equal(
  dt1[, sum(qr), by="time"]$V1[2:17],
  dtagg1[, rates(index,time,"quarter")][2:17]
)

# example data with annual frequency:
dt <- data.table(
  "time"=rep(seq.Date(from=as.Date("2019-11-17"), to=as.Date("2023-11-17"), by="12 month"), times=3),
  "coicop"=rep(c("011","012","013"), each=5),
  "weight"=c(runif(n=5, min=0.1, max=0.15), 
             runif(n=5, min=0.5, max=0.6),
             runif(n=5, min=0.3, max=0.35)),
  "index"=c(100,103,102,99,120, 100,104,110,98,125, 100,99,98,98,95)
)

# adjust weights to sum to 1 in each period:
dt[, "weight" := weight/sum(weight), by="time"]

# unchain indices:
dt[, "price_ratio" := unchain(x=index, t=time), by="coicop"]

# aggregate, chain and rebase indices:
dtagg1 <- dt[, list("coicop"="01", "weight"=sum(weight), "price_ratio"=laspeyres(x=price_ratio, w0=weight)), by="time"]
dtagg1[, "index":=chain(x=price_ratio, t=time)]
dtagg1[, "index":=rebase(x=index, t=time, t.ref="2019-11")]

# add all-items hicp:
dt1 <- merge(x=dt,
             y=dtagg1[, list(time,index,weight)],
             by="time", all.x=TRUE, suffixes=c("","_all"))

# ribe decomposition:
dt1[, "ar" := contrib(x=index, w=weight, t=time, x.all=index_all, w.all=weight_all, type="year"), by="coicop"]

# check results:
expect_equal(
  dt1[, sum(ar), by="time"]$V1[2:5],
  dtagg1[, rates(index,time,"year")][2:5]
)

# adjust all weights proportionally:
af <- 4

# aggregate, chain and rebase indices:
dtagg2 <- dt[, list("coicop"="01", "weight"=sum(weight/af), "price_ratio"=laspeyres(x=price_ratio, w0=weight/af)), by="time"]
dtagg2[, "index":=chain(x=price_ratio, t=time)]
dtagg2[, "index":=rebase(x=index, t=time, t.ref="2019-11")]
# -> only difference is the sum of weights

# add all-items hicp:
dt2 <- merge(x=dt,
             y=dtagg2[, list(time,index,weight)],
             by="time", all.x=TRUE, suffixes=c("","_all"))

# ribe decomposition:
dt2[, "ar" := contrib(x=index, w=weight/af, t=time, x.all=index_all, w.all=weight_all, type="year"), by="coicop"]

# check results:
expect_equal(
  dt2[, sum(ar), by="time"]$V1[2:5],
  dtagg2[, rates(index,time,"year")][2:5]
)

expect_equal(dt1$ar, dt2$ar)


# Comparison to published data --------------------------------------------


### HICP

# import data:
load(test_path("testdata","dta.RData"))
load(test_path("testdata","dtm.RData"))
load(test_path("testdata","dtw.RData"))

## Change rates

# compute monthly, annual and 12-month average rates of change:
dtcomp <- copy(dtm)
dtcomp[, "mr_own":=rates(x=index, t=time, type="month"), by="coicop"]
dtcomp[, "ar_own":=rates(x=index, t=time, type="year"), by="coicop"]
dtcomp[, "index_12mar":=convert(x=index, t=time, type="12mavg"), by="coicop"]
dtcomp[, "12mar_own":=rates(x=index_12mar, t=time, type="year"), by="coicop"]

# compare to published change rates:
expect_equal(0, nrow(dtcomp[!is.na(mr) & abs(mr-mr_own)>0.1,]))
expect_equal(0, nrow(dtcomp[!is.na(ar) & abs(ar-ar_own)>0.1,]))
expect_equal(0, nrow(dtcomp[!is.na(`12mar`) & abs(`12mar`-`12mar_own`)>0.1,]))

# annual average rate of change:
dtcomp <- copy(dta)
dtcomp[, "rate_own":=rates(x=index, t=time, type="year"), by=c("geo","coicop")]

# compare to published change rates:
expect_equal(0, nrow(dtcomp[!is.na(rate) & abs(rate-rate_own)>0.1,]))

## Ribe contributions

# merge price indices and item weights:
dtcomp <- merge(x=dtm, y=dtw, by=c("geo","coicop","year"), all.x=TRUE)

# add all-items hicp:
dtcomp <- merge(x=dtcomp,
                y=dtcomp[coicop=="CP00", list(geo,time,index,weight)],
                by=c("geo","time"), all.x=TRUE, suffixes=c("","_all"))

# ribe decomposition:
dtcomp[, "ribe_own" := contrib(x=index, w=weight, t=time, 
                                  x.all=index_all, w.all=weight_all,
                                  type="year", settings=list(method="ribe")), 
       by=c("geo","coicop")]

# compare to published contributions:
expect_equal(0, nrow(dtcomp[!is.na(ribe) & abs(ribe-ribe_own)>0.1, ]))

### OOHPI

# import data:
load(test_path("testdata","dtooh.RData"))

## Change rates

# compute annual rates of change:
dtcomp <- copy(dtooh)
dtcomp[, "qr_own":=rates(x=index, t=time, type="quarter"), by="expend"]
dtcomp[, "ar_own":=rates(x=index, t=time, type="year"), by="expend"]

# compare to published change rates:
expect_equal(0, nrow(dtcomp[!is.na(qr) & abs(qr-qr_own)>0.1,]))
expect_equal(0, nrow(dtcomp[!is.na(ar) & abs(ar-ar_own)>0.1,]))

## Ribe contributions

# add all-items oohpi:
dttmp <- merge(x=dtcomp,
               y=dtcomp[expend=="TOTAL", list(geo,time,index,weight)],
               by=c("geo","time"), all.x=TRUE, suffixes=c("","_all"))

# ribe decompositions of change rates:
dttmp[, "ar" := contrib(x=index, w=weight, t=time, x.all=index_all, w.all=weight_all, type="year"), by="expend"]
dttmp[, "qr" := contrib(x=index, w=weight, t=time, x.all=index_all, w.all=weight_all, type="quarter"), by="expend"]

# check sums:
dtcomp <- merge(
  x=dttmp[expend=="TOTAL", list(time,ar_own,qr_own)], 
  y=dttmp[expend=="TOTAL", list("ar_sum"=sum(ar), "qr_sum"=sum(qr)), by="time"], 
  by="time", all.x=TRUE, sort=FALSE)

# compare sum of contributions to total change rate:
expect_equal(0, nrow(dtcomp[!is.na(ar_sum) & abs(ar_own-ar_sum)>0.1, ]))
expect_equal(0, nrow(dtcomp[!is.na(qr_sum) & abs(qr_own-qr_sum)>0.1, ]))

# END
