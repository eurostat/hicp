# START

# set global options:
options(hicp.chatty=FALSE)
options(hicp.coicop.prefix="") # no prefix
options(hicp.coicop.version="ecoicop2.hicp")
options(hicp.all.items.code="TOTAL")


# Bilateral indices -------------------------------------------------------


# example without NAs:
x <- 1:4
w1 <- c(0.1,0.3,0.5,0.1)
w2 <- c(0.15,0.25,0.45,0.15)
expect_length(jevons(x), 1L)
expect_length(carli(x), 1L)
expect_length(harmonic(x), 1L)
expect_length(laspeyres(x, w1), 1L)
expect_length(paasche(x, w2), 1L)
expect_length(fisher(x, w1, w2), 1L)
expect_length(toernqvist(x, w1, w2), 1L)
expect_length(walsh(x=x, w0=w1, wt=w2), 1L)

# example with all weights being 0:
x <- 1:4
w1 <- w2 <- rep(0, 4)
expect_equal(laspeyres(x, w1), NA_real_)
expect_equal(paasche(x, w2), NA_real_)
expect_equal(fisher(x, w1, w2), NA_real_)
expect_equal(toernqvist(x, w1, w2), NA_real_)
expect_equal(walsh(x=x, w0=w1, wt=w2), NA_real_)

# example with NA weight:
x <- c(1,2,3,4)
w1 <- c(0.1,0.3,0.5,NA)
w2 <- c(0.15,0.25,0.45,NA)
expect_gt(jevons(x), 0)
expect_gt(carli(x), 0)
expect_gt(harmonic(x), 0)
expect_equal(laspeyres(x, w1), NA_real_)
expect_equal(paasche(x, w2), NA_real_)
expect_equal(fisher(x, w1, w2), NA_real_)
expect_equal(toernqvist(x, w1, w2), NA_real_)
expect_equal(walsh(x=x, w0=w1, wt=w2), NA_real_)

# example with NA in index and positive weight:
x <- c(1,2,3,NA)
w1 <- c(0.1,0.3,0.5,0.1)
w2 <- c(0.15,0.25,0.45,0.15)
expect_equal(jevons(x), NA_real_)
expect_equal(carli(x), NA_real_)
expect_equal(harmonic(x), NA_real_)
expect_equal(laspeyres(x, w1), NA_real_)
expect_equal(paasche(x, w2), NA_real_)
expect_equal(fisher(x, w1, w2), NA_real_)
expect_equal(toernqvist(x, w1, w2), NA_real_)
expect_equal(walsh(x=x, w0=w1, wt=w2), NA_real_)

# example with (x=NA,w=0):
x <- c(1,2,3,NA)
w1 <- c(0.1,0.3,0.5,0)
w2 <- c(0.15,0.25,0.45,0)
expect_equal(jevons(x), NA_real_)
expect_equal(carli(x), NA_real_)
expect_equal(harmonic(x), NA_real_)
expect_gt(laspeyres(x, w1), 0)
expect_gt(paasche(x, w2), 0)
expect_gt(fisher(x, w1, w2), 0)
expect_gt(toernqvist(x, w1, w2), 0)
expect_gt(walsh(x=x, w0=w1, wt=w2), 0)

# example with zero prices:
x <- c(1,2,3,0)
w1 <- c(0.1,0.3,0.5,0.1)
w2 <- c(0.15,0.25,0.45,0.1)
expect_equal(jevons(x), 0)
expect_gt(carli(x), 0) # different!
expect_equal(harmonic(x), 0)
expect_gt(laspeyres(x, w1), 0) # different!
expect_equal(paasche(x, w2), 0)
expect_equal(fisher(x, w1, w2), 0)
expect_equal(toernqvist(x, w1, w2), 0)
expect_equal(walsh(x=x, w0=w1, wt=w2), 0)

# example with Inf prices:
x <- c(1,2,3,Inf)
w1 <- c(0.1,0.3,0.5,0.1)
w2 <- c(0.15,0.25,0.45,0.1)
expect_equal(jevons(x), Inf)
expect_equal(carli(x), Inf)
expect_lt(harmonic(x), Inf) # different!
expect_equal(laspeyres(x, w1), Inf) 
expect_lt(paasche(x, w2), Inf) # different!
expect_equal(fisher(x, w1, w2), Inf)
expect_equal(toernqvist(x, w1, w2), Inf)
expect_equal(walsh(x=x, w0=w1, wt=w2), Inf)

# example with Inf weight:
x <- c(1,2,3,3)
w1 <- c(0.1,0.3,0.5,Inf)
w2 <- c(0.15,0.25,0.45,Inf)
expect_equal(laspeyres(x, w1), NA_real_)
expect_equal(paasche(x, w2), NA_real_)
expect_equal(fisher(x, w1, w2), NA_real_)
expect_equal(toernqvist(x, w1, w2), NA_real_)
expect_equal(walsh(x=x, w0=w1, wt=w2), NA_real_)


# aggregate() and disaggregate() ------------------------------------------


## Case 1: complete data

# aggregation of (a1,a2,a3,a4,a5) into A:
x1 <- c(100,105,90,120,113)
w1 <- c(0.2,0.05,0.03,0.09,0.2)
A <- aggregate(x=x1, 
               w0=w1, 
               id=c("a1","a2","a3","a4","a5"), 
               agg=list(c("a1","a2","a3","a4","a5")), 
               settings=list(names="A"))

expect_equal(A$id, "A")
expect_equal(A$w0, sum(w1))
expect_equal(A$laspeyres, weighted.mean(x1, w1, na.rm=FALSE))

a4 <- disaggregate(x=c(x1,A$laspeyres), 
                   w0=c(w1,A$w), 
                   id=c("a1","a2","a3","a4","a5","A"),
                   agg=list("A"=c("a1","a2","a3","a5")),
                   settings=list(names="a4"))

expect_equal(a4$id, "a4")
expect_equal(a4$w0, w1[4])
expect_equal(a4$laspeyres, x1[4])

# aggregation  of (A,B,C,D) into total:
x2 <- c(A$laspeyres,103,92,NA)
w2 <- c(A$w,0.5,0.13,0)
(P <- laspeyres(x=x2, w=w2))
(W <- sum(w2, na.rm=TRUE))

# aggregation and disaggregation identical:
expect_equal(
  aggregate(x=x1, w0=w1, id=c("a1","a2","a3","a4","a5"),
            agg=list("A"=c("a1","a2","a3","a4","a5")), 
            settings=list(names="A")),
  disaggregate(x=c(P,x2), w0=c(W,w2), id=c("total","A","B","C","D"), 
               agg=list("total"=c("B","C","D")), 
               settings=list(names="A"))
)

## Case 2: NA present in index and w=0

# aggregation of (a1,a2,a3,a4,a5) into A:
x1 <- c(100,105,90,120,NA)
w1 <- c(0.2,0.05,0.03,0.09,0)
A <- aggregate(x=x1, 
               w0=w1, 
               id=c("a1","a2","a3","a4","a5"), 
               agg=list(c("a1","a2","a3","a4","a5")), 
               settings=list(names="A"))

expect_equal(A$id, "A")
expect_equal(A$w0, sum(w1[-5]))
expect_equal(A$laspeyres, weighted.mean(x1, w1, na.rm=TRUE))

# disaggregation into a4:
a4 <- disaggregate(x=c(x1,A$laspeyres), 
                   w0=c(w1,A$w), 
                   id=c("a1","a2","a3","a4","a5","A"),
                   agg=list("A"=c("a1","a2","a3","a5")),
                   settings=list(names="a4"))

expect_equal(a4$id, "a4")
expect_equal(a4$w0, w1[4])
expect_equal(a4$laspeyres, x1[4])

# aggregation  of (A,B,C,D) into total:
x2 <- c(A$laspeyres,103,92,NA)
w2 <- c(A$w0,0.5,0.13,0)
(P <- laspeyres(x=x2, w=w2))
(W <- sum(w2, na.rm=TRUE))

# aggregation and disaggregation identical:
expect_equal(
  aggregate(x=x1, w0=w1, id=c("a1","a2","a3","a4","a5"),
            agg=list("A"=c("a1","a2","a3","a4","a5")), 
            settings=list(names="A")),
  disaggregate(x=c(P,x2), w0=c(W,w2), id=c("total","A","B","C","D"), 
               agg=list("total"=c("B","C","D")), 
               settings=list(names="A"))
)

## General cases

# expect empty data.table for empty agg:
expect_equal(
  data.table("id"=character(),"w0"=numeric(),"x"=numeric()),
  aggregate(x=x1, w0=w1, id=c("a1","a2","a3","a4","a5"), agg=list()),
  ignore_attr=TRUE
)

expect_equal(
  data.table("id"=character(),"w0"=numeric(),"x"=numeric()),
  disaggregate(x=x1, w0=w1, id=c("a1","a2","a3","a4","a5"), agg=list()),
  ignore_attr=TRUE
)

# expect number of rows to be identical to length(agg) even if no data:
expect_equal(
  data.table("id"=as.character(1:2),"w0"=NA_real_,"x"=NA_real_),
  aggregate(x=numeric(), w0=numeric(), id=character(), agg=list("a","b")),
  ignore_attr=TRUE
)

expect_equal(
  data.table("id"=as.character(1:2),"w0"=NA_real_,"x"=NA_real_),
  disaggregate(x=numeric(), w0=numeric(), id=character(), agg=list("A"="a","B"="b")),
  ignore_attr=TRUE
)


# aggregate.tree() --------------------------------------------------------


# input data:
dt <- data.table(
  "time"=rep(1:2, each=5),
  "coicop"=rep(c("01111","01112","0112","0113","021"), times=2),
  "price"=c(105,103,102,99,120, 105,104,110,98,125),
  "weight"=rep(c(0.05,0.15,0.3,0.2,0.3), times=2),
  "weight_lag"=rep(c(0.03,0.12,0.33,0.2,0.32), times=2))

# aggregate with package function:
res.pkg1 <- dt[, aggregate.tree(x=price, w0=weight, wt=weight_lag, id=coicop), by="time"]

# aggregate manually:
A <- dt[coicop%in%c("01111","01112"),
        list("coicop"="0111",
             "price"=laspeyres(x=price, w0=weight),
             "weight"=sum(weight),
             "weight_lag"=sum(weight_lag)),
        by="time"]

B <- rbindlist(l=list(A, dt[coicop%in%c("0112","0113"),]))
B <- B[, list("coicop"="011",
              "price"=laspeyres(x=price, w0=weight),
              "weight"=sum(weight),
              "weight_lag"=sum(weight_lag)),
       by="time"]

C1 <- copy(B)
C1[, "coicop":="01"]
C2 <- dt[coicop%in%"021", list(time, "coicop"="02", price, weight, weight_lag)]

D <- rbindlist(l=list(C1,C2))
D <- D[, list("coicop"="TOTAL",
              "price"=laspeyres(x=price, w0=weight),
              "weight"=sum(weight),
              "weight_lag"=sum(weight_lag)),
       by="time"]

res.own <- rbindlist(l=list(A,B,C1,C2,D))
res.own[, "is_aggregated":=TRUE]
res.own <- rbindlist(
  l=list(res.own, dt[coicop%in%c("01111","01112","0112","0113","021"),]),
  use.names=TRUE, fill=TRUE)
res.own[is.na(is_aggregated), "is_aggregated":=FALSE]
setnames(x=res.own, c("time","id","laspeyres","w0","wt","is_aggregated"))
setorderv(x=res.own, c("time","id"))
setcolorder(x=res.own, neworder=names(res.pkg1))

# compare results:
expect_equal(res.own, res.pkg1)

# no aggregation if invalid COICOP codes:
res.pkg2 <- dt[, aggregate.tree(x=price, w0=weight, wt=weight_lag, id=paste0("CP", coicop)), by="time"]
expect_equal(nrow(res.pkg2), 0L)

# identical results if COICOP prefix expected:
res.pkg3 <- dt[, aggregate.tree(x=price, w0=weight, wt=weight_lag, id=paste0("CP", coicop), settings=list(coicop.prefix="CP")), by="time"]
expect_equal(res.pkg1$laspeyres, res.pkg3$laspeyres)

# check output:
expect_equal(
  c("time","id","is_aggregated","jevons"),
  colnames(dt[, aggregate.tree(x=price, id=coicop, formula=jevons), by="time"])
)

expect_equal(
  c("time","id","is_aggregated","w0","jevons"),
  colnames(dt[, aggregate.tree(x=price, id=coicop, w0=weight, formula=jevons), by="time"])
)

expect_equal(
  c("time","id","is_aggregated","w0","wt","jevons"),
  colnames(dt[, aggregate.tree(x=price, id=coicop, w0=weight, wt=weight, formula=jevons), by="time"])
)

# same name for prices as for function:
dt[, "laspeyres":=price]

expect_no_error(
  dt[, aggregate.tree(x=laspeyres, id=coicop, w0=weight, formula=laspeyres), by="time"]
)

expect_no_error(
  dt[, aggregate.tree(x=laspeyres, id=coicop, w0=weight, 
                      formula=list("laspeyres"=laspeyres, "jevons"=mean)), by="time"]
)

# expect empty data.table for empty data:
expect_equal(
  data.table("id"=character(),"is_aggregated"=logical(),"w0"=numeric(),"laspeyres"=numeric(), key="id"),
  aggregate.tree(x=numeric(), w0=numeric(), id=character())
)

# check for errors:
expect_error(dt[, aggregate.tree(x=price, w0=weight, formula=laspeyres), by="time"])
expect_error(dt[, aggregate.tree(x=price, id=coicop, formula=laspeyres), by="time"])
expect_error(dt[, aggregate.tree(x=price, id=coicop, wt=weight, formula=list(mean)), by="time"])

# data containing bundle codes:
dt <- data.table(
  "coicop"=c("08","08X","081","08101","082","08201","08203","083","08301"),
  "price"=c(100,104,90,90,104,103,107,105,105),
  "weight"=c(1,0.7,0.3,0.3,0.5,0.4,0.1,0.2,0.2))

# bundle code is not used in aggregation so it should not be part of results:
A <- dt[, aggregate.tree(x=price, w0=weight, id=coicop, settings=list(coicop.version="ecoicop1.hicp"))]
B <- dt[c(1,2,3,4,6,7,8), aggregate.tree(x=price, w0=weight, id=coicop, settings=list(coicop.version="ecoicop1.hicp"))]
expect_true(!"08X"%in%A$id)
expect_true(!"08X"%in%B$id)
expect_equal(A[id=="TOTAL", list(w0,laspeyres)], B[id=="TOTAL", list(w0,laspeyres)])

# bundle code is used in aggregation so it should be part of results:
C <- dt[1:4, aggregate.tree(x=price, w0=weight, id=coicop, settings=list(coicop.version="ecoicop1.hicp"))]
D <- dt[c(1,2,3,4,8,9), aggregate.tree(x=price, w0=weight, id=coicop, settings=list(coicop.version="ecoicop1.hicp"))]
expect_true("08X"%in%C$id)
expect_true("08X"%in%D$id)
expect_equal(C[id=="TOTAL", list(w0,laspeyres)], D[id=="TOTAL", list(w0,laspeyres)])


# Comparison to published HICP ECOICOP ver. 1 data ------------------------


# set global options:
options(hicp.coicop.version="ecoicop1.hicp")
options(hicp.coicop.prefix="CP")
options(hicp.all.items.code="CP00")

# load data:
load(test_path("testdata","dthicp1m.RData"))

# unchain indices for aggregation:
dthicp1m[, "dec_ratio" := unchain(x=index, t=time), by="coicop"]

# compute all-items HICP in one step:
dthicp1m[coicop_weight>0 & !is.na(dec_ratio),
         "tree":=tree(id=coicop, w=coicop_weight, flag=TRUE, settings=list(w.tol=0.1)),
         by=c("geo","time")]
# dthicp1m[tree==TRUE, sum(coicop_weight), by="time"]
hicp.own <- dthicp1m[tree==TRUE, 
                     list("laspeyres"=laspeyres(x=dec_ratio, w0=coicop_weight)), 
                     by="time"]
setorderv(x=hicp.own, cols="time")
hicp.own[, "index_chained" := chain(x=laspeyres, t=time, by=12)]
hicp.own[, "index_own" := rebase(x=index_chained, t=time, t.ref="2015")]
hicp.own <- merge(x=hicp.own[, list(time, index_own)], y=dthicp1m[coicop=="CP00", list(time, "index_publ"=index)], by="time", all.x=TRUE)
expect_equal(hicp.own$index_publ, hicp.own$index_own, tolerance=0.1)

# compute all-items HICP step-wise through all higher-levels:
hicp.own.all <- dthicp1m[,aggregate.tree(x=dec_ratio, w0=coicop_weight, id=coicop, formula=laspeyres), by="time"]
setorderv(x=hicp.own.all, cols="time")
hicp.own.all[, "index_chained" := chain(x=laspeyres, t=time, by=12), by="id"]
hicp.own.all[, "index_own" := rebase(x=index_chained, t=time, t.ref="2015"), by="id"]
# hicp.own.all <- merge(
#   x=hicp.own.all[, list(id,time,index_own,"weight_own"=w0)],
#   y=dthicp1m[, list("id"=coicop,time,"index_publ"=index,coicop_weight)],
#   by=c("time","id"),
#   all.x=TRUE)
dtcomp <- merge(x=hicp.own.all[id=="CP00", list(time, "index_gradual"=index_own)], y=hicp.own[, list(time, "index_direct"=index_own)], by="time")
expect_equal(dtcomp$index_direct, dtcomp$index_gradual, tolerance=0.1)

# compare special aggregates computed by aggregate():
aggs1 <- special.agg(id=c("FOOD","NRG"))
dtspagg1 <- dthicp1m[time>=as.Date("2017-01-01"),
                     aggregate(x=dec_ratio, w0=coicop_weight, id=coicop, 
                               agg=aggs1, 
                               settings=list(names=names(aggs1))), 
                     by="time"]
dtcomp <- merge(x=dthicp1m[, list(time,"id"=coicop,coicop_weight,dec_ratio)], y=dtspagg1, by=c("id","time"))
expect_equal(dtcomp$coicop_weight, dtcomp$w0, tolerance=0.1)
expect_equal(dtcomp$dec_ratio, dtcomp$laspeyres, tolerance=0.1/100)

# compare special aggregates computed by disaggregate():
aggs2 <- list("CP00"=c("FOOD","NRG"), "CP00"="FUEL")
dtspagg2 <- dthicp1m[time>=as.Date("2017-01-01"), 
                     disaggregate(x=dec_ratio, w0=coicop_weight, id=coicop, 
                                  agg=aggs2,
                                  settings=list(names=c("TOT_X_NRG_FOOD","TOT_X_FUEL"))), 
                     by="time"]
dtcomp <- merge(x=dthicp1m[, list(time,"id"=coicop,coicop_weight,dec_ratio)], y=dtspagg2, by=c("id","time"))
expect_equal(dtcomp$coicop_weight, dtcomp$w0, tolerance=0.1)
expect_equal(dtcomp$dec_ratio, dtcomp$laspeyres, tolerance=0.1/100)


# Comparison to published HICP ECOICOP ver. 2 data ------------------------


# set global options:
options(hicp.coicop.version="ecoicop2.hicp")
options(hicp.coicop.prefix="CP")
options(hicp.all.items.code="TOTAL")

# load data:
load(test_path("testdata","dthicp2m.RData"))

# unchain indices for aggregation:
dthicp2m[, "dec_ratio" := unchain(x=index, t=time), by="coicop18"]

# compute all-items HICP in one step:
dthicp2m[coicop18_weight>0 & !is.na(dec_ratio),
         "tree":=tree(id=coicop18, w=coicop18_weight, flag=TRUE, settings=list(w.tol=0.1)),
         by=c("geo","time")]
# dthicp2m[tree==TRUE, sum(coicop18_weight), by="time"]
hicp.own <- dthicp2m[tree==TRUE, 
                     list("laspeyres"=laspeyres(x=dec_ratio, w0=coicop18_weight)),
                     by="time"]
setorderv(x=hicp.own, cols="time")
hicp.own[, "index_chained" := chain(x=laspeyres, t=time, by=12)]
hicp.own[, "index_own" := rebase(x=index_chained, t=time, t.ref="2025")]
hicp.own <- merge(x=hicp.own[, list(time, index_own)], y=dthicp2m[coicop18=="TOTAL", list(time, "index_publ"=index)], by="time", all.x=TRUE)
expect_equal(hicp.own$index_publ, hicp.own$index_own, tolerance=0.1)

# compute all-items HICP step-wise through all higher-levels:
hicp.own.all <- dthicp2m[, aggregate.tree(x=dec_ratio, w0=coicop18_weight, id=coicop18, formula=laspeyres), by="time"]
setorderv(x=hicp.own.all, cols="time")
hicp.own.all[, "index_chained" := chain(x=laspeyres, t=time, by=12), by="id"]
hicp.own.all[, "index_own" := rebase(x=index_chained, t=time, t.ref="2025"), by="id"]
# hicp.own.all <- merge(
#   x=hicp.own.all[, list(id,time,index_own,"weight_own"=w0)],
#   y=dthicp2m[, list("id"=coicop18,time,"index_publ"=index,coicop18_weight)],
#   by=c("time","id"),
#   all.x=TRUE)
dtcomp <- merge(x=hicp.own.all[id=="TOTAL", list(time, "index_gradual"=index_own)], y=hicp.own[, list(time, "index_direct"=index_own)], by="time")
expect_equal(dtcomp$index_gradual, dtcomp$index_direct, tolerance=0.1)

# compare special aggregates computed by aggregate():
aggs1 <- special.agg(id=c("FOOD","NRG"))
dtspagg1 <- dthicp2m[time>as.Date("2019-12-01"),
                     aggregate(x=dec_ratio, w0=coicop18_weight, id=coicop18, 
                               agg=aggs1, 
                               settings=list(names=names(aggs1))), 
                     by="time"]
dtcomp <- merge(x=dthicp2m[, list(time,"id"=coicop18,coicop18_weight,dec_ratio)], y=dtspagg1, by=c("id","time"))
expect_equal(dtcomp$coicop18_weight, dtcomp$w0, tolerance=0.1)
expect_equal(dtcomp$dec_ratio, dtcomp$laspeyres, tolerance=0.1/100)

# compare special aggregates computed by disaggregate():
aggs2 <- list("TOTAL"=c("FOOD","NRG"), "TOTAL"="FUEL")
dtspagg2 <- dthicp2m[time>as.Date("2019-12-01"), 
                     disaggregate(x=dec_ratio, w0=coicop18_weight, id=coicop18, 
                                  agg=aggs2,
                                  settings=list(names=c("TOT_X_NRG_FOOD","TOT_X_FUEL"))), 
                     by="time"]
dtcomp <- merge(x=dthicp2m[, list(time,"id"=coicop18,coicop18_weight,dec_ratio)], y=dtspagg2, by=c("id","time"))
expect_equal(dtcomp$coicop18_weight, dtcomp$w0, tolerance=0.1)
expect_equal(dtcomp$dec_ratio, dtcomp$laspeyres, tolerance=0.1/100)

## aggregation of euro area evolving composition

# load data:
load(test_path("testdata","dthicp2mEA.RData"))

# unchain all indices for aggregation:
dthicp2mEA[, "dec_ratio" := unchain(x=index, t=time), by=c("geo","coicop18")]

# flag euro area countries:
dthicp2mEA[, "is_ea":=is.ea(id=geo, t=time)]

# approach 1 for euro area aggregate:
dtagg1 <- dthicp2mEA[time>as.Date("2019-12-01"), 
                     aggregate(x=dec_ratio, w0=coicop18_weight*geo_weight/1000, id=geo,
                               agg=list(european.agg(id="EA", t=time)),
                               settings=list(names="EA")),
                     by=c("coicop18","time")]

# compare to published values for euro area:
dtcomp <- merge(x=dthicp2mEA[geo=="EA", list(time,coicop18,coicop18_weight,dec_ratio)], y=dtagg1, by=c("coicop18","time"), all.y=TRUE)
expect_equal(dtcomp$coicop18_weight, dtcomp$w0, tolerance=0.1)
expect_equal(dtcomp$dec_ratio, dtcomp$laspeyres, tolerance=0.1/100)

# approach 2 for euro area aggregate:
dtagg2 <- dthicp2mEA[time>as.Date("2019-12-01") & is_ea==TRUE, 
                     laspeyres(x=dec_ratio, w0=coicop18_weight*geo_weight/1000), 
                     by=c("coicop18","time")]

# compare to results of first approach:
dtcomp <- merge(x=dtagg1, y=dtagg2, by=c("coicop18","time"))
expect_equal(dtcomp$laspeyres, dtcomp$V1, tolerance=0.01)

# END
