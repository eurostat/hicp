# START

# set global options:
options(hicp.chatty=FALSE)
options(hicp.coicop.prefix="") # no prefix
options(hicp.coicop.version="ecoicop2.hicp")
options(hicp.all.items.code="TOTAL")


# Bilateral indices -------------------------------------------------------


# scalar as output:
p <- 1:4
w0 <- c(0.5, 0.1, 0.3, 0.1)
wt <- c(0.45, 0.1, 0.25, 0.2)
expect_length(jevons(x=p), 1L)
expect_length(carli(x=p), 1L)
expect_length(harmonic(x=p), 1L)
expect_length(laspeyres(x=p, w0=w0), 1L)
expect_length(paasche(x=p, wt=wt), 1L)
expect_length(fisher(x=p, w0=w0, wt=wt), 1L)
expect_length(toernqvist(x=p, w0=w0, wt=wt), 1L)
expect_length(walsh(x=p, w0=w0, wt=wt), 1L)

# some NAs lead to renormalization of weights:
p <- c(1:3, NA)
w0 <- c(0.5, NA, 0.3, 0.1)
wt <- c(0.45, 0.1, 0.25, 0.2)
idxA <- stats::complete.cases(p)
idxB <- stats::complete.cases(p, w0)
w0B <- w0[idxB]/sum(w0[idxB])
wtB <- wt[idxB]/sum(wt[idxB])
idxC <- stats::complete.cases(p, wt)
w0C <- w0[idxC]/sum(w0[idxC])
wtC <- wt[idxC]/sum(wt[idxC])
idxD <- stats::complete.cases(p, w0, wt)
w0D <- w0[idxD]/sum(w0[idxD])
wtD <- wt[idxD]/sum(wt[idxD])

expect_equal(jevons(x=p), jevons(x=p[idxA]))
expect_equal(carli(x=p), carli(x=p[idxA]))
expect_equal(harmonic(x=p), harmonic(x=p[idxA]))
expect_equal(laspeyres(x=p, w0=w0), laspeyres(x=p[idxB], w0=w0[idxB]))
expect_equal(paasche(x=p, wt=wt), paasche(x=p[idxC], wt=wt[idxC]))
expect_equal(fisher(x=p, w0=w0, wt=wt), fisher(x=p[idxD], w0=w0[idxD], wt=wt[idxD]))
expect_equal(toernqvist(x=p, w0=w0, wt=wt), toernqvist(x=p[idxD], w0=w0[idxD], wt=wt[idxD]))
expect_equal(walsh(x=p, w0=w0, wt=wt), walsh(x=p[idxD], w0=w0[idxD], wt=wt[idxD]))

# incomplete cases lead to NA as output:
p <- c(1:3, NA)
w0 <- c(NA, NA, 0.3, 0.1)
wt <- c(0.45, 0.1, NA, 0.2)
expect_equal(jevons(x=NA_real_), NA_real_)
expect_equal(carli(x=NA_real_), NA_real_)
expect_equal(harmonic(x=NA_real_), NA_real_)
expect_equal(laspeyres(x=p[1:2], w0=w0[1:2]), NA_real_)
expect_equal(paasche(x=p[3], wt=wt[3]), NA_real_)
expect_equal(fisher(x=p, w0=w0, wt=wt), NA_real_)
expect_equal(toernqvist(x=p, w0=w0, wt=wt), NA_real_)
expect_equal(walsh(x=p, w0=w0, wt=wt), NA_real_)


# aggregate() and disaggregate() ------------------------------------------


# aggregation of (a1,a2,a3,a4) into A:
x1 <- c(100, 105, 90, 120)
w1 <- c(0.2,0.05,0.03,0.09)
sum(w1)

res <- disaggregate(x=c(x1,laspeyres(x1,w1)), 
                    w0=c(w1,sum(w1)), 
                    id=c("a1","a2","a3","a4","A"),
                    agg=list("A"=c("a1","a2","a3")),
                    settings=list(names="a4"))

expect_equal(res$w0, w1[4])
expect_equal(res$laspeyres, x1[4])

# aggregation  of (A,B,C) into total:
x2 <- c(103,92,laspeyres(x1,w1))
w2 <- c(0.5,0.13,0.37)
(P <- laspeyres(x=x2, w=w2))
(W <- sum(w2))

# aggregation and disaggregation identical:
all.equal(
  aggregate(x=x1, w0=w1, id=c("a1","a2","a3","a4"),
            agg=list("A"=c("a1","a2","a3","a4")), 
            settings=list(names="A")),
  disaggregate(x=c(P,x2), w0=c(W,w2), id=c("total","A","B","C"), 
               agg=list("total"=c("A","B")), 
               settings=list(names="A"))
)

# expect empty data.table for empty agg:
expect_equal(
  data.table("id"=character(),"w0"=numeric(),"laspeyres"=numeric()),
  aggregate(x=x1, w0=w1, id=c("a1","a2","a3","a4"), agg=list())
)

expect_equal(
  data.table("id"=character(),"w0"=numeric(),"laspeyres"=numeric()),
  disaggregate(x=x1, w0=w1, id=c("a1","a2","a3","a4"), agg=list())
)

# expect number of rows to be identical to length(agg) even if no data:
expect_equal(
  data.table("id"=as.character(1:2),"w0"=NA_real_,"laspeyres"=NA_real_),
  aggregate(x=numeric(), w0=numeric(), id=character(), agg=list("a","b"))
)

expect_equal(
  data.table("id"=as.character(1:2),"w0"=NA_real_,"laspeyres"=NA_real_),
  disaggregate(x=numeric(), w0=numeric(), id=character(), agg=list("A"="a","B"="b"))
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
setorderv(x=res.own, c("time","coicop"))
setnames(x=res.own, c("time","id","laspeyres","w0","wt","is_aggregated"))
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
  c("time","id","is_aggregated","w0","laspeyres"),
  colnames(dt[, aggregate.tree(x=price, id=coicop, w0=weight, formula=laspeyres), by="time"])
)

expect_equal(
  c("time","id","is_aggregated","w0","wt","fisher"),
  colnames(dt[, aggregate.tree(x=price, id=coicop, w0=weight, wt=weight, formula=fisher), by="time"])
)

expect_equal(
  c("time","id","is_aggregated","w0","wt","lasp","fisher"),
  colnames(dt[, aggregate.tree(x=price, id=coicop, w0=weight, wt=weight, 
                          formula=list("lasp"=laspeyres, "fisher"=fisher)), by="time"])
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
expect_equal(nrow(hicp.own[!is.na(index_publ) & abs(index_publ-index_own)>0.1,]), 0)

# compute all-items HICP step-wise through all higher-levels:
hicp.own.all <- dthicp1m[coicop_weight>0 & !is.na(dec_ratio),
                         aggregate.tree(x=dec_ratio, w0=coicop_weight, id=coicop, formula=laspeyres),
                         by="time"]
setorderv(x=hicp.own.all, cols="time")
hicp.own.all[, "index_chained" := chain(x=laspeyres, t=time, by=12), by="id"]
hicp.own.all[, "index_own" := rebase(x=index_chained, t=time, t.ref="2015"), by="id"]
# hicp.own.all <- merge(
#   x=hicp.own.all[, list(id,time,index_own,"weight_own"=w0)],
#   y=dthicp1m[, list("id"=coicop,time,"index_publ"=index,coicop_weight)],
#   by=c("time","id"),
#   all.x=TRUE)
dtcomp <- merge(x=hicp.own.all[id=="CP00", list(time, "index_gradual"=index_own)], y=hicp.own[, list(time, "index_direct"=index_own)], by="time")
expect_equal(nrow(dtcomp[abs(index_gradual-index_direct)>0.1,]), 0)

# compare special aggregates computed by aggregate():
aggs1 <- spec.agg(id=c("FOOD","NRG"))
dtspagg1 <- dthicp1m[time>=as.Date("2017-01-01"),
                     aggregate(x=dec_ratio, w0=coicop_weight, id=coicop, 
                               agg=aggs1, 
                               settings=list(names=names(aggs1))), 
                     by="time"]
dtcomp <- merge(x=dthicp1m[, list(time,"id"=coicop,coicop_weight,dec_ratio)], y=dtspagg1, by=c("id","time"), all.y=TRUE)
expect_equal(nrow(dtcomp[100*abs(dec_ratio-laspeyres)>0.1,]), 0)

# compare special aggregates computed by disaggregate():
aggs2 <- list("CP00"=c("FOOD","NRG"), "CP00"="FUEL")
dtspagg2 <- dthicp1m[time>=as.Date("2017-01-01"), 
                     disaggregate(x=dec_ratio, w0=coicop_weight, id=coicop, 
                                  agg=aggs2,
                                  settings=list(names=c("TOT_X_NRG_FOOD","TOT_X_FUEL"))), 
                     by="time"]
dtcomp <- merge(x=dthicp1m[, list(time,"id"=coicop,coicop_weight,dec_ratio)], y=dtspagg2, by=c("id","time"), all.y=TRUE)
expect_equal(nrow(dtcomp[100*abs(dec_ratio-laspeyres)>0.1,]), 0)


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
expect_equal(nrow(hicp.own[!is.na(index_publ) & abs(index_publ-index_own)>0.1,]), 0)

# compute all-items HICP step-wise through all higher-levels:
hicp.own.all <- dthicp2m[coicop18_weight>0 & !is.na(dec_ratio),
                         aggregate.tree(x=dec_ratio, w0=coicop18_weight, id=coicop18, formula=laspeyres), 
                         by="time"]
setorderv(x=hicp.own.all, cols="time")
hicp.own.all[, "index_chained" := chain(x=laspeyres, t=time, by=12), by="id"]
hicp.own.all[, "index_own" := rebase(x=index_chained, t=time, t.ref="2025"), by="id"]
# hicp.own.all <- merge(
#   x=hicp.own.all[, list(id,time,index_own,"weight_own"=w0)],
#   y=dthicp2m[, list("id"=coicop18,time,"index_publ"=index,coicop18_weight)],
#   by=c("time","id"),
#   all.x=TRUE)
dtcomp <- merge(x=hicp.own.all[id=="TOTAL", list(time, "index_gradual"=index_own)], y=hicp.own[, list(time, "index_direct"=index_own)], by="time")
expect_equal(nrow(dtcomp[abs(index_gradual-index_direct)>0.1,]), 0)

# compare special aggregates computed by aggregate():
aggs1 <- spec.agg(id=c("FOOD","NRG"))
dtspagg1 <- dthicp2m[time>=as.Date("2019-12-01"),
                     aggregate(x=dec_ratio, w0=coicop18_weight, id=coicop18, 
                               agg=aggs1, 
                               settings=list(exact=FALSE, names=names(aggs1))), 
                     by="time"]
dtcomp <- merge(x=dthicp2m[, list(time,"id"=coicop18,coicop18_weight,dec_ratio)], y=dtspagg1, by=c("id","time"), all.y=TRUE)
expect_equal(nrow(dtcomp[100*abs(dec_ratio-laspeyres)>0.1,]), 0)

# compare special aggregates computed by disaggregate():
aggs2 <- list("TOTAL"=c("FOOD","NRG"), "TOTAL"="FUEL")
dtspagg2 <- dthicp2m[time>=as.Date("2019-12-01"), 
                     disaggregate(x=dec_ratio, w0=coicop18_weight, id=coicop18, 
                                  agg=aggs2,
                                  settings=list(names=c("TOT_X_NRG_FOOD","TOT_X_FUEL"))), 
                     by="time"]
dtcomp <- merge(x=dthicp2m[, list(time,"id"=coicop18,coicop18_weight,dec_ratio)], y=dtspagg2, by=c("id","time"), all.y=TRUE)
expect_equal(nrow(dtcomp[100*abs(dec_ratio-laspeyres)>0.1,]), 0)

# END
