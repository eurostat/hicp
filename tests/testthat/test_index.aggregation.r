# START


# Bilateral indices -------------------------------------------------------


# weights without impact on unweighted indices if no NAs present:
p <- 1:3
w0 <- c(0.5, 0.2, 0.3)
expect_equal(jevons(x=p, w0=w0), jevons(x=p))
expect_equal(carli(x=p, w0=w0), carli(x=p))
expect_equal(harmonic(x=p, w0=w0), harmonic(x=p))

# weights with impact on unweighted indices if NAs present
# due to subsetting of data:
p <- 1:3
w0 <- c(0.5, 0.2, NA)
expect_false(abs(jevons(x=p, w0=w0)-jevons(x=p))<1e-9)
expect_false(abs(carli(x=p, w0=w0)-carli(x=p))<1e-9)
expect_false(abs(harmonic(x=p, w0=w0)-harmonic(x=p))<1e-9)

# incomplete cases only:
p <- as.numeric(c(NA,NA,NA))
w0 <- c(0.5, 0.2, 0.3)
wt <- c(0.4, 0.25, 0.35)

expect_no_error(jevons(x=p))
expect_equal(NA_real_, jevons(x=p))
expect_equal(jevons(x=p, w0=w0), jevons(x=p))

expect_no_error(carli(x=p))
expect_equal(NA_real_, carli(x=p))
expect_equal(carli(x=p, w0=w0), carli(x=p))

expect_no_error(harmonic(x=p))
expect_equal(NA_real_, harmonic(x=p))
expect_equal(harmonic(x=p, w0=w0), harmonic(x=p))

expect_error(laspeyres(x=p))
expect_error(laspeyres(x=p, wt=wt))
expect_no_error(laspeyres(x=p, w0=w0))
expect_equal(NA_real_, laspeyres(x=p, w0=w0))
expect_equal(laspeyres(x=p, w0=w0), laspeyres(x=p, w0=w0, wt=wt))

expect_error(paasche(x=p))
expect_error(paasche(x=p, w0=w0))
expect_no_error(paasche(x=p, wt=wt))
expect_equal(NA_real_, paasche(x=p, wt=wt))
expect_equal(paasche(x=p, wt=wt), paasche(x=p, w0=w0, wt=wt))

expect_error(fisher(x=p))
expect_error(fisher(x=p, w0=w0))
expect_error(fisher(x=p, wt=wt))
expect_no_error(fisher(x=p, w0=w0, wt=wt))
expect_equal(NA_real_, fisher(x=p, w0=w0, wt=wt))

expect_error(toernqvist(x=p))
expect_error(toernqvist(x=p, w0=w0))
expect_error(toernqvist(x=p, wt=wt))
expect_no_error(toernqvist(x=p, w0=w0, wt=wt))
expect_equal(NA_real_, toernqvist(x=p, w0=w0, wt=wt))

expect_error(walsh(x=p))
expect_error(walsh(x=p, w0=w0))
expect_error(walsh(x=p, wt=wt))
expect_no_error(walsh(x=p, w0=w0, wt=wt))
expect_equal(NA_real_, walsh(x=p, w0=w0, wt=wt))


# aggregate() -------------------------------------------------------------


# input data:
dt <- data.table(
  "time"=rep(1:2, each=5),
  "coicop"=rep(c("01111","01112","0112","0113","021"), times=2),
  "price"=c(105,103,102,99,120, 105,104,110,98,125),
  "weight"=rep(c(0.05,0.15,0.3,0.2,0.3), times=2),
  "weight_lag"=rep(c(0.03,0.12,0.33,0.2,0.32), times=2))

# aggregate with package function:
res.pkg <- dt[, aggregate(x=price, w0=weight, wt=weight_lag, grp=coicop), by="time"]

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
D <- D[, list("coicop"="00",
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
setnames(x=res.own, c("time","grp","laspeyres","w0","wt","is_aggregated"))
setcolorder(x=res.own, neworder=names(res.pkg))

# compare results:
expect_equal(res.own, res.pkg)

# check output:
expect_equal(
  c("time","grp","is_aggregated","w0","laspeyres"),
  colnames(dt[, aggregate(x=price, grp=coicop, w0=weight, index=laspeyres), by="time"])
)

expect_equal(
  c("time","grp","is_aggregated","w0","wt","fisher"),
  colnames(dt[, aggregate(x=price, grp=coicop, w0=weight, wt=weight, index=fisher), by="time"])
)

expect_equal(
  c("time","grp","is_aggregated","w0","wt","lasp","fisher"),
  colnames(dt[, aggregate(x=price, grp=coicop, w0=weight, wt=weight, 
  index=list("lasp"=laspeyres, "fisher"=fisher)), by="time"])
)

expect_equal(
  c("time","grp","w0","wt","fisher"),
  colnames(dt[, aggregate(x=price, grp=coicop, w0=weight, wt=weight, 
                          index=fisher, settings=list(keep.lowest=FALSE)), by="time"])
)

expect_equal(
  c("time","grp","jevons"),
  colnames(dt[, aggregate(x=price, grp=coicop, 
                          index=jevons, settings=list(keep.lowest=FALSE)), by="time"])
)

# check for errors:
expect_error(
  dt[, aggregate(x=price, w0=weight, index=laspeyres), by="time"]
)

expect_error(
  dt[, aggregate(x=price, grp=coicop, wt=weight, index=list(mean)), by="time"]
)

expect_error(
  dt[, aggregate(x=price,  grp=coicop, wt=weight, index=list("mean"=function(x) mean(x))), by="time"]
)

# END
