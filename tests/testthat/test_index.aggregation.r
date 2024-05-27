# START

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

# check for errors:
expect_error(
  dt[, aggregate(x=price, wt=weight, index=laspeyres), by="time"]
)

expect_error(
  dt[, aggregate(x=price, grp=coicop, wt=weight, index=list(mean)), by="time"]
)

expect_error(
  dt[, aggregate(x=price,  grp=coicop, wt=weight, index=list("mean"=function(x) mean(x))), by="time"]
)
# END
