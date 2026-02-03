# START

# set global options:
options(hicp.chatty=FALSE)

# input data:
t <- seq.Date(from=as.Date("2020-01-01"), to=as.Date("2023-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5) # current index
p.new <- p+rnorm(n=length(p), mean=0, sd=1) # new index
p[25:48] <- NA # current index stops in 2021

# expected results for linked index series:
res1 <- c(p[1:24], (p.new*mean(p[format(t,"%Y")=="2022"])/mean(p.new[format(t,"%Y")=="2022"]))[25:48]) # not available
res2 <- c(p[1:24], (p.new*p[t=="2021-12-01"]/p.new[t=="2021-12-01"])[25:48])
res3 <- c(p[1:24], (p.new*mean(p[format(t,"%Y")=="2021"])/mean(p.new[format(t,"%Y")=="2021"]))[25:48])
res4 <- c(p[1:24], (p.new*p[t=="2021-06-01"]/p.new[t=="2021-06-01"])[25:48])

# expected results for level-shift factor:
lsf1 <- (mean(p[format(t,"%Y")=="2022"])/mean(p.new[format(t,"%Y")=="2022"])) # not available
lsf2 <- p[t=="2021-12-01"]/p.new[t=="2021-12-01"] 
lsf3 <- (mean(p[format(t,"%Y")=="2021"])/mean(p.new[format(t,"%Y")=="2021"]))
lsf4 <- p[t=="2021-06-01"]/p.new[t=="2021-06-01"]


### Tests on output format


expect_true(
  is.vector(link(x=p, x.new=p.new, t=t, t.overlap="2021"))
)

expect_true(
  is.matrix(link(x=p, x.new=p.new, t=t, t.overlap=c("2021","2021-12")))
)

expect_equal(
  c(length(t), 24+2),
  dim(link(x=p, x.new=p.new, t=t, t.overlap=NULL))
)

expect_true(
  is.vector(lsf(x=p, x.new=p.new, t=t, t.overlap=NULL))
)

expect_equal(
  24+2,
  length(lsf(x=p, x.new=p.new, t=t, t.overlap=NULL))
)


### Consecutive time periods without gaps/breaks


dt0 <- data.table("time"=t, "old"=p, "new"=p.new)

expect_equal(
  as.matrix(data.table("2022"=res1, "2021-12"=res2, "2021"=res3, "2021-06"=res4)),
  dt0[, link(x=old, x.new=new, t=time, t.overlap=c("2022","2021-12","2021","2021-06"))]
)

expect_equal(
  c("2022"=lsf1/lsf2, "2021-12"=1, "2021"=lsf3/lsf2, "2021-06"=lsf4/lsf2),
  dt0[, lsf(x=old, x.new=new, t=time, t.overlap=c("2022","2021-12","2021","2021-06"))]
)

# no overlap periods available:
dt1 <- copy(dt0)
dt1[1:24, "new":=NA] # new index starts in 2022

expect_equal(
  dt1$old,
  dt1[, link(x=old, x.new=new, t=time, t.overlap=NULL)]
)

expect_equal(
  dt1$old,
  dt1[, link(x=old, x.new=new, t=time, t.overlap="2022")]
)

expect_equal(
  NA_real_,
  dt1[, lsf(x=old, x.new=new, t=time, t.overlap=NULL)]
)

expect_equal(
  c("2022"=NA_real_, "2022-12"=NA_real_),
  dt1[, lsf(x=old, x.new=new, t=time, t.overlap=c("2022","2022-12"))]
)


### Time periods in random, non-chronological order without gaps/breaks


# random ordering of time periods:
idx <- sample(1:length(t))
dt2 <- copy(dt0)[idx, ]

expect_equal(
  as.matrix(data.table("2022"=res1, "2021-12"=res2, "2021"=res3, "2021-06"=res4)),
  dt2[, link(x=old, x.new=new, t=time, t.overlap=c("2022","2021-12","2021","2021-06"))][order(idx),]
)

expect_equal(
  c("2022"=lsf2/lsf1, "2021-12"=1, "2021"=lsf3/lsf2, "2021-06"=lsf4/lsf2),
  dt2[, lsf(x=old, x.new=new, t=time, t.overlap=c("2022","2021-12","2021","2021-06"))]
)


### Dealing with breaks/gaps in time series


dt3 <- copy(dt0)

## (1) time periods available but index value missing (NA)
idx <- dt3[, time%in%c("2021-01-01","2021-02-01")]
dt3[idx, old:=NA]

expect_equal(
  as.matrix(data.table(
    "2022"=dt3$old, 
    "2021-12"=ifelse(dt3[, is.na(old) & time<"2022-01-01"], NA, res2), 
    "2021"=dt3$old,
    "2021-06"=ifelse(dt3[, is.na(old) & time<"2022-01-01"], NA, res4))),
  a1 <- dt3[, link(x=old, x.new=new, t=time, t.overlap=c("2022","2021-12","2021","2021-06"))]
)

expect_equal(
  c("2022"=NA, "2021-12"=1, "2021"=NA, "2021-06"=lsf4/lsf2),
  a2 <- dt3[, lsf(x=old, x.new=new, t=time, t.overlap=c("2022","2021-12","2021","2021-06"))]
)

expect_equal(
  dt3[, ifelse(time<"2022-01-01", old, new*mean(old[year(time)==2021], na.rm=TRUE)/mean(new[year(time)==2021], na.rm=TRUE))],
  a3 <- dt3[, link(x=old, x.new=new, t=time, t.overlap="2021", settings=list(na.rm=TRUE))]
)

expect_equal(
  c("2021"=dt3[, (mean(old[year(time)==2021], na.rm=TRUE)/mean(new[year(time)==2021], na.rm=TRUE)) / lsf2]),
  a4 <- dt3[, lsf(x=old, x.new=new, t=time, t.overlap="2021", settings=list(na.rm=TRUE))]
)

## (2) time periods and index values not available
dt4 <- dt3[!idx,]

expect_equal(
  as.matrix(data.table(
    "2022"=dt4$old, 
    "2021-12"=res2[!dt3[, is.na(old) & time<"2022-01-01"]], 
    "2021"=dt4$old,
    "2021-06"=res4[!dt3[, is.na(old) & time<"2022-01-01"]])),
  b1 <- dt4[, link(x=old, x.new=new, t=time, t.overlap=c("2022","2021-12","2021","2021-06"))]
)

expect_equal(a1[!idx,], b1)

expect_equal(
  c("2022"=NA, "2021-12"=1, "2021"=NA, "2021-06"=lsf4/lsf2),
  b2 <- dt4[, lsf(x=old, x.new=new, t=time, t.overlap=c("2022","2021-12","2021","2021-06"))]
)

expect_equal(a2, b2)

expect_equal(
  dt4[, ifelse(time<"2022-01-01", old, new*mean(old[year(time)==2021], na.rm=TRUE)/mean(new[year(time)==2021], na.rm=TRUE))],
  b3 <- dt4[, link(x=old, x.new=new, t=time, t.overlap="2021", settings=list(na.rm=TRUE))]
)

expect_equal(
  c("2021"=dt4[, (mean(old[year(time)==2021], na.rm=TRUE)/mean(new[year(time)==2021], na.rm=TRUE)) / lsf2]),
  b4 <- dt4[, lsf(x=old, x.new=new, t=time, t.overlap="2021", settings=list(na.rm=TRUE))]
)

# if we drop the periods in idx even though there is an index
# value present for p.new (which we drop as well), we cannot
# expect that a3=b3 and a4=b4


### Quarterly and annual data


# quarterly data (note that days and months do not refer to last day in quarter):
t <- seq.Date(from=as.Date("2020-02-01"), to=as.Date("2023-11-01"), by="3 month")
t[length(t)] <- as.Date("2023-12-17")
p <- rnorm(n=length(t), mean=100, sd=5) # current index
p.new <- p+rnorm(n=length(p), mean=0, sd=1) # new index
p[9:16] <- NA # current index stops in 2021

expect_equal(
  cbind(
    "2020-12"=c(p[1:8], rep(NA, 8)),
    "2020"=c(p[1:8], (p.new*mean(p[1:4])/mean(p.new[1:4]))[9:16]),
    "2020-11"=c(p[1:8], (p.new*mean(p[t=="2020-11-01"])/mean(p.new[t=="2020-11-01"]))[9:16])
    ),
  link(x=p, x.new=p.new, t=t, t.overlap=c("2020-12","2020","2020-11"))
)

expect_equal(
  c(
    "2020-12"=NA,
    "2020"=(mean(p[1:4])/mean(p.new[1:4]))/(p[t=="2021-11-01"]/p.new[t=="2021-11-01"]),
    "2020-11"=(p[t=="2020-11-01"]/p.new[t=="2020-11-01"])/(p[t=="2021-11-01"]/p.new[t=="2021-11-01"])
  ),
  lsf(x=p, x.new=p.new, t=t, t.overlap=c("2020-12","2020","2020-11"))
)

# annual data (note that days and months do not refer to last day in quarter):
t <- seq.Date(from=as.Date("2020-02-01"), to=as.Date("2025-02-01"), by="12 month")
t[length(t)] <- as.Date("2025-08-13")
p <- rnorm(n=length(t), mean=100, sd=5) # current index
p.new <- p+rnorm(n=length(p), mean=0, sd=1) # new index
p[4:6] <- NA # current index stops in 2022

expect_equal(
  cbind(
    "2020-12"=c(p[1:3], rep(NA, 3)),
    "2020"=c(p[1:3], (p.new*p[t=="2020-02-01"]/p.new[t=="2020-02-01"])[4:6]),
    "2021-02"=c(p[1:3], (p.new*p[t=="2021-02-01"]/p.new[t=="2021-02-01"])[4:6])
  ),
  link(x=p, x.new=p.new, t=t, t.overlap=c("2020-12","2020","2021-02"))
)

expect_equal(
  c(
    "2020-12"=NA,
    "2020"=(p[t=="2020-02-01"]/p.new[t=="2020-02-01"])/(p[t=="2022-02-01"]/p.new[t=="2022-02-01"]),
    "2021-02"=(p[t=="2021-02-01"]/p.new[t=="2021-02-01"])/(p[t=="2022-02-01"]/p.new[t=="2022-02-01"])
  ),
  lsf(x=p, x.new=p.new, t=t, t.overlap=c("2020-12","2020","2021-02"))
)

# END
