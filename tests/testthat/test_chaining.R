# START

# set global options:
options(hicp.chatty=FALSE)


# Functions unchain() and chain() -----------------------------------------


### Consecutive time periods without series breaks

## (1) index series from december to december:
t <- seq.Date(from=as.Date("2021-12-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)
p[c(27,35)] <- NA

# december chain-linking:
expect_equal(
  100*p/p[1],
  chain(unchain(p,t, by=12), t, by=12)
)

# chain-linking via january:
expect_equal(
  c(NA, (100*p/p[2])[-1]),
  chain(unchain(p,t, by=1), t, by=1)
)

# chain-linking via may:
expect_equal(
  c(rep(NA, 5), (100*p/p[6])[-c(1:5)]),
  chain(unchain(p,t, by=5), t, by=5)
)

# chain-linking via annual average:
expect_equal(
  c(NA, rep(100, 12), (100*p/mean(p[2:13]))[-c(1:13)]),
  chain(unchain(p, t, by=NULL), t, by=NULL)
)


## (2) index series from May to December:
t <- seq.Date(from=as.Date("2021-05-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)
p[c(27,35)] <- NA

# december chain-linking:
expect_equal(
  c(rep(NA,7), (100*p/p[8])[-c(1:7)]),
  chain(unchain(p,t, by=12), t, by=12)
)

# chain-linking via january:
expect_equal(
  c(rep(NA,8), (100*p/p[9])[-c(1:8)]),
  chain(unchain(p,t, by=1), t, by=1)
)

# chain-linking via may:
expect_equal(
  100*p/p[1],
  chain(unchain(p,t, by=5), t, by=5)
)

# chain-linking via annual average:
expect_equal(
  c(rep(NA,8), rep(100,12), (100*p[21:32]/mean(p[9:20])), rep(NA,12)),
  chain(unchain(p, t, by=NULL), t, by=NULL)
)

# chain-linking via annual average with na.rm=T:
expect_equal(
  c(rep(100,8), (100*p[9:44]/mean(p[1:8]))),
  chain(unchain(p, t, by=NULL, settings=list(na.rm=T)), t, by=NULL, settings=list(na.rm=T))
)


## (3) index series from December to May:
t <- seq.Date(from=as.Date("2020-12-01"), to=as.Date("2024-05-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)
p[c(27,35)] <- NA

# december chain-linking:
expect_equal(
  100*p/p[1],
  chain(unchain(p,t, by=12), t, by=12)
)

# chain-linking via january:
expect_equal(
  c(NA, (100*p/p[2])[-1]),
  chain(unchain(p,t, by=1), t, by=1)
)

# chain-linking via may:
expect_equal(
  c(rep(NA, 5), (100*p/p[6])[-c(1:5)]),
  chain(unchain(p,t, by=5), t, by=5)
)

# chain-linking via annual average:
expect_equal(
  c(NA, rep(100, 12), (100*p[14:37]/mean(p[2:13])), rep(NA,5)),
  chain(unchain(p, t, by=NULL), t, by=NULL)
)

# chain-linking via annual average with na.rm=T:
expect_equal(
  100*p/p[1],
  chain(unchain(p, t, by=NULL, settings=list(na.rm=T)), t, by=NULL, settings=list(na.rm=T))
)

## (4) index series from May to May:
t <- seq.Date(from=as.Date("2021-05-01"), to=as.Date("2024-05-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)
p[c(27,35)] <- NA

# december chain-linking:
expect_equal(
  c(rep(NA,7), (100*p/p[8])[-c(1:7)]),
  chain(unchain(p,t, by=12), t, by=12)
)

# chain-linking via january:
expect_equal(
  c(rep(NA,8), (100*p/p[9])[-c(1:8)]),
  chain(unchain(p,t, by=1), t, by=1)
)

# chain-linking via may:
expect_equal(
  100*p/p[1],
  chain(unchain(p,t, by=5), t, by=5)
)

# chain-linking via annual average:
expect_equal(
  c(rep(NA,8), rep(100,12), (100*p[21:32]/mean(p[9:20])), rep(NA,5)),
  chain(unchain(p, t, by=NULL), t, by=NULL)
)

# chain-linking via annual average with na.rm=T:
expect_equal(
  c(rep(100,8), 100*p[9:37]/mean(p[1:8], na.rm=TRUE)),
  chain(unchain(p, t, by=NULL, settings=list(na.rm=T)), t, by=NULL, settings=list(na.rm=T))
)

## (5) index series from January to December:
t <- seq.Date(from=as.Date("2021-01-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)
p[c(27,35)] <- NA

# december chain-linking:
expect_equal(
  c(rep(NA,11), (100*p/p[12])[-c(1:11)]),
  chain(unchain(p,t, by=12), t, by=12)
)

# chain-linking via january:
expect_equal(
  100*p/p[1],
  chain(unchain(p,t, by=1), t, by=1)
)

# chain-linking via may:
expect_equal(
  c(rep(NA,4), (100*p/p[5])[-c(1:4)]),
  chain(unchain(p,t, by=5), t, by=5)
)

# chain-linking via annual average:
expect_equal(
  c(rep(100,12), (100*p[13:36]/mean(p[1:12])), rep(NA,12)),
  chain(unchain(p, t, by=NULL), t, by=NULL)
)

# chain-linking via annual average with na.rm=T:
expect_equal(
  c(rep(100,12), 100*p[13:48]/mean(p[1:12], na.rm=TRUE)),
  chain(unchain(p, t, by=NULL, settings=list(na.rm=T)), t, by=NULL, settings=list(na.rm=T))
)

### Time periods in random, non-chronological order without series breaks

## (1) index series from december to december:
t <- seq.Date(from=as.Date("2021-12-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)
p[c(27,35)] <- NA
idx <- sample(1:length(t)) # random ordering of time periods

# december chain-linking:
expect_equal(
  100*p/p[1],
  chain(unchain(p[idx], t[idx], by=12), t[idx], by=12)[order(idx)]
)

# chain-linking via january:
expect_equal(
  c(NA, (100*p/p[2])[-1]),
  chain(unchain(p[idx], t[idx], by=1), t[idx], by=1)[order(idx)]
)

# chain-linking via may:
expect_equal(
  c(rep(NA, 5), (100*p/p[6])[-c(1:5)]),
  chain(unchain(p[idx], t[idx], by=5), t[idx], by=5)[order(idx)]
)

# chain-linking via annual average:
expect_equal(
  c(NA, rep(100, 12), (100*p/mean(p[2:13]))[-c(1:13)]),
  chain(unchain(p[idx], t[idx], by=NULL), t[idx], by=NULL)[order(idx)]
)


## (2) index series from May to December:
t <- seq.Date(from=as.Date("2021-05-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)
p[c(27,35)] <- NA
idx <- sample(1:length(t)) # random ordering of time periods

# december chain-linking:
expect_equal(
  c(rep(NA,7), (100*p/p[8])[-c(1:7)]),
  chain(unchain(p[idx], t[idx], by=12), t[idx], by=12)[order(idx)]
)

# chain-linking via january:
expect_equal(
  c(rep(NA,8), (100*p/p[9])[-c(1:8)]),
  chain(unchain(p[idx], t[idx], by=1), t[idx], by=1)[order(idx)]
)

# chain-linking via may:
expect_equal(
  100*p/p[1],
  chain(unchain(p[idx], t[idx], by=5), t[idx], by=5)[order(idx)]
)

# chain-linking via annual average:
expect_equal(
  c(rep(NA,8), rep(100,12), (100*p[21:32]/mean(p[9:20])), rep(NA,12)),
  chain(unchain(p[idx], t[idx], by=NULL), t[idx], by=NULL)[order(idx)]
)

### Dealing with breaks/gaps in time series

## (1) series break of more than one year

## (a) time periods available but index value missing (NA):
t <- seq.Date(from=as.Date("2017-12-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)
df <- data.frame(t,p)
idx <- 26:48
df$p[idx] <- NA # introduce break of two years
df$p[c(15,70)] <- NA # random NAs

# December overlap:
df$unchained <- unchain(x=df$p, t=df$t)
df$chained <- a1 <- chain(df$unchained, df$t)
df$p_adj <- NA
df$p_adj[1:25] <- 100*df$p[1:25]/df$p[1]
df$p_adj[49:85] <- 100*df$p[49:85]/df$p[49]
expect_equal(df$p_adj, df$chained)

# July overlap:
df$unchained <- unchain(x=df$p, t=df$t, by=7)
df$chained <- a2 <- chain(df$unchained, df$t, by=7)
df$p_adj <- NA
df$p_adj[8:25] <- 100*df$p[8:25]/df$p[8]
df$p_adj[56:85] <- 100*df$p[56:85]/df$p[56]
expect_equal(df$p_adj, df$chained)

# annual overlap:
df$unchained <- unchain(x=df$p, t=df$t, by=NULL)
df$chained <- a3 <- chain(df$unchained, t=df$t, by=NULL)
df$p_adj <- NA
df$p_adj[14:25] <- 100*df$p[14:25]/mean(df$p[2:13])
df$p_adj[62:73] <- 100*df$p[62:73]/mean(df$p[50:61])
df$p_adj[c(2:13,50:61)] <- 100
expect_equal(df$p_adj, df$chained)

# annual overlap with na.rm=TRUE:
df$unchained <- unchain(x=df$p, t=df$t, by=NULL, settings=list(na.rm=T))
df$chained <- a4 <- chain(df$unchained, t=df$t, by=NULL, settings=list(na.rm=T))
df$p_adj <- NA
df$p_adj[2:37] <- 100*df$p[2:37]/mean(df$p[1], na.rm=T)
df$p_adj[50:85] <- 100*df$p[50:85]/mean(df$p[38:49], na.rm=T)
df$p_adj[c(1,38:49)] <- 100
expect_equal(df$p_adj, df$chained)

## (b) time periods and index values not available
df <- data.frame(t,p)
df$p[c(15,70)] <- NA # random NAs
df <- df[-idx,] # introduce break but drop time periods

# December overlap:
df$p_adj <- NA
df$p_adj[1:25] <- 100*df$p[1:25]/df$p[1]
df$p_adj[26:62] <- 100*df$p[26:62]/df$p[26]
df$unchained <- unchain(x=df$p, t=df$t)
df$chained <- b1 <- chain(df$unchained, df$t)
expect_equal(df$p_adj, df$chained)
expect_equal(a1[-idx], b1)

# July overlap:
df$p_adj <- NA
df$p_adj[8:25] <- 100*df$p[8:25]/df$p[8]
df$p_adj[33:62] <- 100*df$p[33:62]/df$p[33]
df$unchained <- unchain(x=df$p, t=df$t, by=7)
df$chained <- b2 <- chain(df$unchained, df$t, by=7)
expect_equal(df$p_adj, df$chained)
expect_equal(a2[-idx], b2)

# annual overlap:
df$p_adj <- NA
df$p_adj[14:25] <- 100*df$p[14:25]/mean(df$p[2:13])
df$p_adj[39:50] <- 100*df$p[39:50]/mean(df$p[27:38])
df$p_adj[c(2:13,27:38)] <- 100
df$unchained <- unchain(x=df$p, t=df$t, by=NULL)
df$chained <- b3 <- chain(df$unchained, t=df$t, by=NULL)
expect_equal(df$chained, df$p_adj)
expect_equal(a3[-idx], b3)

# annual overlap with na.rm=TRUE:
df$p_adj <- NA
df$p_adj[2:25] <- 100*df$p[2:25]/mean(df$p[1], na.rm=T)
df$p_adj[27:62] <- 100*df$p[27:62]/mean(df$p[26], na.rm=T)
df$p_adj[c(1,26)] <- 100
df$unchained <- unchain(x=df$p, t=df$t, by=NULL, settings=list(na.rm=T))
df$chained <- b4 <- chain(df$unchained, t=df$t, by=NULL, settings=list(na.rm=T))
expect_equal(df$chained, df$p_adj)
expect_equal(a4[-idx], b4)

## (2) series break of one year

## (a) time periods available but index value missing (NA):
t <- seq.Date(from=as.Date("2017-12-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)
df <- data.frame(t,p)
idx <- 26:36
df$p[idx] <- NA # introduce break of two years
df$p[c(15,70)] <- NA # random NAs

# December overlap:
df$p_adj <- NA
df$p_adj[1:25] <- 100*df$p[1:25]/df$p[1]
df$p_adj[37:85] <- 100*df$p[37:85]/df$p[37]
df$unchained <- unchain(x=df$p, t=df$t, by=12)
df$chained <- a1 <- chain(df$unchained, df$t, by=12)
expect_equal(df$p_adj, df$chained)

# July overlap:
df$p_adj <- NA
df$p_adj[8:25] <- 100*df$p[8:25]/df$p[8]
df$p_adj[44:85] <- 100*df$p[44:85]/df$p[44]
df$unchained <- unchain(x=df$p, t=df$t, by=7)
df$chained <- a2 <- chain(df$unchained, df$t, by=7)
expect_equal(df$p_adj, df$chained)

# annual overlap:
df$p_adj <- NA
df$p_adj[14:25] <- 100*df$p[14:25]/mean(df$p[2:13])
df$p_adj[50:73] <- 100*df$p[50:73]/mean(df$p[38:49])
df$p_adj[c(2:13,38:49)] <- 100
df$unchained <- unchain(x=df$p, t=df$t, by=NULL)
df$chained <- a3 <- chain(df$unchained, t=df$t, by=NULL)
expect_equal(df$chained, df$p_adj)

# annual overlap with na.rm=TRUE:
df$p_adj <- NA
df$p_adj[2:25] <- 100*df$p[2:25]/mean(df$p[1], na.rm=T)
df$p_adj[38:85] <- 100*df$p[38:85]/mean(df$p[26:37], na.rm=T)
df$p_adj[c(1,26:37)] <- 100
df$unchained <- unchain(x=df$p, t=df$t, by=NULL, settings=list(na.rm=T))
df$chained <- a4 <- chain(df$unchained, t=df$t, by=NULL, settings=list(na.rm=T))
expect_equal(df$chained, df$p_adj)

## (b) time periods and index values not available
df <- data.frame(t,p)
df$p[c(15,70)] <- NA # random NAs
df <- df[-idx,] # introduce break but drop time periods

# December overlap:
df$p_adj <- NA
df$p_adj[1:25] <- 100*df$p[1:25]/df$p[1]
df$p_adj[26:74] <- 100*df$p[26:74]/df$p[26]
df$unchained <- unchain(x=df$p, t=df$t, by=12)
df$chained <- b1 <- chain(df$unchained, df$t, by=12)
expect_equal(df$p_adj, df$chained)
expect_equal(a1[-idx], b1)

# July overlap:
df$p_adj <- NA
df$p_adj[8:25] <- 100*df$p[8:25]/df$p[8]
df$p_adj[33:74] <- 100*df$p[33:74]/df$p[33]
df$unchained <- unchain(x=df$p, t=df$t, by=7)
df$chained <- b2 <- chain(df$unchained, df$t, by=7)
expect_equal(df$p_adj, df$chained)
expect_equal(a2[-idx], b2)

# annual overlap:
df$p_adj <- NA
df$p_adj[14:25] <- 100*df$p[14:25]/mean(df$p[2:13])
df$p_adj[39:62] <- 100*df$p[39:62]/mean(df$p[27:38])
df$p_adj[c(2:13,27:38)] <- 100
df$unchained <- unchain(x=df$p, t=df$t, by=NULL)
df$chained <- b3 <- chain(df$unchained, t=df$t, by=NULL)
expect_equal(df$chained, df$p_adj)
expect_equal(a3[-idx], b3)

# annual overlap with na.rm=TRUE:
df$p_adj <- NA
df$p_adj[2:25] <- 100*df$p[2:25]/mean(df$p[1], na.rm=T)
df$p_adj[27:74] <- 100*df$p[27:74]/mean(df$p[26], na.rm=T)
df$p_adj[c(1,26)] <- 100
df$unchained <- unchain(x=df$p, t=df$t, by=NULL, settings=list(na.rm=T))
df$chained <- b4 <- chain(df$unchained, t=df$t, by=NULL, settings=list(na.rm=T))
expect_equal(df$chained, df$p_adj)
expect_equal(a4[-idx], b4)


### Quarterly and annual data

# quarterly data (note that days and months do not refer to last day in quarter):
t <- seq.Date(from=as.Date("2019-11-17"), to=as.Date("2024-11-17"), by="3 months")
p <- runif(n=length(t), min=90, max=110)
p <- p/p[1]*100

# chain-linking via fourth quarter:
expect_equal(
  chain(x=unchain(x=p, t=t, by=12), t=t, by=12),
  p
)

# chain-linking via second quarter:
expect_equal(
  chain(x=unchain(x=p, t=t, by=6), t=t, by=6),
  c(rep(NA,2), 100*p[3:21]/p[3])
)

# chain-linking via quarterly average:
expect_equal(
  chain(x=unchain(x=p, t=t, by=NULL), t=t, by=NULL),
  c(rep(NA,1), rep(100,4), 100*p[6:21]/mean(p[2:5]))
)

# annual data (note that day and month do not refer to last day in year):
t <- seq.Date(from=as.Date("2019-07-17"), to=as.Date("2024-07-17"), by="12 months")
p <- runif(n=length(t), min=90, max=110)
p <- p/p[1]*100

# chain-linking via december:
expect_equal(
  chain(x=unchain(x=p, t=t, by=12), t=t, by=12),
  p
)

# chain-linking via another month:
expect_equal(
  chain(x=unchain(x=p, t=t, by=6), t=t, by=6),
  p
)

# chain-linking via annual average:
expect_equal(
  chain(x=unchain(x=p, t=t, by=NULL), t=t, by=NULL),
  p
)


# Function rebase() -------------------------------------------------------


### Consecutive time periods without series breaks
t <- seq.Date(from=as.Date("2015-01-01"), to=as.Date("2020-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

expect_equal(
  p,
  rebase(x=p, t=t, t.ref="2014-01")
)

expect_equal(
  100*p/p[1],
  rebase(x=p, t=t, t.ref="first")
)

expect_equal(
  100*p/p[1],
  rebase(x=p, t=t, t.ref="2015-01")
)

expect_equal(
  100*p/mean(p[1:12]),
  rebase(x=p, t=t, t.ref="2015")
)

expect_equal(
  100*p/p[1],
  rebase(x=p, t=t, t.ref=c("2014-01","2015-01","2015"))
)

### Time periods in random, non-chronological order without series breaks

# random ordering of time periods:
idx <- sample(1:length(t))

expect_equal(
  p,
  rebase(x=p[idx], t=t[idx], t.ref="2014-01")[order(idx)]
)

expect_equal(
  100*p/p[1],
  rebase(x=p[idx], t=t[idx], t.ref="2015-01")[order(idx)]
)

expect_equal(
  100*p/p[length(t)],
  rebase(x=p, t=t, t.ref="last")
)

expect_equal(
  100*p/mean(p[1:12]),
  rebase(x=p[idx], t=t[idx], t.ref="2015")[order(idx)]
)

expect_equal(
  100*p/p[1],
  rebase(x=p[idx], t=t[idx], t.ref=c("2014-01","2015-01","2015"))[order(idx)]
)

### Dealing with breaks/gaps in time series

t <- seq.Date(from=as.Date("2015-01-01"), to=as.Date("2020-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

## (1) time periods available but index value missing (NA)

# first half of 2015 and full 2017 with index values NA:
idx <- c(1:6,25:36) 
p[idx] <- NA

expect_equal(
  p,
  rebase(x=p, t=t, t.ref="2015-01")
)

expect_equal(
  p,
  rebase(x=p, t=t, t.ref="2015")
)

expect_equal(
  100*p/mean(p[7:12]),
  a1 <- rebase(x=p, t=t, t.ref="2015", settings=list(na.rm=TRUE))
)

expect_equal(
  100*p/p[7],
  a2 <- rebase(x=p, t=t, t.ref="2015-07")
)

expect_equal(
  100*p/p[7],
  rebase(x=p, t=t, t.ref=c("2015-01","2015","2015-07"))
)

expect_equal(
  100*p/mean(p[7:12]),
  rebase(x=p, t=t, t.ref=c("2015-01","2015","2015-07"), settings=list(na.rm=TRUE))
)

## (2) time periods and index values not available

# first half of 2015 and full 2017 dropped:
p <- p[-idx] 
t <- t[-idx]

expect_equal(
  p,
  rebase(x=p, t=t, t.ref="2015-01")
)

expect_equal(
  p,
  rebase(x=p, t=t, t.ref="2015")
)

expect_equal(
  100*p/mean(p[1:6]),
  b1 <- rebase(x=p, t=t, t.ref="2015", settings=list(na.rm=TRUE))
)

expect_equal(a1[-idx], b1)

expect_equal(
  100*p/p[1],
  b2 <- rebase(x=p, t=t, t.ref="2015-07")
)

expect_equal(a2[-idx], b2)

expect_equal(
  100*p/p[1],
  rebase(x=p, t=t, t.ref=c("2015-01","2015","2015-07"))
)

expect_equal(
  100*p/mean(p[1:6]),
  rebase(x=p, t=t, t.ref=c("2015-01","2015","2015-07"), settings=list(na.rm=TRUE))
)

### Quarterly and annual data

# quarterly data (note that days and months do not refer to last day in quarter):
t <- seq.Date(from=as.Date("2019-08-17"), to=as.Date("2024-08-17"), by="3 months")
p <- runif(n=length(t), min=90, max=110)
p <- p/p[1]*100

expect_equal(100*p/p[2], rebase(x=p, t=t, t.ref="2019-11"))
expect_equal(p, rebase(x=p, t=t, t.ref="2019-12"))
expect_equal(p, rebase(x=p, t=t, t.ref="2019"))
expect_equal(100*p/mean(p[1:2]), rebase(x=p, t=t, t.ref="2019", settings=list(na.rm=TRUE)))
expect_equal(100*p/mean(p[3:6]), rebase(x=p, t=t, t.ref="2020"))

# annual data (note that days and months do not refer to last day in quarter):
t <- seq.Date(from=as.Date("2019-08-17"), to=as.Date("2024-08-17"), by="12 months")
p <- runif(n=length(t), min=90, max=110)
p <- p/p[1]*100

expect_equal(p, rebase(x=p, t=t, t.ref="2019-12"))
expect_equal(100*p/p[2], rebase(x=p, t=t, t.ref="2020-08"))
expect_equal(100*p/p[2], rebase(x=p, t=t, t.ref="2020"))


# Function convert() ------------------------------------------------------


### Consecutive time periods without series breaks

## (1) index series from january to december:
t <- seq.Date(from=as.Date("2015-01-01"), to=as.Date("2020-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

expect_equal(
  c(tapply(X=p, pin.date(t, freq=1), mean)),
  convert(x=p, t=t, type="y")
)

expect_equal(
  c(tapply(X=p, pin.date(t, freq=4), mean)),
  convert(x=p, t=t, type="q")
)

expect_equal(
  data.table::frollmean(x=p, n=12, fill=NA, algo="exact", align="right", na.rm=FALSE),
  convert(x=p, t=t, type="12mavg")
)


### Time periods in random, non-chronological order without series breaks

# random ordering of time periods:
idx <- sample(1:length(t))

expect_equal(
  c(tapply(X=p, pin.date(t, freq=1), mean)),
  convert(x=p[idx], t=t[idx], type="y")
)

expect_equal(
  c(tapply(X=p, pin.date(t, freq=4), mean)),
  convert(x=p[idx], t=t[idx], type="q")
)

expect_equal(
  data.table::frollmean(x=p, n=12, fill=NA, algo="exact", align="right", na.rm=FALSE),
  convert(x=p[idx], t=t[idx], type="12mavg")[order(idx)]
)

### Dealing with breaks/gaps in time series

t <- seq.Date(from=as.Date("2015-01-01"), to=as.Date("2020-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

## (1) time periods available but index value missing (NA)

# first 5 months of 2015 and full 2017 with index values NA:
idx <- c(1:5,25:36) 
p[idx] <- NA

expect_equal(
  c(tapply(X=p, pin.date(t, freq=1), mean)),
  a1 <- convert(x=p, t=t, type="y")
)

expect_equal(
  c(tapply(X=p, pin.date(t, freq=1), mean, na.rm=T)),
  a2 <- convert(x=p, t=t, type="y", settings=list(na.rm=T))
)

expect_equal(
  c(tapply(X=p, pin.date(t, freq=4), mean)),
  a3 <- convert(x=p, t=t, type="q")
)

expect_equal(
  c(tapply(X=p, pin.date(t, freq=4), mean, na.rm=T)),
  a4 <- convert(x=p, t=t, type="q", settings=list(na.rm=T))
)

expect_equal(
  mavg1 <- data.table::frollmean(x=p, n=12, fill=NA, algo="exact", align="right", na.rm=FALSE),
  a5 <- convert(x=p, t=t, type="12mavg")
)

expect_equal(
  mavg2 <- data.table::frollmean(x=p, n=12, fill=NA, algo="exact", align="right", na.rm=TRUE),
  a6 <- convert(x=p, t=t, type="12mavg", settings=list(na.rm=T))
)

## (2) time periods and index values not available

# first 5 months of 2015 and full 2017 dropped:
p <- p[-idx] 
t <- t[-idx]

expect_equal(
  c(tapply(X=p, pin.date(t, freq=1), FUN=function(z) if(length(z)<12){NA}else{mean(z)})),
  b1 <- convert(x=p, t=t, type="y")
)

expect_equal(a1[-3], b1)

expect_equal(
  c(tapply(X=p, pin.date(t, freq=1), mean, na.rm=T)),
  b2 <- convert(x=p, t=t, type="y", settings=list(na.rm=T))
)

expect_equal(a2[-3], b2)

expect_equal(
  c(tapply(X=p, pin.date(t, freq=4), FUN=function(z) if(length(z)<3){NA}else{mean(z)})),
  b3 <- convert(x=p, t=t, type="q")
)

expect_equal(a3[-c(1,9:12)], b3)

expect_equal(
  c(tapply(X=p, pin.date(t, freq=4), mean, na.rm=T)),
  b4 <- convert(x=p, t=t, type="q", settings=list(na.rm=T))
)

expect_equal(a4[-c(1,9:12)], b4)

expect_equal(
  mavg1[-idx],
  b5 <- convert(x=p, t=t, type="12mavg")
)

expect_equal(a5[-idx], b5)

expect_equal(
  ifelse(t>="2016-05-01", mavg2[-idx], NA), # this is different now
  convert(x=p, t=t, type="12mavg", settings=list(na.rm=TRUE))
)
# for na.rm=TRUE, the output is different if the index series starts
# with NAs or if the NAs are completely removed from the data

### Quarterly and annual data

# quarterly data (note that days and months do not refer to last day in quarter):
t <- seq.Date(from=as.Date("2019-08-17"), to=as.Date("2024-08-17"), by="3 months")
p <- runif(n=length(t), min=90, max=110)
p <- p/p[1]*100

# convert in annual index:
expect_equal(
  c(tapply(
    X=p, 
    INDEX=pin.date(t, freq=1), 
    FUN=function(z){if(length(z)==4) mean(z, na.rm=FALSE) else NA_real_})),
  convert(x=p, t=t, type="y")
)

# convert in quarterly index:
expect_equal(
  c(tapply(
    X=p, 
    INDEX=pin.date(t, freq=4), 
    FUN=function(z){if(length(z)==1) mean(z, na.rm=FALSE) else NA_real_})),
  convert(x=p, t=t, type="q")
)

# convert in rolling average of same frequency:
expect_equal(
  data.table::frollmean(x=p, n=4, fill=NA, algo="exact", align="right", na.rm=FALSE),
  convert(x=p, t=t, type="12mavg")
)

# annual data (note that days and months do not refer to last day in quarter):
t <- seq.Date(from=as.Date("2019-08-17"), to=as.Date("2024-08-17"), by="12 months")
p <- runif(n=length(t), min=90, max=110)
p <- p/p[1]*100

# convert in annual index:
expect_equal(
  c(tapply(
    X=p, 
    INDEX=pin.date(t, freq=1),
    FUN=function(z){if(length(z)==1) mean(z, na.rm=FALSE) else NA_real_})),
  convert(x=p, t=t, type="y")
)

# convert in quarterly index:
expect_equal(
  c(tapply(
    X=p, 
    INDEX=pin.date(t, freq=4),
    FUN=function(z){if(length(z)==0) mean(z, na.rm=FALSE) else NA_real_})),
  convert(x=p, t=t, type="q")
)

# convert in rolling average of same frequency:
expect_equal(
  data.table::frollmean(x=p, n=1, fill=NA, algo="exact", align="right", na.rm=FALSE),
  convert(x=p, t=t, type="12mavg")
)


# Comparison to published HICP ECOICOP ver. 1 data ------------------------


# load data:
load(test_path("testdata","dthicp1m.RData"))
load(test_path("testdata","dthicp1a.RData"))

# check chain-linked indices against published data:
dtcomp <- copy(dthicp1m)
dtcomp[, "dec_ratio" := unchain(x=index, t=time), by="coicop"]
dtcomp[, "chained_index" := chain(x=dec_ratio, t=time), by="coicop"]
dtcomp[, "index_own" := rebase(x=chained_index, t=time, t.ref="2015"), by="coicop"]
expect_equal(nrow(dtcomp[!is.na(index) & abs(index-index_own)>0.01 & !(coicop=="CP07369" & year>2023),]), 0)
# there seems to be a problem in the data for CP07369 so we exclude it here

# check converted indices against published data:
dtown <- dthicp1m[, as.data.table(convert(x=index, t=time, type="y"), keep.rownames=TRUE), by="coicop"]
setnames(x=dtown, c("coicop","time","index_own"))
dtown[, "time":=as.Date(paste(year(time), "01", "01", sep="-"))]
dtcomp <- merge(x=dthicp1a, y=dtown, by=c("coicop","time"), all=TRUE)
expect_equal(nrow(dtcomp[!is.na(index) & abs(index-index_own)>0.01,]), 0)


# Comparison to published HICP ECOICOP ver. 2 data ------------------------


# load data:
load(test_path("testdata","dthicp2m.RData"))
load(test_path("testdata","dthicp2a.RData"))

# check chain-linked indices against published data:
dtcomp <- copy(dthicp2m)
dtcomp[, "dec_ratio" := unchain(x=index, t=time), by="coicop18"]
dtcomp[, "chained_index" := chain(x=dec_ratio, t=time), by="coicop18"]
dtcomp[, "index_own" := rebase(x=chained_index, t=time, t.ref="2025"), by="coicop18"]
expect_equal(nrow(dtcomp[!is.na(index) & abs(index-index_own)>0.01,]), 0)

# check converted indices against published data:
dtown <- dthicp2m[, as.data.table(convert(x=index, t=time, type="y"), keep.rownames=TRUE), by="coicop18"]
setnames(x=dtown, c("coicop18","time","index_own"))
dtown[, "time":=as.Date(paste(year(time), "01", "01", sep="-"))]
dtcomp <- merge(x=dthicp2a, y=dtown, by=c("coicop18","time"), all=TRUE)
expect_equal(nrow(dtcomp[!is.na(index) & abs(index-index_own)>0.01,]), 0)


# Comparison to published OOH data ----------------------------------------


# load data:
load(test_path("testdata","dtooh.RData"))

# check chain-linked indices against published data:
dtooh[, "dec_ratio" := unchain(x=index, t=time), by="expend"]
dtooh[, "chained_index" := chain(x=dec_ratio, t=time), by="expend"]
dtooh[, "index_own" := rebase(x=chained_index, t=time, t.ref="2015"), by="expend"]
expect_equal(nrow(dtooh[!is.na(index) & abs(index-index_own)>0.01,]), 0)

# END
