# START


# Functions unchain() and chain() -----------------------------------------


### (A) index series from december to december:
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


### (B) index series from May to December:
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
  c(rep(NA,8), rep(100,12), (100*p/mean(p[9:20]))[-c(1:20)]),
  chain(unchain(p, t, by=NULL), t, by=NULL)
)


### (C) index series from December to May:
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
  c(NA, rep(100, 12), (100*p/mean(p[2:13]))[-c(1:13)]),
  chain(unchain(p, t, by=NULL), t, by=NULL)
)


### (D) index series from May to May:
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
  c(rep(NA,8), rep(100,12), (100*p/mean(p[9:20]))[-c(1:20)]),
  chain(unchain(p, t, by=NULL), t, by=NULL)
)


# (E) index series from January to December:
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
  c(rep(100,12), (100*p/mean(p[1:12]))[-c(1:12)]),
  chain(unchain(p, t, by=NULL), t, by=NULL)
)


### Dealing with breaks/missings in time series

t <- seq.Date(from=as.Date("2017-12-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

# December overlap:
df <- data.frame(t,p)
df$p[26:48] <- NA # introduce break
df$p[c(15, 70)] <- NA # random NAs
df$unchained <- unchain(x=df$p, t=df$t)
df$chained <- chain(df$unchained, df$t)
df$p_adj <- NA
df$p_adj[1:25] <- 100*df$p[1:25]/df$p[1]
df$p_adj[49:85] <- 100*df$p[49:85]/df$p[49]

expect_equal(df$p_adj, df$chained)

# July overlap:
df <- data.frame(t,p)
df$p[26:48] <- NA # introduce break
df$p[c(15, 70)] <- NA # random NAs
df$unchained <- unchain(x=df$p, t=df$t, by=7)
df$chained <- chain(df$unchained, df$t, by=7)
df$p_adj <- NA
df$p_adj[8:25] <- 100*df$p[8:25]/df$p[8]
df$p_adj[56:85] <- 100*df$p[56:85]/df$p[56]

expect_equal(df$p_adj, df$chained)

# annual overlap:
df <- data.frame(t,p)
df$p[26:49] <- NA # introduce break
df$p[c(15, 70)] <- NA # random NAs
df$unchained <- unchain(x=df$p, t=df$t, by=NULL)
df$chained <- chain(df$unchained, t=df$t, by=NULL)
df$p_adj <- NA
df$p_adj[14:25] <- 100*df$p[14:25]/mean(df$p[2:13])
df$p_adj[62:85] <- 100*df$p[62:85]/mean(df$p[50:61])
df$p_adj[c(2:13,50:61)] <- 100

expect_equal(df$chained, df$p_adj)


# Function rebase() -------------------------------------------------------


t <- seq.Date(from=as.Date("2015-01-01"), to=as.Date("2020-05-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

expect_equal(
  p,
  rebase(x=p, t=t, t.ref="2014-01")
)

expect_equal(
  100*p/p[1],
  rebase(x=p, t=t, t.ref="2015-01")
)

expect_equal(
  100*p/p[2],
  rebase(x=p, t=t, t.ref="2015-02")
)

expect_equal(
  100*p/mean(p[1:12]),
  rebase(x=p, t=t, t.ref="2015")
)

expect_equal(
  100*p/mean(p[13:24]),
  rebase(x=p, t=t, t.ref="2016")
)

# incomplete last year:
expect_equal(
  p,
  rebase(x=p, t=t, t.ref="2020")
)

# introduce missings:
p[1] <- NA

expect_equal(
  p,
  rebase(x=p, t=t, t.ref="2015-01")
)

expect_equal(
  100*p/p[2],
  rebase(x=p, t=t, t.ref="2015-02")
)


# Function convert() ------------------------------------------------------


### (A) index series from october to december:
t <- seq.Date(from=as.Date("2021-10-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

expect_true(
  is.vector(convert(x=p, t=t, freq="annual"))
)

expect_true(
  is.vector(convert(x=p, t=t, freq="q"))
)

expect_equal(
  1,
  sum(is.na(convert(x=p, t=t, freq="annual")))
)

expect_equal(
  0,
  sum(is.na(convert(x=p, t=t, freq="quarterly")))
)

### (B) index series from january to december:
t <- seq.Date(from=as.Date("2022-01-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

expect_equal(
  0,
  sum(is.na(convert(x=p, t=t, freq="annual")))
)

expect_equal(
  0,
  sum(is.na(convert(x=p, t=t, freq="quarterly")))
)

### (C) index series from february to december:
t <- seq.Date(from=as.Date("2022-02-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

expect_equal(
  1,
  sum(is.na(convert(x=p, t=t, freq="annual")))
)

expect_equal(
  1,
  sum(is.na(convert(x=p, t=t, freq="quarterly")))
)


# END
