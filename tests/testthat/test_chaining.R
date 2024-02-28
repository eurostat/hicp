# START


# Functions unchain() and chain() -----------------------------------------


### (A) index series from december to december:
t <- seq.Date(from=as.Date("2021-12-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

# december chain-linking:
expect_equal(
  100*p/p[1],
  chain(unchain(p,t, by=12), t, by=12)
)

# chain-linking via january:
expect_equal(
  (100*p/p[2])[-1],
  (chain(unchain(p,t, by=1), t, by=1))[-1]
)

# chain-linking via may:
expect_equal(
  (100*p/p[6])[-c(1:5)],
  (chain(unchain(p,t, by=5), t, by=5))[-c(1:5)]
)

# chain-linking via annual average:
expect_equal(
  (100*p/mean(p[2:13]))[-c(1:13)],
  chain(unchain(p, t, by=NULL), t, by=NULL)[-c(1:13)]
)


### (B) index series from May to December:
t <- seq.Date(from=as.Date("2021-05-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

# december chain-linking:
expect_equal(
  (100*p/p[8])[-c(1:7)],
  (chain(unchain(p,t, by=12), t, by=12))[-c(1:7)]
)

# chain-linking via january:
expect_equal(
  (100*p/p[9])[-c(1:8)],
  (chain(unchain(p,t, by=1), t, by=1))[-c(1:8)]
)

# chain-linking via may:
expect_equal(
  100*p/p[1],
  chain(unchain(p,t, by=5), t, by=5)
)

# chain-linking via annual average:
expect_equal(
  (100*p/mean(p[9:20]))[-c(1:20)],
  (chain(unchain(p, t, by=NULL), t, by=NULL))[-c(1:20)]
)


### (C) index series from December to May:
t <- seq.Date(from=as.Date("2022-12-01"), to=as.Date("2024-05-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

# december chain-linking:
expect_equal(
  100*p/p[1],
  chain(unchain(p,t, by=12), t, by=12)
)

# chain-linking via january:
expect_equal(
  (100*p/p[2])[-1],
  (chain(unchain(p,t, by=1), t, by=1))[-1]
)

# chain-linking via may:
expect_equal(
  (100*p/p[6])[-c(1:5)],
  (chain(unchain(p,t, by=5), t, by=5))[-c(1:5)]
)

# chain-linking via annual average:
expect_equal(
  (100*p/mean(p[2:13]))[-c(1:13)],
  (chain(unchain(p, t, by=NULL), t, by=NULL))[-c(1:13)]
)


### (D) index series from May to May:
t <- seq.Date(from=as.Date("2021-05-01"), to=as.Date("2024-05-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

# december chain-linking:
expect_equal(
  (100*p/p[8])[-c(1:7)],
  (chain(unchain(p,t, by=12), t, by=12))[-c(1:7)]
)

# chain-linking via january:
expect_equal(
  (100*p/p[9])[-c(1:8)],
  (chain(unchain(p,t, by=1), t, by=1))[-c(1:8)]
)

# chain-linking via may:
expect_equal(
  100*p/p[1],
  chain(unchain(p,t, by=5), t, by=5)
)

# chain-linking via annual average:
expect_equal(
  (100*p/mean(p[9:20]))[-c(1:20)],
  (chain(unchain(p, t, by=NULL), t, by=NULL))[-c(1:20)]
)


# (E) index series from January to December:
t <- seq.Date(from=as.Date("2021-01-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

# december chain-linking:
expect_equal(
  (100*p/p[12])[-c(1:12)],
  (chain(unchain(p,t, by=12), t, by=12))[-c(1:12)]
)

# chain-linking via january:
expect_equal(
  100*p/p[1],
  chain(unchain(p,t, by=1), t, by=1)
)

# chain-linking via may:
expect_equal(
  (100*p/p[5])[-c(1:4)],
  (chain(unchain(p,t, by=5), t, by=5))[-c(1:4)]
)

# chain-linking via annual average:
expect_equal(
  (100*p/mean(p[1:12]))[-c(1:12)],
  (chain(unchain(p, t, by=NULL), t, by=NULL))[-c(1:12)]
)


### Dealing with breaks/missings in time series

t <- seq.Date(from=as.Date("2017-12-01"), to=as.Date("2024-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

# December overlap:
df <- data.frame(t,p)
df$p[26:48] <- NA # introduce break
df$unchained <- unchain(x=df$p, t=df$t)
df$chained <- chain(df$unchained, df$t)
df$p_adj <- NA
df$p_adj[1:25] <- 100*df$p[1:25]/df$p[1]
df$p_adj[49:85] <- 100*df$p[49:85]/df$p[49]

expect_equal(
  df$p_adj,
  df$chained
)

# annual overlap:
df <- data.frame(t,p)
df$p[26:49] <- NA # introduce break
df$unchained <- unchain(x=df$p, t=df$t, by=NULL)
df$chained <- chain(df$unchained, t=df$t, by=NULL)
df$p_adj <- NA
df$p_adj[1:25] <- 100*df$p[1:25]/mean(df$p[2:13])
df$p_adj[50:85] <- 100*df$p[50:85]/mean(df$p[50:61])

expect_equal(
  df$chained[c(14:49,62:85)],
  df$p_adj[c(14:49,62:85)]
)


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
