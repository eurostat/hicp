# START

options("hicp.chatty"=FALSE)


# set.coicop() ------------------------------------------------------------


expect_no_error(set.coicop("ecoicop"))
expect_no_error(set.coicop("ecoicop.hicp"))
expect_no_error(set.coicop("coicop1999"))
expect_no_error(set.coicop("coicop2018"))


# keep.bundle() -----------------------------------------------------------


expect_equal(
  c(T,T,T,T,T),
  keep.bundle(id=c(NA,"00","08","081","08X"))
)

expect_equal(
  c(T,T,T,T,T,F),
  keep.bundle(id=c(NA,"00","08","081","08X","082"))
)

expect_equal(
  c(T,T,T,T,F,T,T),
  keep.bundle(id=c(NA,"00","08","081","08X","082","083"))
)

expect_equal(
  c(T,T,T,T,T,T),
  keep.bundle(id=c(NA,"00","08","081","082","083"))
)

# now with by given:
expect_equal(
  c(T,T,T, T,T,T),
  keep.bundle(id=c("08","081","08X", "08","081","08X"), 
              by=rep(1:2, each=3))
)

expect_equal(
  c(T,T,T,F, T,T,T,F),
  keep.bundle(id=c("08","081","08X","082", "08","081","08X","082"),
              by=rep(1:2, each=4))
)

expect_equal(
  c(T,T,F,T,T, T,T,F,T,T),
  keep.bundle(id=c("08","081","08X","082","083", "08","081","08X","082","083"),
              by=rep(1:2, each=5))
)

expect_equal(
  c(T,T,T,F, T,T,T,F,F),
  keep.bundle(id=c("08","081","08X","082", "08","081","08X","082","083"),
              by=rep(1:2, times=c(4,5)))
)

expect_equal(
  c(T,T,F,T, T,T,T,T),
  keep.bundle(id=c("08","081","08X","082", "08","081","082","083"),
              by=rep(1:2, each=4))
)


# nperiods() --------------------------------------------------------------


# monthly frequency:
expect_equal(12L, nperiods(t=as.Date(c("2020-01-17","2020-02-19"))))
expect_equal(12L, nperiods(t=as.Date(c("2020-01-17","2020-02-19","2020-03-01"))))
expect_equal(12L, nperiods(t=as.Date(c("2020-01-17","2020-02-19","2020-07-01"))))

# quarterly frequency:
expect_equal(4L, nperiods(t=as.Date(c("2020-01-17","2020-04-01"))))
expect_equal(4L, nperiods(t=as.Date(c("2020-01-17","2020-04-01","2020-07-15"))))
expect_equal(4L, nperiods(t=as.Date(c("2020-01-17","2020-04-01","2020-12-15"))))

# yearly frequency:
expect_equal(1L, nperiods(t=as.Date(c("2020-01-17","2021-01-01"))))
expect_equal(1L, nperiods(t=as.Date(c("2020-01-17","2021-01-01","2022-01-01"))))
expect_equal(1L, nperiods(t=as.Date(c("2020-01-17","2021-01-01","2023-01-01"))))


# pin.month(), pin.month.int() and pin.date() -----------------------------


t <- as.Date(c("2020-01-13","2020-03-01","2020-04-03","2021-05-01","2021-09-13"))
expect_equal(c(1,3,4,5,9), pin.month(t, freq=12L)) # monthly
expect_equal(c(3,3,6,6,9), pin.month(t, freq=4L)) # quarterly
expect_equal(c(12,12,12,12,12), pin.month(t, freq=1L)) # yearly

m <- as.integer(format(t, "%m"))
expect_equal(c(1,3,4,5,9), pin.month.int(m, freq=12L)) # monthly
expect_equal(c(3,3,6,6,9), pin.month.int(m, freq=4L)) # quarterly
expect_equal(c(12,12,12,12,12), pin.month.int(m, freq=1L)) # yearly

expect_equal(as.Date(format(t, "%Y-%m-01")), pin.date(t, freq=12L)) # monthly
expect_equal(
  as.Date(c("2020-03-01","2020-03-01","2020-06-01","2021-06-01","2021-09-01")),
  pin.date(t, freq=4L)
) # quarterly
expect_equal(as.Date(format(t, "%Y-12-01")), pin.date(t, freq=1L)) # yearly


# lag.yearmonth() ---------------------------------------------------------


t <- as.Date(c("2020-01-13","2020-03-01","2020-04-03","2021-05-01","2021-09-13"))
y <- as.integer(format(t, "%Y"))
m <- as.integer(format(t, "%m"))

# no shift:
expect_equal(
  format(t, "%Y-%m"),
  lag.yearmonth(y=y, m=m, n=0)
)

# lag by 1 month:
expect_equal(
  c("2019-12","2020-02","2020-03","2021-04","2021-08"),
  lag.yearmonth(y=y, m=m, n=1)
)

# lag by 12 months:
expect_equal(
  c("2019-01","2019-03","2019-04","2020-05","2020-09"),
  lag.yearmonth(y=y, m=m, n=12)
)


# navg() ------------------------------------------------------------------


# missings not ok (=> na.rm=FALSE)

# no NAs and required number of observations present:
x <- 1:20
g <- rep(1:5, each=4)
expect_false(any(is.na(navg(x=x, g=g, n=4, na.rm=FALSE))))

# NAs present, required number of observations present:
x <- 1:20
x[1] <- NA
g <- rep(1:5, each=4)
expect_true(any(is.na(navg(x=x, g=g, n=4, na.rm=FALSE))))

# no NAs, required number of observations not present:
x <- 2:20
g <- rep(1:5, c(3,4,4,4,4))
expect_true(any(is.na(navg(x=x, g=g, n=4, na.rm=FALSE))))

# missings dropped (=> na.rm=TRUE)

# no NAs and required number of observations present:
x <- 1:20
g <- rep(1:5, each=4)
expect_false(any(is.na(navg(x=x, g=g, n=4, na.rm=TRUE))))

# NAs present, required number of observations present:
x <- 1:20
x[1] <- NA
g <- rep(1:5, each=4)
expect_false(any(is.na(navg(x=x, g=g, n=4, na.rm=TRUE))))

# no NAs, required number of observations not present:
x <- 2:20
g <- rep(1:5, c(3,4,4,4,4))
expect_false(any(is.na(navg(x=x, g=g, n=4, na.rm=TRUE))))

# END
