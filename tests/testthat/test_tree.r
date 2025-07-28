# START

options("hicp.chatty"=FALSE)


# Simple tests ------------------------------------------------------------


expect_equal(
  tree(id=as.character(c(NA, NA))),
  list()
)

expect_equal(
  tree(id=as.character(c(NA, NA)), flag=TRUE),
  c(F,F)
)

expect_equal(
  tree(id=character()),
  list()
)

expect_equal(
  tree(id=character(), flag=TRUE),
  logical()
)

expect_equal(
  tree(id=c("00")),
  list("00")
)

expect_equal(
  tree(id=c("00"), flag=TRUE),
  c(T)
)

expect_equal(
  tree(id=c("00","01","02")),
  list("00", c("01","02"))
)

expect_equal(
  tree(id=c("00","01","02"), flag=TRUE),
  c(F,T,T)
)

expect_equal(
  tree(id=c("01","02")),
  list(c("01","02"))
)

expect_equal(
  tree(id=c("01","02"), flag=TRUE),
  c(T,T)
)

expect_equal(
  tree(id=c("00","01","011","012")),
  list("00", "01", c("011","012"))
)

expect_equal(
  tree(id=c("00","01","011","012"), flag=TRUE),
  c(F,F,T,T)
)

expect_equal(
  tree(id=c("00","011","012")),
  list("00", c("011","012"))
)

expect_equal(
  tree(id=c("00","011","012"), flag=TRUE),
  c(F,T,T)
)

expect_equal(
  tree(id=c("011","012")),
  list(c("011","012"))
)

expect_equal(
  tree(id=c("011","012"), flag=TRUE),
  c(T,T)
)

expect_equal(
  tree(id=c("01","12")),
  list(c("01","12"))
)

expect_equal(
  tree(id=c("01","12"), flag=TRUE),
  c(T,T)
)

expect_equal(
  tree(id=c("01","12"), settings=list(max.lvl=1)),
  list()
)

expect_equal(
  tree(id=c("01","12"), flag=TRUE, settings=list(max.lvl=1)),
  c(F,F)
)

expect_equal(
  tree(id=c("00","01",NA,"12")),
  list("00",c("01","12"))
)

expect_equal(
  tree(id=c("00","01",NA,"12"), flag=TRUE),
  c(F,T,F,T)
)

expect_equal(
  tree(id=c("00","01","12"), w=c(1,0.2,0.3)),
  list("00","00")
)

expect_equal(
  tree(id=c("00","01","12"), w=c(1,0.2,0.3), flag=TRUE),
  c(T,F,F)
)

expect_equal(
  tree(id=c("00","01","12"), w=c(1,0.4,0.6)),
  list("00",c("01","12"))
)

expect_equal(
  tree(id=c("00","01","12"), w=c(1,0.4,0.6), flag=TRUE),
  c(F,T,T)
)

expect_equal(
  tree(id=c("00","01","011","0111","01111")),
  list("00","01","011","0111","01111")
)

expect_equal(
  tree(id=c("00","01","011","0111","01111"), flag=TRUE),
  c(F,F,F,F,T)
)


# More detailed tree ------------------------------------------------------


expect_equal(
  tree(id=c("08","081","08301","08302","12"), flag=TRUE),
  c(F,T,T,T,T)
)

expect_equal(
  tree(id=c("08","081","08301","08302","12"), w=c(0.2,0.05,0.03,0.07,0.4), flag=TRUE),
  c(T,F,F,F,T)
)

expect_equal(
  tree(id=c("08","081","08301","08302","12"), w=c(0.2,0.05,0.05,0.1,0.4), flag=TRUE),
  c(F,T,T,T,T)
)


# COICOP bundles ----------------------------------------------------------


expect_equal(
  tree(id=c("08","081","08X","08301","08302","12"), flag=TRUE),
  c(F,T,F,T,T,T)
)

expect_equal(
  tree(id=c("08","081","08X","08301","08302","12"), w=c(0.2,0.05,0.15,0.03,0.07,0.4), flag=TRUE),
  c(F,T,T,F,F,T)
)

expect_equal(
  tree(id=c("08","081","082_083","08301","08302","12"), w=c(0.2,0.05,0.15,0.03,0.07,0.4), flag=TRUE),
  c(F,T,T,F,F,T)
)

expect_equal(
  tree(id=c("08","081","082_083","08301","08302","12"), w=c(0.2,0.05,0.15,0.05,0.1,0.4), flag=TRUE),
  c(F,T,F,T,T,T)
)

expect_equal(
  tree(id=c("08","081","082_083","08301","08302","12"), w=c(0.2,0.05,0.15,0.05,0.1,0.4),
       flag=TRUE, settings=list(max.lvl=3)),
  c(F,T,T,F,F,T)
)

expect_equal(
  tree(id=c("08","081","082_083","08301","08302","12"), w=c(0.2,0.05,0.1,0.05,0.1,0.4), 
       flag=TRUE, settings=list(max.lvl=3)),
  c(T,F,F,F,F,T)
)


# COICOP bundles plus their components ------------------------------------


expect_equal(
  tree(id=c("053","0531","0532","0531_0532"), flag=TRUE),
  c(F,T,T,F)
)

expect_equal(
  tree(id=c("053","0531","0531_0532"), flag=TRUE),
  c(F,F,T)
)

expect_equal(
  tree(id=c("053","0531","0532","0531_0532"), w=c(0.3,0.1,0.2,0.3), flag=TRUE),
  c(F,T,T,F)
)

expect_equal(
  tree(id=c("053","0531","0531_0532"), w=c(0.3,0.1,0.3), flag=TRUE),
  c(F,F,T)
)

expect_equal(
  tree(id=c("053","0531","0531_0532"), w=c(0.3,0.1,0.3), flag=TRUE, settings=list(max.lvl=3)),
  c(T,F,F)
)

expect_equal(
  tree(id=c("053","0531","0531_0532"), w=c(0.3,0.1,0.25), flag=TRUE),
  c(T,F,F)
)

expect_equal(
  tree(id=c("053","0531","0531_0532","08","081","08X","082","083"), flag=TRUE),
  c(F,F,T,F,T,F,T,T)
)

expect_equal(
  tree(id=c("081","08X","082","083"), flag=TRUE),
  c(T,F,T,T)
)


# Duplicated ids ----------------------------------------------------------


expect_equal(
  tree(id=c("00","01","011","012","02"), flag=TRUE),
  c(F,F,T,T,T)
)

expect_equal(
  tree(id=c("00","01","011","012","02","011"), flag=TRUE),
  c(F,F,T,T,T,T)
)

expect_equal(
  tree(id=c("00","01","011","012","02","011","02","012"), flag=TRUE),
  c(F,F,T,T,T,T,T,T)
)


# Merge by variable -------------------------------------------------------


t <- c(1,1,1,1,1, 2,2,2,2,2,2,2)
id <- c("00","01","011","012","02", "00","01","011","0111","0112","012","02")
w1 <- c(0.5,0.3,0.2,0.1,0.2, 0.55,0.32,0.22,0.12,0.1,0.1,0.23)
w2 <- c(0.5,0.3,0.2,0.1,0.2, 0.55,0.32,0.22,0.12,0.1,0.05,0.23)

expect_equal(
  tree(id=id, by=t, flag=TRUE), # 011,012,02
  c(F,F,T,T,T, F,F,T,F,F,T,T)
)

expect_equal(
  tree(id=id, by=t, w=w1, flag=TRUE), # 011,012,02
  c(F,F,T,T,T, F,F,T,F,F,T,T)
)

expect_equal(
  tree(id=id, by=t, w=w2, flag=TRUE), # 01,02
  c(F,T,F,F,T, F,T,F,F,F,F,T)
)

t <- c(1,1,1,1,1, 2,2,2,2,2,2)
id <- c("00","01","011","012","02", "01","011","0111","0112","012","02")
w1 <- c(0.5,0.3,0.2,0.1,0.2, 0.32,0.22,0.12,0.1,0.1,0.23)
w2 <- c(0.5,0.3,0.2,0.1,0.2, 0.32,0.22,0.12,0.1,0.05,0.23)

expect_equal(
  tree(id=id, by=t, flag=TRUE), # 011,012,02
  c(F,F,T,T,T, F,T,F,F,T,T)
)

expect_equal(
  tree(id=id, by=t, w=w1, flag=TRUE), # 011,012,02
  c(F,F,T,T,T, F,T,F,F,T,T)
)

expect_equal(
  tree(id=id, by=t, w=w2, flag=TRUE), # 01,02
  c(F,T,F,F,T, T,F,F,F,F,T)
)

t <- as.character(c(1,1,1,1, 2,2,2,2,2,2))
id <- c("00","08","081","082_083", "00","08","081","082","083","082_083")
w <- c(0.5,0.5,0.2,0.3, 0.5,0.5,0.23,0.14,0.13,0.27)

expect_equal(
  tree(id=id, by=t, flag=TRUE),
  c(F,F,T,T, F,F,T,F,F,T)
)

expect_equal(
  tree(id=id, by=t, w=w, flag=TRUE),
  c(F,F,T,T, F,F,T,F,F,T)
)

t <- c(1,1,1,1,1,1, 2,2,2,2,2,2)
id <- c("00","08","081","082","083","082_083", "00","08","081","082","083","082_083")
w <- c(0.5,0.5,0.2,0.15,0.15,0.3, 0.5,0.5,0.23,0.14,0.13,0.27)

expect_equal(
  tree(id=id, by=t, flag=TRUE),
  c(F,F,T,T,T,F, F,F,T,T,T,F)
)

expect_equal(
  tree(id=id, by=t, w=w, flag=TRUE),
  c(F,F,T,T,T,F, F,F,T,T,T,F)
)

expect_equal(
  tree(id=id, by=t, w=w, flag=TRUE, settings=list(max.lvl=2)),
  c(F,T,F,F,F,F, F,T,F,F,F,F)
)

t <- c(1,1,1,1, 2,2,2,2,2)
id <- c("00","08","081","082_083", "00","08","081","082","083")
w <- c(0.5,0.5,0.2,0.3, 0.5,0.5,0.23,0.14,0.13)

expect_equal(
  tree(id=id, by=t, flag=TRUE),
  c(F,T,F,F, F,T,F,F,F)
)

expect_equal(
  tree(id=id, by=t, w=w, flag=TRUE),
  c(F,T,F,F, F,T,F,F,F)
)

# END
