# START


# Simple tests ------------------------------------------------------------

expect_equal(
  suppressWarnings(tree(id=as.character(c(NA, NA)))),
  as.logical(c(NA,NA))
)

expect_equal(
  tree(id=c("00")),
  c(T)
)

expect_equal(
  tree(id=c("00","01","02")),
  c(F,T,T)
)

expect_equal(
  tree(id=c("01","02")),
  c(T,T)
)

expect_equal(
  tree(id=c("00","01","011","012")),
  c(F,F,T,T)
)

expect_equal(
  tree(id=c("00","011","012")),
  c(F,T,T)
)

expect_equal(
  tree(id=c("011","012")),
  c(T,T)
)

expect_equal(
  tree(id=c("01","12")),
  c(T,T)
)

expect_equal(
  tree(id=c("01","12"), max.lvl=1),
  c(F,F)
)

expect_equal(
  tree(id=c("00","01",NA,"12")),
  c(F,T,NA,T)
)

expect_equal(
  tree(id=c("00","01","12"), w=c(1,0.2,0.3)),
  c(T,F,F)
)

expect_equal(
  tree(id=c("00","01","12"), w=c(1,0.4,0.6)),
  c(F,T,T)
)

expect_equal(
  tree(id=c("00","01","011","0111","01111")),
  c(F,F,F,F,T)
)


# More detailed tree ------------------------------------------------------

expect_equal(
  tree(id=c("08","081","08301","08302","12")),
  c(F,T,T,T,T)
)

expect_equal(
  tree(id=c("08","081","08301","08302","12"), w=c(0.2,0.05,0.03,0.07,0.4)),
  c(T,F,F,F,T)
)

expect_equal(
  tree(id=c("08","081","08301","08302","12"), w=c(0.2,0.05,0.05,0.1,0.4)),
  c(F,T,T,T,T)
)


# COICOP bundles ----------------------------------------------------------

expect_equal(
  tree(id=c("08","081","08X","08301","08302","12")),
  c(F,T,F,T,T,T)
)

expect_equal(
  tree(id=c("08","081","08X","08301","08302","12"), w=c(0.2,0.05,0.15,0.03,0.07,0.4)),
  c(F,T,T,F,F,T)
)

expect_equal(
  tree(id=c("08","081","082_083","08301","08302","12"), w=c(0.2,0.05,0.15,0.03,0.07,0.4)),
  c(F,T,T,F,F,T)
)

expect_equal(
  tree(id=c("08","081","082_083","08301","08302","12"), w=c(0.2,0.05,0.15,0.05,0.1,0.4)),
  c(F,T,F,T,T,T)
)

expect_equal(
  tree(id=c("08","081","082_083","08301","08302","12"), w=c(0.2,0.05,0.15,0.05,0.1,0.4), max.lvl=3),
  c(F,T,T,F,F,T)
)

expect_equal(
  tree(id=c("08","081","082_083","08301","08302","12"), w=c(0.2,0.05,0.1,0.05,0.1,0.4), max.lvl=3),
  c(T,F,F,F,F,T)
)


# COICOP bundles plus their components ------------------------------------

expect_equal(
  tree(id=c("053","0531","0532","0531_0532")),
  c(F,T,T,F)
)

expect_equal(
  tree(id=c("053","0531","0531_0532")),
  c(F,F,T)
)

expect_equal(
  tree(id=c("053","0531","0531_0532"), unbundle=FALSE),
  c(F,T,F)
)

expect_equal(
  tree(id=c("053","0531","0532","0531_0532"), w=c(0.3,0.1,0.2,0.3)),
  c(F,T,T,F)
)

expect_equal(
  tree(id=c("053","0531","0531_0532"), w=c(0.3,0.1,0.3)),
  c(F,F,T)
)

expect_equal(
  tree(id=c("053","0531","0531_0532"), w=c(0.3,0.1,0.3), max.lvl=3),
  c(T,F,F)
)

expect_equal(
  tree(id=c("053","0531","0531_0532"), w=c(0.3,0.1,0.25)),
  c(T,F,F)
)

expect_equal(
  tree(id=c("053","0531","0531_0532","08","081","08X","082","083")),
  c(F,F,T,F,T,F,T,T)
)


# Duplicated ids ----------------------------------------------------------

expect_equal(
  tree(id=c("00","01","011","012","02")),
  c(F,F,T,T,T)
)

expect_equal(
  suppressWarnings(tree(id=c("00","01","011","012","02","011"))),
  c(F,F,T,T,T,T)
)

expect_equal(
  suppressWarnings(tree(id=c("00","01","011","012","02","011","02","012"))),
  c(F,F,T,T,T,T,T,T)
)


# Merge by variable -------------------------------------------------------


t <- c(1,1,1,1,1, 2,2,2,2,2,2,2)
id <- c("00","01","011","012","02", "00","01","011","0111","0112","012","02")
w1 <- c(0.5,0.3,0.2,0.1,0.2, 0.55,0.32,0.22,0.12,0.1,0.1,0.23)
w2 <- c(0.5,0.3,0.2,0.1,0.2, 0.55,0.32,0.22,0.12,0.1,0.05,0.23)

expect_equal(
  tree(id=id, by=t), # 011,012,02
  c(F,F,T,T,T, F,F,T,F,F,T,T)
)

expect_equal(
  tree(id=id, by=t, w=w1), # 011,012,02
  c(F,F,T,T,T, F,F,T,F,F,T,T)
)

expect_equal(
  tree(id=id, by=t, w=w2), # 01,02
  c(F,T,F,F,T, F,T,F,F,F,F,T)
)

t <- as.character(c(1,1,1,1, 2,2,2,2,2,2))
id <- c("00","08","081","082_083", "00","08","081","082","083","082_083")
w <- c(0.5,0.5,0.2,0.3, 0.5,0.5,0.23,0.14,0.13,0.27)

expect_equal(
  tree(id=id, by=t),
  c(F,F,T,T, F,F,T,F,F,T)
)

expect_equal(
  tree(id=id, by=t, w=w),
  c(F,F,T,T, F,F,T,F,F,T)
)

expect_equal(
  tree(id=id, by=t, unbundle=FALSE),
  c(F,T,F,F, F,T,F,F,F,F)
)

expect_equal(
  tree(id=id, by=t, w=w, unbundle=FALSE),
  c(F,T,F,F, F,T,F,F,F,F)
)

t <- c(1,1,1,1,1,1, 2,2,2,2,2,2)
id <- c("00","08","081","082","083","082_083", "00","08","081","082","083","082_083")
w <- c(0.5,0.5,0.2,0.15,0.15,0.3, 0.5,0.5,0.23,0.14,0.13,0.27)

expect_equal(
  tree(id=id, by=t),
  c(F,F,T,T,T,F, F,F,T,T,T,F)
)

expect_equal(
  tree(id=id, by=t, w=w),
  c(F,F,T,T,T,F, F,F,T,T,T,F)
)

expect_equal(
  tree(id=id, by=t, w=w, unbundle=FALSE),
  c(F,F,T,T,T,F, F,F,T,T,T,F)
)

expect_equal(
  tree(id=id, by=t, w=w, max.lvl=2),
  c(F,T,F,F,F,F, F,T,F,F,F,F)
)

t <- c(1,1,1,1, 2,2,2,2,2)
id <- c("00","08","081","082_083", "00","08","081","082","083")
w <- c(0.5,0.5,0.2,0.3, 0.5,0.5,0.23,0.14,0.13)

expect_equal(
  tree(id=id, by=t),
  c(F,T,F,F, F,T,F,F,F)
)

expect_equal(
  tree(id=id, by=t, w=w),
  c(F,T,F,F, F,T,F,F,F)
)

# END
