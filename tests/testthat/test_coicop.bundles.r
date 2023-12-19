# START


# Function is.bundle() ----------------------------------------------------

expect_equal(
  c(F,F,F,T,T,F),
  is.bundle(id=c(NA,"01","08","08X","1212_1213","1212"))
)


# Function unbundle() -----------------------------------------------------

res <- c(NA,"01","08","082","083","1212","1213","1212")
names(res) <- c(NA,"01","08","08X","08X","1212_1213","1212_1213","1212")
expect_equal(
  res,
  unbundle(id=c(NA,"01","08","08X","1212_1213","1212"))
)

res <- c("081","082","083","082","083")
names(res) <- c("081","08X","08X","082_083","082_083")
expect_equal(
  res,
  unbundle(id=c("081","08X","082_083"))
)


# Function keep.bundle() --------------------------------------------------

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

# END
