# START


# check.char() -----------------------------------------------------------


expect_no_error(
  check.char(x="test")
)

expect_no_error(
  check.char(miss.ok=TRUE)
)

expect_no_error(
  check.char(x=c("test", "abc"), min.len=1)
)

expect_no_error(
  check.char(x=NULL, null.ok=TRUE)
)

expect_no_error(
  check.char(x=NA_character_, na.ok=TRUE)
)

expect_error(
  check.char(x=1)
)

expect_error(
  check.char(miss.ok=FALSE)
)

expect_error(
  check.char(x=c("test", "abc"), max.len=1)
)

expect_error(
  check.char(x=NULL, null.ok=FALSE)
)

expect_error(
  check.char(x=NA_character_, na.ok=FALSE)
)


# check.date() -----------------------------------------------------------


expect_no_error(
  check.date(x=as.Date("2022-01-02"))
)

expect_no_error(
  check.date(miss.ok=TRUE)
)

expect_no_error(
  check.date(x=as.Date("2005-02-01"), min.len=1)
)

expect_no_error(
  check.date(x=NULL, null.ok=TRUE)
)

expect_no_error(
  check.date(x=as.Date(NA), na.ok=TRUE)
)

expect_error(
  check.date(x=1)
)

expect_error(
  check.date(miss.ok=FALSE)
)

expect_error(
  check.date(x=as.Date(c("2021-01-02", "2005-02-01")), max.len=1)
)

expect_error(
  check.date(x=NULL, null.ok=FALSE)
)

expect_error(
  check.date(x=as.Date(NA), na.ok=FALSE)
)


# check.dateseries() ------------------------------------------------------


# expect monthly frequency:
t <- as.Date(c("2021-01-01","2021-02-01"))
expect_no_warning(
  check.dateseries(x=t, freq=12)
)

# non-chronological ordering:
t <- as.Date(c("2021-02-01", "2021-01-01"))
expect_warning(
  check.dateseries(x=t, freq=12),
  regexp="chronological"
)

# monthly frequency with gap:
t <- as.Date(c("2021-01-01","2021-03-01"))
expect_warning(
  check.dateseries(x=t, freq=12),
  regexp="months .* missing"
)

# monthly frequency with duplicates:
t <- as.Date(c("2021-01-01","2021-01-13"))
expect_error(
  check.dateseries(x=t, freq=12),
  regexp="duplicated months"
)

# expect quarterly frequency:
t <- as.Date(c("2021-01-01","2021-04-01"))
expect_no_warning(
  check.dateseries(x=t, freq=4)
)

# non-chronological ordering:
t <- as.Date(c("2021-04-01","2021-01-01"))
expect_warning(
  check.dateseries(x=t, freq=4),
  regexp="chronological"
)

# quarterly frequency with gap:
t <- as.Date(c("2021-01-01","2021-07-01"))
expect_warning(
  check.dateseries(x=t, freq=4),
  regexp="quarters .* missing"
)

# quarterly frequency with duplicates:
t <- as.Date(c("2021-01-01","2021-03-13"))
expect_error(
  check.dateseries(x=t, freq=4),
  regexp="duplicated quarters"
)

# expect annual frequency:
t <- as.Date(c("2021-01-01","2022-01-01"))
expect_no_warning(
  check.dateseries(x=t, freq=1)
)

# non-chronological ordering:
t <- as.Date(c("2022-01-01","2021-01-01"))
expect_warning(
  check.dateseries(x=t, freq=1),
  regexp="chronological"
)

# annual frequency with gap:
t <- as.Date(c("2021-01-01","2023-01-01"))
expect_warning(
  check.dateseries(x=t, freq=1),
  regexp="years .* missing"
)

# annual frequency with duplicates:
t <- as.Date(c("2021-01-01","2021-12-01"))
expect_error(
  check.dateseries(x=t, freq=1),
  regexp="duplicated years"
)


# check.log() ------------------------------------------------------------


expect_no_error(
  check.log(x=TRUE)
)

expect_no_error(
  check.log(miss.ok=TRUE)
)

expect_no_error(
  check.log(x=c(TRUE, FALSE), min.len=1)
)

expect_no_error(
  check.log(x=NULL, null.ok=TRUE)
)

expect_no_error(
  check.log(x=NA, na.ok=TRUE)
)

expect_error(
  check.log(x="test")
)

expect_error(
  check.log(miss.ok=FALSE)
)

expect_error(
  check.log(x=c(TRUE, FALSE), max.len=1)
)

expect_error(
  check.log(x=NULL, null.ok=FALSE)
)

expect_error(
  check.log(x=NA, na.ok=FALSE)
)


# check.num() ------------------------------------------------------------


expect_no_error(
  check.num(x=1)
)

expect_no_error(
  check.num(miss.ok=TRUE)
)

expect_no_error(
  check.num(x=1:10, min.len=1)
)

expect_no_error(
  check.num(x=NULL, null.ok=TRUE)
)

expect_no_error(
  check.num(x=NA_real_, na.ok=TRUE)
)

expect_no_error(
  check.num(x=1:10, int=c(0,100))
)

expect_error(
  check.num(x="test")
)

expect_error(
  check.num(miss.ok=FALSE)
)

expect_error(
  check.num(x=1:10, min.len=0, max.len=1)
)

expect_error(
  check.num(x=NULL, null.ok=FALSE)
)

expect_error(
  check.num(x=NA_real_, na.ok=FALSE)
)

expect_error(
  check.num(x=1:10, int=c(0,5))
)


# check.lenghts() --------------------------------------------------------


expect_no_error(
  check.lengths(x=1:10, y=1:10)
)

expect_no_error(
  check.lengths(x=1:10, y=NULL)
)

expect_no_error(
  check.lengths(x=NULL, y=NULL)
)

expect_error(
  check.lengths(x=1:10, y=1:8)
)


# check.list() ------------------------------------------------------------


expect_no_error(
  check.list(x=list("a"=1, "b"=2))
)

expect_no_error(
  check.list(x=NULL, null.ok=TRUE)
)

expect_no_error(
  check.list(x=list(1, 2), names=FALSE)
)

expect_error(
  check.list(x=list(1, "b"=2), names=TRUE)
)

expect_error(
  check.list(x=list())
)

expect_no_error(
  check.list(x=list(), min.len=0, names=TRUE)
)

# END
