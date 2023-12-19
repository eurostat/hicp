# START

# input data:
t <- seq.Date(from=as.Date("2020-01-01"), to=as.Date("2023-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5) # current index
p.new <- p+rnorm(n=length(p), mean=0, sd=1) # new index
p[25:48] <- NA # current index stops in 2021

res1 <- c(p[1:24], (p.new*p[t=="2021-12-01"]/p.new[t=="2021-12-01"])[25:48])
res2 <- c(p[1:24], (p.new*mean(p[format(t,"%Y")=="2021"])/mean(p.new[format(t,"%Y")=="2021"]))[25:48])
res3 <- c(p[1:24], (p.new*mean(p[format(t,"%Y")=="2022"])/mean(p.new[format(t,"%Y")=="2022"]))[25:48])

expect_equal(
  24+2,
  ncol(link(x=p, x.new=p.new, t=t, t.overlap=NULL))
)

expect_equal(
  data.table("2021-12"=res1, "2021"=res2, "2022"=res3),
  suppressMessages(link(x=p, x.new=p.new, t=t, t.overlap=c("2021-12","2021","2022")))
)

lsf1 <- p[t=="2021-12-01"]/p.new[t=="2021-12-01"]
lsf2 <- (mean(p[format(t,"%Y")=="2021"])/mean(p.new[format(t,"%Y")=="2021"]))
lsf3 <- (mean(p[format(t,"%Y")=="2022"])/mean(p.new[format(t,"%Y")=="2022"]))

expect_equal(
  24+2,
  length(lsf(x=p, x.new=p.new, t=t, t.overlap=NULL))
)

expect_equal(
  c("2021"=lsf2/lsf1, "9999"=NA, "2022"=lsf3/lsf1),
  lsf(x=p, x.new=p.new, t=t, t.overlap=c("2021","9999","2022"))
)


# no overlap periods available:
p.new[1:24] <- NA # new index starts in 2022

expect_equal(
  p,
  suppressMessages(link(x=p, x.new=p.new, t=t, t.overlap=NULL))
)

expect_equal(
  p,
  suppressMessages(link(x=p, x.new=p.new, t=t, t.overlap="2022"))
)

expect_equal(
  NA_real_,
  lsf(x=p, x.new=p.new, t=t, t.overlap=NULL)
)

expect_equal(
  c("2022"=NA_real_, "2022-12"=NA_real_),
  lsf(x=p, x.new=p.new, t=t, t.overlap=c("2022","2022-12"))
)



# END
