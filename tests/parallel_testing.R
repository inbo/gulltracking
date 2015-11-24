random_dates <- function(n) {
	years <- c("2011", "2012", "2013", "2014")
	months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
	days <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12",
						"13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
						"25", "26", "27", "28")
	dates <- paste(sample(years, n, replace=TRUE),
								 sample(months, n, replace=TRUE),
								 sample(days, n, replace=TRUE),
							sep="-")
	times <- paste(sample(months, n, replace=TRUE),
								 sample(days, n, replace=TRUE),
								 sample(days, n, replace=TRUE),
							sep=":")
	return(paste(dates, times))
}


f <- function(n) {
	dates <- random_dates(n)
  d <- data.table(column1=dates)
  t <- system.time(d$column2 <- lapply(d$column1, function(x){fast_strptime(x, "%Y-%m-%d %H:%M:%OS")}))
  return(as.numeric(t[3]))
}

parF <- function(n) {
  cl <- makeCluster(6, type="FORK")
  dates <- random_dates(n)
  d <- data.table(column1=dates)
  t <- system.time(d$column2 <- parLapply(cl, d$column1, function(x){fast_strptime(x, "%Y-%m-%d %H:%M:%OS")}))
  stopCluster(cl)
  return(as.numeric(t[3]))
}

dtF <- function(n) {
	dates <- random_dates(n)
	d <- data.table(column1=dates)
	# I have the idea that this happens lazy, so I'm wrapping it in a "tail" statement
	t <- system.time(eval(d[, fast_strptime(column1, "%Y-%m-%d %H:%M:%OS")]))
	return(as.numeric(t[3]))
}

mcF <- function(n) {
	cl <- makeCluster(6, type="FORK")
	dates <- random_dates(n)
	d <- data.table(column1=dates)
	t <- system.time(d$column2 <- mclapply(d$column1, function(x){fast_strptime(x, "%Y-%m-%d %H:%M:%OS")}))
	stopCluster(cl)
	return(as.numeric(t[3]))
}
input <- c(1000, 2000, 3000, 4000, 5000, 10000, 20000, 40000, 80000)
f_timing <- sapply(input, f)
parF_timing <- sapply(input, parF)
dtF_timing <- sapply(input, dtF)
mcF_timing <- sapply(input, mcF)

d <- data.table(n=input,
								not_parallel=f_timing,
								parallel=parF_timing,
								mcParallel=mcF_timing,
								data.table_way=dtF_timing)
d2 <- melt(d, id=c("n"))
p <- ggplot(d2, aes(x=n, y=value, colour=variable))
p + geom_line()

