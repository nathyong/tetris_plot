library("lubridate")

ma <- function(x, n=5){filter(x, rep(1/n, n), sides=1)}

linerace <- read.csv("output.csv")
linerace$achieved <- as.POSIXct(linerace$achieved)
linerace$time <- linerace$time / 60
plot(x=linerace$achieved, y=linerace$time, type="p", ylab="Seconds", xlab="Time achieved", main="Linerace time")
lines(y=ma(linerace$time), x=linerace$achieved, type="l", col="blue")
abline(lm(linerace$time ~ linerace$achieved), col="red")
abline(h=min(linerace$time), lty=2)
