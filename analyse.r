library("lubridate")

ma <- function(x, n=5){filter(x, rep(1/n, n), sides=1)}

linerace <- read.csv("output.csv")
linerace$achieved <- as.POSIXct(linerace$achieved)
linerace$time <- linerace$time / 60
plot(x=linerace$achieved, y=linerace$time, type="p", ylab="Seconds", xlab="Time achieved", main="Linerace time")
lines(y=ma(linerace$time), x=linerace$achieved, type="l", col="blue")
abline(lm(linerace$time ~ linerace$achieved), col="red")
abline(h=min(linerace$time), lty=2)

plot(x=linerace$achieved, y=linerace$ppm, type="p", ylab="Pieces", xlab="Time achieved", main="Pieces per minute")
lines(y=ma(linerace$ppm), x=linerace$achieved, type="l", col="blue")
abline(lm(linerace$ppm ~ linerace$achieved), col="red")

plot(x=linerace$achieved, y=linerace$finesse, type="p", ylab="Excess keypresses", xlab="Time achieved", main="Finesse")
lines(y=ma(linerace$finesse), x=linerace$achieved, type="l", col="blue")
abline(lm(linerace$finesse ~ linerace$achieved), col="red")
