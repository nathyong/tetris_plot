library("lubridate")

ma <- function(x, n=5){filter(x, rep(1/n, n), sides=1)}

# linerace stats
linerace <- read.csv("linerace.csv")
linerace$achieved <- as.POSIXct(linerace$achieved)
linerace$time <- linerace$time / 60
plot(x=linerace$achieved, y=linerace$time, type="p", ylab="Seconds", xlab="Time achieved", main="Linerace time")
lines(y=ma(linerace$time), x=linerace$achieved, type="l", col="blue")
abline(lm(linerace$time ~ linerace$achieved), col="red")
abline(h=min(linerace$time), lty=2)

plot(x=linerace$achieved, y=linerace$ppm, type="p", ylab="Pieces", xlab="Time achieved", main="Pieces per minute(Linerace)")
lines(y=ma(linerace$ppm), x=linerace$achieved, type="l", col="blue")
abline(lm(linerace$ppm ~ linerace$achieved), col="red")

plot(x=linerace$achieved, y=linerace$finesse, type="p", ylab="Excess keypresses", xlab="Time achieved", main="Finesse (Linerace)")
lines(y=ma(linerace$finesse, n=10), x=linerace$achieved, type="l", col="blue")
abline(lm(linerace$finesse ~ linerace$achieved), col="red")

# digchallenge stats
digchallenge <- read.csv("digchallenge.csv")
digchallenge$achieved <- as.POSIXct(digchallenge$achieved)
digchallenge$time <- digchallenge$time / 60
plot(x=digchallenge$achieved, y=digchallenge$time, type="p", ylab="Seconds", xlab="Time achieved", main="Dig Challenge Survival")
lines(y=ma(digchallenge$time), x=digchallenge$achieved, type="l", col="blue")
abline(lm(digchallenge$time ~ digchallenge$achieved), col="red")
abline(h=max(digchallenge$time), lty=2)

plot(x=digchallenge$achieved, y=digchallenge$ppm, type="p", ylab="Pieces", xlab="Time achieved", main="Pieces per minute (Dig Challenge)")
lines(y=ma(digchallenge$ppm), x=digchallenge$achieved, type="l", col="blue")
abline(lm(digchallenge$ppm ~ digchallenge$achieved), col="red")

plot(x=digchallenge$achieved, y=digchallenge$score, type="p", ylab="Score", xlab="Time achieved", main="Score (Dig Challenge)")
lines(y=ma(digchallenge$score), x=digchallenge$achieved, type="l", col="blue")
abline(lm(digchallenge$score ~ digchallenge$achieved), col="red")

plot(x=digchallenge$achieved, y=digchallenge$spm, type="p", ylab="Score per minute", xlab="Time achieved", main="Score per minute (Dig Challenge)")
lines(y=ma(digchallenge$spm), x=digchallenge$achieved, type="l", col="blue")
abline(lm(digchallenge$spm ~ digchallenge$achieved), col="red")
