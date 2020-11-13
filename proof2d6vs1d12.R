d6 = sample(seq(1,6,1), 1000, replace=T) + sample(seq(1,6,1), 1000, replace=T)
d12 = sample(seq(1,12,1), 1000, replace=T)

hist(d6,  xlim=c(0,12), breaks = seq(0,13,1), main=paste("2d6 sum, mean = ", mean(d6)), ylim=c(0,200))
hist(d12, xlim=c(0,12), breaks = seq(0,13,1), main=paste("1d12 sum, mean = ", mean(d12)), ylim=c(0,200))

