castatlevel = 2
hitmodifier = 4

OUT = NULL
targetAC = seq(10,20,1)
for(t in targetAC){
  for(r in 1:100){
    #magic missles - 3d4+1, plus extra for levels
    magicm = sum(sample(seq(1,4,1),(3 + (castatlevel - 1)), replace=T)) + (3 + (castatlevel - 1))
    
    #scorching ray - 3 rays @ 2d6 damage, must beat AC to hit
    tohit = sample(seq(1,20,1), 3, replace = T) + hitmodifier
    
    #check for crits
    add = NULL
    for(th in tohit){
      if(th==(20+hitmodifier)){
        add = c(add, (20+hitmodifier))
      }
    }
    hit.sray = tohit[tohit>=t]
    sray = NULL
    for(hts in hit.sray){
      sray  = c(sray, sum(sample(seq(1, 6, 1), 2, replace = T)))
    }
    OUT = rbind(OUT, c(t, r, sum(magicm), sum(hit.sray)))
  }
}
data = as.data.frame(OUT)
colnames(data) = c("AC", "rep", "missle", "ray")

#What is the average damage for magic missle and scorching ray at various foe AC?
plot(-100,-100, xlim=c(10,20), ylim=c(0,70), xlab="foe's AC", ylab="damage dealt")

#estimate means/SE
means = data.frame(AC = targetAC, missleM = rep(NA, length(targetAC)), missleS = rep(NA, length(targetAC)), srayM = rep(NA, length(targetAC)), srayS = rep(NA, length(targetAC)))
for(t in 1:length(targetAC)){
  temp = data[data$AC==as.character(targetAC[t]),]
  means$missleM[t] = mean(temp$missle)
  means$missleU[t] = quantile(temp$missle, probs=0.975)
  means$missleL[t] = quantile(temp$missle, probs=0.025)
  means$missleS[t] = sd(temp$missle)#/sqrt(nrow(temp))
  means$srayM[t]   = mean(temp$ray)
  means$srayU[t]   = quantile(temp$ray, probs=0.975)
  means$srayL[t]   = quantile(temp$ray, probs=0.025)
}

#add points and lines
points(means$AC, means$missleM, pch=20, col="dodgerblue3", cex=2)
lines(means$AC, means$missleM, lty=1, col="dodgerblue3", cex=2)
lines(means$AC, means$missleU, lty=2, col="dodgerblue3", cex=2)
lines(means$AC, means$missleL, lty=2, col="dodgerblue3", cex=2)

points(means$AC, means$srayM, pch=20, col="firebrick3", cex=2)
lines(means$AC, means$srayM, lty=1, col="firebrick3", cex=2)
lines(means$AC, means$srayU, lty=2, col="firebrick3", cex=2)
lines(means$AC, means$srayL, lty=2, col="firebrick3", cex=2)

