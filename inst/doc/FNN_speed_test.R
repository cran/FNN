library(FNN);
sigma<- function(v, r, p)
{
	
	V<- matrix(r^2, ncol=p, nrow=p)
	diag(V)<- 1;
	V*v;
}

library(mvtnorm)
N<- c(200, 500, 1000, 2000, 5000, 10000);
P<- c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100);

kd.time<- cover.time<- VR.time<- 
  matrix(0, nrow=length(N), ncol=length(P), dimnames=list(N, P));
  
for(p in P)
for(n in N){
  cat ("p = ", p, ", n = ", n, "\n");
  X<- rmvnorm(n, mean=rep(0, p), sigma(1, .5, p))
  kd.time[as.character(n), as.character(p)]<- system.time(knn.dist(X, k=10))[1];
  cover.time[as.character(n), as.character(p)]<- system.time(knn.dist(X, k=10, "cover"))[1];
  VR.time[as.character(n), as.character(p)]<- system.time(knn.dist(X, k=10, algo="VR"))[1];
}

pdf(file="FNN_speed_comparison.pdf", height=8, width=6)
par(mfrow=c(4, 3), mai=c(.5, .5, 0.2, 0.2), omi=c(.5, 0, .5, 0))
for (p in P){
  plot(N, VR.time[, as.character(p)]/kd.time[, as.character(p)], ylim=c(0,5), type="l", xlab="n", ylab="speed ratio",  main=paste("p = ", p), log="x")
  lines(N, VR.time[, as.character(p)]/cover.time[, as.character(p)], col=2)
  abline(h=1, lty=2)
}
plot(1,type="n", axes=FALSE, xlab="", ylab="")
legend("center", legend=c("kd-tree", "cover-tree"), lty=1, col=1:2, bty="n")
title("Speed Comparison of Different Nearest Neighbor Algorithms", outer=TRUE)

dev.off()


#small sample test:

N2<- c(50, 100);

kd.time2<- cover.time2<- VR.time2<- 
  matrix(0, nrow=length(N2), ncol=length(P), dimnames=list(N2, P));

for(s in 1:100)  
for(p in P)
for(n in N2){
  cat ("p = ", p, ", n = ", n, "\n");
  X<- rmvnorm(n, mean=rep(0, p), sigma(1, .5, p))
  kd.time2[as.character(n), as.character(p)]<- kd.time2[as.character(n), as.character(p)] + system.time(knn.dist(X, k=10))[1];
  cover.time2[as.character(n), as.character(p)]<- cover.time2[as.character(n), as.character(p)]+system.time(knn.dist(X, k=10, "cover"))[1];
  VR.time2[as.character(n), as.character(p)]<- VR.time2[as.character(n), as.character(p)] + system.time(knn.dist(X, k=10, algo="VR"))[1];
}

pdf(file="FNN_small_sample_speed_comparison_PC.pdf", height=6, width=6)
par(mfrow=c(1, 2), mai=c(1, 1, 0.5, 0.2), omi=c(.5, 0, .5, 0))
plot(P, VR.time2["50",]/kd.time2["50",], ylim=c(0,2), type="l", xlab="p", ylab="speed ratio",  main="n = 50")
lines(P, VR.time2["50",]/cover.time2["50",], col=2)
abline(h=1, lty=2)
legend("topleft", legend=c("kd-tree", "cover-tree"), lty=1, col=1:2, bty="n")
plot(P, VR.time2["100",]/kd.time2["100",], ylim=c(0,2), type="l", xlab="p", ylab="speed ratio",  main="n = 100")
lines(P, VR.time2["100",]/cover.time2["100",], col=2)
abline(h=1, lty=2)
legend("topleft", legend=c("kd-tree", "cover-tree"), lty=1, col=1:2, bty="n")
dev.off()

save.image(file="FNN_speed_test.RData")