library(FNN);
library(mvtnorm);
sigma<- function(v, r, p)
{
	
	V<- matrix(r^2, ncol=p, nrow=p)
	diag(V)<- 1;
	V*v;
}


N<- c(200, 500, 1000, 2000, 5000, 10000);
P<- c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100);

kd.time<- cover.time<- VR.time<- 
  array(0, dim=c(length(N), length(P), 3), dimnames=list(N, P, c("user",  "system", "elapsed") ));
  
for(p in P)
for(n in N){
  cat ("p = ", p, ", n = ", n, "\n");
  X<- rmvnorm(n, mean=rep(0, p), sigma(1, .5, p))
  kd.time[as.character(n), as.character(p),]<- system.time(for(i in 1:10)knn.dist(X, k=10, "kd"))[1:3];
  cover.time[as.character(n), as.character(p),]<- system.time(for(i in 1:10)knn.dist(X, k=10, "cover"))[1:3];
  VR.time[as.character(n), as.character(p),]<- system.time(for(i in 1:10)knn.dist(X, k=10, algo="VR"))[1:3];
   save.image(file="FNN_speed_test_new.RData")

}

pdf(file="FNN_speed_comparison.pdf", height=8, width=6)
par(mfrow=c(4, 3), mai=c(.5, .5, 0.2, 0.2), omi=c(.5, 0, .5, 0))
for (p in P){
  plot(N, VR.time[, as.character(p),3]/cover.time[, as.character(p),3], ylim=c(0,2), type="b", pch=0, xlab="n", ylab="speed ratio",  main=paste("p = ", p), log="x")
  lines(N, VR.time[, as.character(p),3]/kd.time[, as.character(p),3], type="b", pch=1, col=2)
  abline(h=1, lty=2)
}
plot(1,type="n", axes=FALSE, xlab="", ylab="")
legend("center", legend=c("cover-tree", "kd-tree"), lty=1, col=1:2, pch=0:1, bty="n")
title("Speed Comparison of Different Nearest Neighbor Algorithms", outer=TRUE)
dev.off()
save.image(file="FNN_speed_test_new.RData")


#small sample test:

N2<- c(50, 100, 200, 500);

kd.time2<- cover.time2<- VR.time2<- 
 array(0, dim=c(length(N2), length(P), 3), dimnames=list(N2, P, c("user",  "system", "elapsed")));

for(p in P) for(n in N2){
  cat ("p = ", p, ", n = ", n, "\n");
  gc(TRUE)

  cover.time2[as.character(n), as.character(p),]<- system.time(
      for(i in 1:1000){
        X<- rmvnorm(n, mean=rep(0, p), sigma(1, .5, p)); knn.dist(X, k=10, "cover")
      })[1:3];
  gc(TRUE)
  kd.time2[as.character(n), as.character(p),]<-  system.time(
      for(i in 1:1000){
        X<- rmvnorm(n, mean=rep(0, p), sigma(1, .5, p)); knn.dist(X, k=10, "kd")
      })[1:3];
  gc(TRUE)
  VR.time2[as.character(n), as.character(p),]<-  system.time(
      for(i in 1:1000){
        X<- rmvnorm(n, mean=rep(0, p), sigma(1, .5, p)); knn.dist(X, k=10, "VR")
      })[1:3];
      
  save.image(file="FNN_speed_test_memory.RData")
}

pdf(file="FNN_small_sample_speed_comparison.pdf", height=6, width=6)
par(mfrow=c(2, 2), mai=c(1, 0.5, 0.5, 0.2), omi=c(.5, 0, .5, 0))
plot(P, VR.time2["50",,3]/cover.time2["50",,1], ylim=c(0,2), type="b", pch=0, xlab="p", ylab="speed ratio",  main="n = 50")
lines(P, VR.time2["50",,3]/kd.time2["50",,1], type="b", pch=1, col=2)
abline(h=1, lty=2)
legend("topleft", legend=c("cover-tree", "kd-tree"), lty=1, col=1:2, pch=0:1, bty="n")
plot(P, VR.time2["100",,3]/cover.time2["100",,1], ylim=c(0,2), type="b", pch=0, xlab="p", ylab="speed ratio",  main="n = 100")
lines(P, VR.time2["100",,3]/kd.time2["100",,1], type="b", pch=1, col=2)
abline(h=1, lty=2)
legend("topleft", legend=c("cover-tree", "kd-tree"), lty=1, col=1:2, pch=0:1, bty="n")

plot(P, VR.time2["200",,3]/cover.time2["200",,1], ylim=c(0,2), type="b", pch=0, xlab="p", ylab="speed ratio",  main="n = 200")
lines(P, VR.time2["200",,3]/kd.time2["200",,1], type="b", pch=1, col=2)
abline(h=1, lty=2)
legend("topleft", legend=c("cover-tree", "kd-tree"), lty=1, col=1:2, pch=0:1, bty="n")

plot(P, VR.time2["500",,3]/cover.time2["500",,1], ylim=c(0,2), type="b", pch=0, xlab="p", ylab="speed ratio",  main="n = 500")
lines(P, VR.time2["500",,3]/kd.time2["500",,1], type="b", pch=1, col=2)
abline(h=1, lty=2)
legend("topleft", legend=c("cover-tree", "kd-tree"), lty=1, col=1:2, pch=0:1, bty="n")



dev.off()


library(FNN);
library(mvtnorm);
sigma<- function(v, r, p)
{
	
	V<- matrix(r^2, ncol=p, nrow=p)
	diag(V)<- 1;
	V*v;
}

system.time(
      for(i in 1:1000){
        X<- rmvnorm(5000, mean=rep(0, 10), sigma(1, .5, 10)); knn.dist(X, k=1, "cover")
      })
 
 