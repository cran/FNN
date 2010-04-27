
datafile<- "FNN_speed_test_0.5.RData";

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
  save.image(file=datafile)

}

#small sample test:

N2<- c(50, 100, 200, 500);

kd.time2<- cover.time2<- VR.time2<- 
 array(0, dim=c(length(N2), length(P), 3), dimnames=list(N2, P, c("user",  "system", "elapsed")));

for(p in P) for(n in N2){
  cat ("p = ", p, ", n = ", n, "\n");
  cover.time2[as.character(n), as.character(p),]<- system.time(
      for(i in 1:1000){
        X<- rmvnorm(n, mean=rep(0, p), sigma(1, .5, p)); knn.dist(X, k=10, "cover")
      })[1:3];
  kd.time2[as.character(n), as.character(p),]<-  system.time(
      for(i in 1:1000){
        X<- rmvnorm(n, mean=rep(0, p), sigma(1, .5, p)); knn.dist(X, k=10, "kd")
      })[1:3];
  VR.time2[as.character(n), as.character(p),]<-  system.time(
      for(i in 1:1000){
        X<- rmvnorm(n, mean=rep(0, p), sigma(1, .5, p)); knn.dist(X, k=10, "VR")
      })[1:3];
      
  save.image(file=datafile)
}

#large sample
N3<- c(1e5, 1e6);
P3<- c(5, 10, 20, 50);
kd.time3<- cover.time3<- VR.time3<- 
 array(0, dim=c(length(N3), length(P3), 3), dimnames=list(N3, P3, c("user",  "system", "elapsed")));

s<- 1;
for(p in P3) for(n in N3){
  cat ("p = ", p, ", n = ", n, "\n");
  cover.time3[as.character(n), as.character(p),]<- system.time(
      for(i in 1:s){
        X<- rmvnorm(n, mean=rep(0, p), sigma(1, .5, p)); knn.dist(X, k=10, "cover")
      })[1:3];
  kd.time3[as.character(n), as.character(p),]<-  system.time(
      for(i in 1:s){
        X<- rmvnorm(n, mean=rep(0, p), sigma(1, .5, p)); knn.dist(X, k=10, "kd")
      })[1:3];
  VR.time3[as.character(n), as.character(p),]<-  system.time(
      for(i in 1:s){
        X<- rmvnorm(n, mean=rep(0, p), sigma(1, .5, p)); knn.dist(X, k=10, "VR")
      })[1:3];
      
  save.image(file=datafile)
}


pdf(file="FNN_speed_comparison.pdf", height=11, width=8.5)
par(mfrow=c(4, 3), mai=c(.5, .5, 0.2, 0.2), omi=c(1, 1, 1.5, 1))
for (p in P){
  plot(N, cover.time[,as.character(p),1]/10/N, type="b", pch=0, xlab="n", ylab="t", main=paste("p = ", p))
  lines(N, kd.time[,as.character(p),1]/10/N, type="b", pch=1, col=2)
  lines(N, VR.time[,as.character(p),1]/10/N, type="b", pch=2, col=3)
  legend("topleft", legend=c("cover-tree", "kd-tree", "VR"), lty=1, col=1:3, pch=0:2, bty="n")
}
title("Time vs Sample Size", outer=TRUE)

par(mfrow=c(3, 2), mai=c(.5, .5, 0.2, 0.2), omi=c(1, 1, 1.5, 1))
for (n in N){
  plot(P, cover.time[as.character(n),,1]/10/n, type="b", pch=0, xlab="p", ylab="t",  main=paste("n = ", n))
  lines(P, kd.time[as.character(n),,1]/10/n, type="b", pch=1, col=2)
  lines(P, VR.time[as.character(n),,1]/10/n, type="b", pch=2, col=3)
  legend("topleft", legend=c("cover-tree", "kd-tree", "VR"), lty=1, col=1:3, pch=0:2, bty="n")
}
title("Time vs Dimension", outer=TRUE)

par(mfrow=c(4, 3), mai=c(0.5, .5, 0.2, 0.2), omi=c(1, 1, 1.5, 1))
for (p in P){
  plot(N2, cover.time2[,as.character(p),1]/1000/N2, type="b", pch=0, xlab="n", ylab="t", main=paste("p = ", p),   
   ylim=if(p<=20) c(2e-5, 1e-4) else if(p>20 & p<=50) c(0.5e-4, 2e-4) else  c(1e-4, 4e-4) )
  lines(N2, kd.time2[,as.character(p),1]/1000/N2, type="b", pch=1, col=2)
  lines(N2, VR.time2[,as.character(p),1]/1000/N2, type="b", pch=2, col=3)
  legend("topleft", legend=c("cover-tree", "kd-tree", "VR"), lty=1, col=1:3, pch=0:2, bty="n")
}
title("Time vs Sample Size for Small Size", outer=TRUE)

par(mfrow=c(2, 2), mai=c(1, 1, 0.5, 0.2), omi=c(1, 1, 1.5, 1))
for (n in N2){
  plot(P, cover.time2[as.character(n),,1]/1000/n, type="b", pch=0, xlab="p", ylab="t",  main=paste("n = ", n))
  lines(P, kd.time2[as.character(n),,1]/1000/n, type="b", pch=1, col=2)
  lines(P, VR.time2[as.character(n),,1]/1000/n, type="b", pch=2, col=3)
  legend("topleft", legend=c("cover-tree", "kd-tree", "VR"), lty=1, col=1:3, pch=0:2, bty="n")
}
title("Time vs Dimension for Small Samples", outer=TRUE)

par(mfrow=c(2, 1), mai=c(1, 1, 0.2, 0.2), omi=c(1, 1, 1.5, 1))
  plot(P3, cover.time3[1,,1]/N3[1], type="b", pch=0, xlab="p", ylab="t",  main=paste("n = ", 10^5))
  lines(P3, kd.time3[1,,1]/N3[1], type="b", pch=1, col=2)
  lines(P3, VR.time3[1,,1]/N3[1], type="b", pch=2, col=3)
  legend("topleft", legend=c("cover-tree", "kd-tree", "VR"), lty=1, col=1:3, pch=0:2, bty="n")

  plot(P3[1:3], cover.time3[2,1:3,1]/N3[2], type="b", pch=0, xlab="p", ylab="t",  main=paste("n = ", 10^6))
  lines(P3[1:3], kd.time3[2,1:3,1]/N3[2], type="b", pch=1, col=2)
  lines(P3[1:3], VR.time3[2,1:3,1]/N3[2], type="b", pch=2, col=3)
  legend("topleft", legend=c("cover-tree", "kd-tree", "VR"), lty=1, col=1:3, pch=0:2, bty="n")

title("Time vs Dimension for Large Samples", outer=TRUE)
dev.off()
