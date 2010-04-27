################################################################################
# KNN Information Theory Measures                                              #
# File:   KNN.information.measures.R                                           #                   #
# Author: Shengqiao Li                                                         #
# Date:   December 12, 2008                                                    #
#         2010-3-25 add entropy and crossentropy                               #
################################################################################
entropy<- function (X, k=10,  algorithm="kd_tree")
{
  algorithm<- match.arg(algorithm);

  if (!is.numeric(X))  stop("Data non-numeric")
  if (any(is.na(X)))   stop("Data include NAs")
  if (!is.matrix(X))   X <- matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  if (k >= n) stop("k must less than the sample size!")

  if (storage.mode(X) == "integer")  storage.mode(X) <- "double"

  MLD<- .C("KNN_MLD", t(X), as.integer(k), p, n, MLD = double(k), DUP = FALSE)$MLD
  # mean of log dist

  H <- p*MLD + p/2*log(pi) - lgamma(p/2+1) + log(n) - digamma(1:k)

  return(H)
}

crossentropy <- function(X, Y, k=10, algorithm=c("cover_tree", "kd_tree", "VR"))
{
  algorithm<- match.arg(algorithm);
	if (!(is.numeric(X)&& is.numeric(Y))) stop("Data non-numeric");
	if (any(is.na(X), is.na(Y)))  stop("Data include NAs");

	if (!is.matrix(X))  X<- matrix(X);
	if (!is.matrix(Y))  Y<- matrix(Y);


	n <- nrow(X); m<- nrow(Y);
	p <- ncol(X);

	if(k>=n) stop("k must less than the sample size!");

	dnn<- knnx.dist(Y, X, k=k, algorithm=algorithm);
  MLD<- colMeans(log(dnn));
	H<- p*MLD + p/2*log(pi) - lgamma(p/2+1) + log(m) - digamma(1:k)

	return (H);
}

KL.divergence<- function(X, Y, k=10, algorithm=c("cover_tree", "kd_tree", "VR"))
{
  #Kullback-Leibler Distance
  algorithm<- match.arg(algorithm);

  if (!is.matrix(X))  X<- matrix(X);
  if (!is.matrix(Y))  Y<- matrix(Y);

  n<- nrow(X); p<- ncol(X);
  m<- nrow(Y);

  log(m/n) + p*(colMeans(log(knnx.dist(Y, X, k=k, algorithm)))- colMeans(log(knn.dist(X, k=k, algorithm))));

}

KL.dist<- function(X, Y, k=10,  algorithm=c("cover_tree", "kd_tree", "VR"))
{
  #Symmetric Kullback-Leibler divergence. i.e. Kullback-Leibler distance
  algorithm<- match.arg(algorithm);

  KL.divergence(X, Y, k, algorithm) + KL.divergence(Y, X, k, algorithm)

}

KLx.divergence<- function (X, Y, k = 10, algorithm="kd_tree")
{
  #Kullback-Leibler divergence
  algorithm<- match.arg(algorithm);
  if(storage.mode(X)=="integer") storage.mode(X)<- "double";
  if(storage.mode(Y)=="integer") storage.mode(Y)<- "double";

  if(!is.matrix(X)) X<- as.matrix(X);
  if(!is.matrix(Y)) Y<- as.matrix(Y);

  n<- nrow(X); m<- nrow(Y);
  d <- ncol(X); p<- ncol(Y);

  if(d!=p) stop("Number of columns must be same!.");
  if(k>=n) warning("k should be less than sample size!");

  .C("KL_divergence", t(X), t(Y), as.integer(k), d, n, m, KL = double(k), DUP=FALSE)$KL;

}

KLx.dist<- function (X, Y, k = 10, algorithm="kd_tree")
{
  #Symmetric Kullback-Leibler divergence. i.e. Kullback-Leibler distance
  algorithm<- match.arg(algorithm);
  if(storage.mode(X)=="integer") storage.mode(X)<- "double";
  if(storage.mode(Y)=="integer") storage.mode(Y)<- "double";

  if(!is.matrix(X)) X<- as.matrix(X);
  if(!is.matrix(Y)) Y<- as.matrix(Y);

  n<- nrow(X); m<- nrow(Y);
  d<- ncol(X); p<- ncol(Y);

  if(d!=p) stop("Number of columns must be same!.");
  if(k>=n) warning("k should be less than sample size!");

  .C("KL_dist", t(X), t(Y), as.integer(k), d, n, m, KLD = double(k), DUP=FALSE)$KLD;

}


