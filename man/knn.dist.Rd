\name{knn.dist}
\alias{knn.dist}
\alias{knnx.dist}
\title{k Nearest Neighbor Distances}
\description{
  Fast k-nearest neighbor distance searching algorithms.
}
\usage{

  knn.dist(data, k=10, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
  knnx.dist(data, query, k=10, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{an input data matrix.}
  \item{query}{a query data matrix.}
  \item{algorithm}{nearest neighbor searching algorithm.}
  \item{k}{the maximum number of nearest neighbors to search. The default value
  is set to 10.}
}

\value{
 return the Euclidiean distances of k nearest neighbors.
}

\author{Shengqiao Li. To report any bugs or suggestions please email: \email{shli@stat.wvu.edu.}}

\references{
Bentley J.L. (1975), \dQuote{Multidimensional binary search trees used for associative
search,} \emph{Communication ACM}, \bold{18}, 309-517.

Arya S. and Mount D.M. (1993),
\dQuote{Approximate nearest neighbor searching,}
\emph{Proc. 4th Ann. ACM-SIAM Symposium on Discrete Algorithms (SODA'93)}, 271-280.

Arya S., Mount D.M., Netanyahu N.S., Silverman R. and Wu A.Y. (1998),
\dQuote{An optimal algorithm for approximate nearest neighbor searching,}
\emph{Journal of the ACM}, \bold{45}, 891-923.

Beygelzimer A., Kakade S. and Langford J. (2006),
\dQuote{Cover trees for nearest neighbor,}
\emph{ACM Proc. 23rd international conference on Machine learning}, \bold{148}, 97-104.

}

\seealso{
  \code{\link{get.knn}} and \code{\link{knn.index}} .
}
\examples{
  if(require(mvtnorm))
  {
    sigma<- function(v, r, p)
    {
      	V<- matrix(r^2, ncol=p, nrow=p)
    	  diag(V)<- 1
        V*v
    }

    X<- rmvnorm(1000, mean=rep(0, 20), sigma(1, .5, 20))
    print(system.time(knn.dist(X)) )
    print(system.time(knn.dist(X, algorithm = "kd_tree")))

  }
}

\keyword{manip}
