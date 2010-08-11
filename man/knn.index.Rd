\name{knn.index}
\alias{knn.index}
\alias{knnx.index}
\title{Search Nearest Neighbors}
\description{
  Fast k-nearest neighbor searching algorithms including a kd-tree, cover-tree
  and the algorithm implemented in class package.
}
\usage{
  knn.index(data, k=10, algorithm=c("cover_tree", "kd_tree", "VR", "CR"))
  knnx.index(data, query, k=10, algorithm=c("cover_tree", "kd_tree", "VR", "CR"))

}
\arguments{
  \item{data}{an input data matrix.}
  \item{query}{a query data matrix.}
  \item{algorithm}{nearest neighbor searching algorithm.}
  \item{k}{the maximum number of nearest neighbors to search. The default value
  is set to 10.}
}


\value{
return the indice of k nearest neighbors.
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
  \code{\link{knn.dist}} and \code{\link{get.knn}}.
}
\examples{
  data<- query<- cbind(1:10, 1:10)

  knn.index(data, k=5)
  knnx.index(data, query, k=5)
  knnx.index(data, query, k=5, algo="kd_tree")
  knnx.index(data, query, k=5, algo="VR")

}

\keyword{manip}
