\name{get.knn}
\alias{get.knn}
\alias{get.knnx}

\title{Search Nearest Neighbors}
\description{
  Fast k-nearest neighbor searching algorithms including a kd-tree, cover-tree
  and the algorithm implemented in class package.
}
\usage{
  get.knn(data, k=10, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
  get.knnx(data, query, k=10, algorithm=c("kd_tree", "cover_tree",
	  "CR", "brute"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{an input data matrix.}
  \item{query}{a query data matrix.}
  \item{algorithm}{nearest neighbor searching algorithm.}
  \item{k}{the maximum number of nearest neighbors to search. The default value
  is set to 10.}
}
\details{
  The \emph{cover tree} is O(n) space data structure which allows us to answer queries
  in the same O(log(n)) time as \emph{kd tree} given a fixed intrinsic dimensionality.
  Templated code from \url{https://hunch.net/~jl/projects/cover_tree/cover_tree.html} is used.

  The \emph{kd tree} algorithm is implemented in the Approximate Near Neighbor (ANN) C++ library (see \url{http://www.cs.umd.edu/~mount/ANN/}).
  The exact nearest neighbors are searched in this package.
  
  The \emph{CR} algorithm is the \emph{VR} using distance \emph{1-x'y} assuming \code{x} and \code{y} are unit vectors.
  The \emph{brute} algorithm searches linearly. It is a naive method.
  
}

\value{
  a list contains:
      \item{nn.index}{an n x k matrix for the nearest neighbor indice.}
      \item{nn.dist}{an n x k matrix for the nearest neighbor Euclidean distances.}

}

\author{Shengqiao Li. To report any bugs or suggestions please email: \email{lishengqiao@yahoo.com}}

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
  \code{nn2} in \pkg{RANN}, \code{ann} in \pkg{yaImpute} and \code{\link[class]{knn}} in \pkg{class}.
}
\examples{
  data<- query<- cbind(1:10, 1:10)

  get.knn(data, k=5)
  get.knnx(data, query, k=5)
  get.knnx(data, query, k=5, algo="kd_tree")

  th<- runif(10, min=0, max=2*pi)
  data2<-  cbind(cos(th), sin(th))
  get.knn(data2, k=5, algo="CR")

}

\keyword{manip}
