\name{get.knn}
\alias{get.knn}
\alias{get.knnx}
\alias{knn.dist}
\alias{knn.index}
\alias{knnx.dist}
\alias{knnx.index}
\title{Search Nearest Neighbors}
\description{
  Fast k-nearest neighbor searching algorithms including a kd-tree, cover-tree 
  and the algorithm implemented in class package.
}
\usage{
  get.knn(data, k=10, algorithm=c("cover_tree", "kd_tree", "VR"))
  get.knnx(data, query, k=10, algorithm=c("cover_tree", "kd_tree", "VR"))
  
  knn.dist(data, k=10, algorithm=c("cover_tree", "kd_tree", "VR"))
  knn.index(data, k=10, algorithm=c("cover_tree", "kd_tree", "VR"))
  
  knnx.dist(data, query, k=10, algorithm=c("cover_tree", "kd_tree", "VR"))
  knnx.index(data, query, k=10, algorithm=c("cover_tree", "kd_tree", "VR"))
        
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
  Templated code from \url{http://hunch.net/~jl/projects/cover_tree/cover_tree.html} is used.
  
  The \emph{kd tree} algorithm is  implemented in the Approximate Near Neighbor (ANN) C++ library (see  \url{http://www.cs.umd.edu/~mount/ANN/}).
  The exact nearest neighbors are searched in this package. \emph{Self-match} is disabled
   in this package. \emph{Self-match} enabled interface is available in package \pkg{RANN}. 
  
  \code{get.VR_knnx} is the search algorith from package \pkg{class}. This algorithm allows
   \emph{Self-match} and is used for classificaton and regression.
   
  In a performance test on a PC,  \emph{kd tree} is faster than the other two for low-dimensional data (\eqn{p < 10}{p < 10}),
  \emph{VR} algorithm is fast for moderate dimension (\eqn{10 < p < 30}{10 < p < 30}), 
  while \emph{cover tree} is are fastest for lager high-dimensional data (\eqn{p > 30}{p > 30}).
  For small samll sample (n<=100), \emph{VR} is faster than the other complex data structure.
  
   See see comparison figures in the doc folder. 
}

\value{
  \item{get.knn and get.knnx}{return a list:
    \itemize{
      \item nn.index -- an n x k matrix for the nearest neighbor indice.
      \item nn.dist -- an n x k matrix for the nearest neighbor Euclidean distances.
    }
  }
  
  \item{knn.index and knnx.index}{only return the indice of k nearest neighbors.}
  \item{knn.dist and knnx.dist}{only return the Euclidiean distances of k nearest neighbors.}
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
  \code{\link[RANN]{nn2}} in \pkg{RANN}, \code{\link[yaImpute]{ann}} in \pkg{yaImpute} and \code{\link[class]{knn}} in \pkg{class}.
}
\examples{
  data<- query<- cbind(1:10, 1:10)

  get.knn(data, k=5)
  get.knnx(data, query, k=5) #please note that self-match is not allowed.
  get.knnx(data, query, k=5, algo="kd_tree") 
  get.knnx(data, query, k=5, algo="VR") #please note that self-match is allowed.
   
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
    print(system.time(knn.dist(X, algorithm = "VR")))
  }
}

\keyword{manip}
