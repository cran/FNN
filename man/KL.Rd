\name{KL}
\alias{KL.dist}
\alias{KLD.dist}

\title{Kullback-Leibler Distance}
\description{Compute Kullback-Leibler distance or symmetric divergence.}
\usage{
  KL.dist(X, Y, k = 10, algorithm="kd_tree") 
  KLD.dist(X, Y, k = 10, algorithm="kd_tree") 
}

\arguments{
  \item{X}{An input data matrix.}
  \item{Y}{An input data matrix.}  
  \item{k}{The maximum number of nearest neighbors to search. The default value 
  is set to 10.}
  \item{algorithm}{nearest neighbor search algorithm. Only "kd_tree" is implemented.}
}

\value{
  \item{KL.dist}{Return the Kullback-Leibler distance from the first dataset to the second dataset.}
  \item{KLD.dist}{Return the Kullback-Leibler divergence between two datasets.}
}
\references{

Boltz S., Debreuve E. and Barlaud, M. (2007),
\dQuote{kNN-based high-dimensional Kullback-Leibler distance for tracking},
\emph{Image Analysis for Multimedia Interactive Services, 2007. WIAMIS '07. Eighth International Workshop on}, 

Boltz S., Debreuve E. and Barlaud, M. (2009),
\dQuote{High-dimensional statistical measure for region-of-interest tracking},
\emph{Trans. Img. Proc.}, \bold{18}:6, 1266--1283.

}

\author{Shengqiao Li. To report any bugs or suggestions please email: \email{shli@stat.wvu.edu.}}

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
    Y<- rmvnorm(1000, mean=rep(0, 20), sigma(1, .6, 20))
    
    KL.dist(X, Y, k=5)
    KLD.dist(X, Y, k=5)
   }
}
\keyword{manip}
