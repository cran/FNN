\name{KL.dist}
\alias{KL.dist}
\alias{KLx.dist}

\title{Kullback-Leibler Divergence}
\description{Compute Kullback-Leibler symmetric distance.}
\usage{
  KL.dist(X, Y, k = 10, algorithm=c("cover_tree", "kd_tree", "VR"))
  KLx.dist(X, Y, k = 10, algorithm="kd_tree")    
}

\arguments{
  \item{X}{An input data matrix.}
  \item{Y}{An input data matrix.}  
  \item{k}{The maximum number of nearest neighbors to search. The default value 
  is set to 10.}
  \item{algorithm}{nearest neighbor search algorithm.}
}

\value{
Return the Kullback-Leibler distance between \code{X} to \code{Y}.
  
}

\details{ 
   Kullback-Leibler distance is the sum of divergence from \code{p(x)} to  \code{q(x)} and from \code{q(x)} to  \code{p(x)}.
   
   \code{KL.*} versions return distances from \code{C} code to \code{R} but \code{KLx.*} do not.
   
}
\seealso{
  \code{\link{KL.divergence}}.

}
\author{Shengqiao Li. To report any bugs or suggestions please email: \email{shli@stat.wvu.edu.}}

\references{
S. Boltz, E. Debreuve and M. Barlaud (2007).
\dQuote{kNN-based high-dimensional Kullback-Leibler distance for tracking}.
\emph{Image Analysis for Multimedia Interactive Services, 2007. WIAMIS '07. Eighth International Workshop on}. 

S. Boltz, E. Debreuve and M. Barlaud (2009).
\dQuote{High-dimensional statistical measure for region-of-interest tracking}.
\emph{Trans. Img. Proc.}, \bold{18}:6, 1266--1283.
}

\examples{
    set.seed(1000)
    X<- rexp(10000, rate=0.2)
    Y<- rexp(10000, rate=0.4)
    
    KL.dist(X, Y, k=5)                 
    KLx.dist(X, Y, k=5) 
    #thoretical distance = (0.2-0.4)^2/(0.2*0.4) = 0.5
    
}
\keyword{manip}
