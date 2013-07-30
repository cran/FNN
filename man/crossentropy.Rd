\name{crossentropy}
\alias{crossentropy}
\title{Cross Entropy}
\description{KNN Cross Entropy Estimators.}
\usage{
  crossentropy(X, Y, k=10, algorithm=c("kd_tree", "cover_tree", "brute"))
}

\arguments{
  \item{X}{an input data matrix.}
  \item{Y}{an input data matrix.}

  \item{k}{the maximum number of nearest neighbors to search. The default value is set to 10.}
  \item{algorithm}{nearest neighbor search algorithm.}
}

\value{
  a vector of length \code{k} for crossentropy estimates using \code{1:k} nearest neighbors, respectively.
}

\details{
   If \code{p(x)} and \code{q(x)} are two continuous probability density functions,
   then the cross-entropy of \code{p} and \code{q} is defined as
   \eqn{H(p;q) = E_p[-\log q(x)]}{H(p;q) = E_p[-\log q(x)]}.
}

\author{Shengqiao Li. To report any bugs or suggestions please email: \email{shli@stat.wvu.edu.}}

\references{
S. Boltz, E. Debreuve and M. Barlaud (2007).
\dQuote{kNN-based high-dimensional Kullback-Leibler distance for tracking}.
\emph{Image Analysis for Multimedia Interactive Services, 2007. WIAMIS '07. Eighth International Workshop on}.

}

\keyword{manip}
