\name{crossentropy}
\alias{crossentropy}
\title{Cross Entropy}
\description{KNN Cross Entropy Estimators.}
\usage{
  crossentropy(X, Y, k=10, algorithm=c("cover_tree", "kd_tree", "VR"))
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

\author{Shengqiao Li. To report any bugs or suggestions please email: \email{shli@stat.wvu.edu.}}

\references{
S. Boltz, E. Debreuve and M. Barlaud (2007).
\dQuote{kNN-based high-dimensional Kullback-Leibler distance for tracking}.
\emph{Image Analysis for Multimedia Interactive Services, 2007. WIAMIS '07. Eighth International Workshop on}.

}

\keyword{manip}
