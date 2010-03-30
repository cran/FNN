\name{entropy}
\alias{entropy}
\title{Shannon Entropy}
\description{KNN Shannon Entropy Estimators.}
\usage{
  entropy(X, k = 10, algorithm = "kd_tree")
}

\arguments{
  \item{X}{an input data matrix.}  
  \item{k}{the maximum number of nearest neighbors to search. The default value is set to 10.}
  \item{algorithm}{nearest neighbor search algorithm.}
}

\value{
  a vector of length \code{k} for entropy estimates using \code{1:k} nearest neighbors, respectively.
}

\author{Shengqiao Li. To report any bugs or suggestions please email: \email{shli@stat.wvu.edu.}}

\references{

H. Singh, N. Misra, V. Hnizdo, A. Fedorowicz and E. Demchuk (2003). \dQuote{Nearest neighbor
estimates of entropy}. \emph{American Journal of Mathematical and Management Sciences}, \bold{23}, 301-321.

M.N. Goria, N.N.Leonenko, V.V. Mergel and P.L. Novi Inverardi (2005). 
\dQuote{A new class of random vector entropy estimators and its applications in testing statistical hypotheses}.
\emph{Journal of Nonparametric Statistics}, \bold{17}:3, 277--297.

R.M. Mnatsakanov, N. Misra, S. Li and E.J. Harner (2008). \dQuote{K_n-nearest neighbor estimators of entropy}.
\emph{Mathematical Methods of Statistics}, \bold{17}:3, 261-277.

}

\keyword{manip}
