\name{KL.divergence}
\alias{KL.divergence}
\alias{KLx.divergence}


\title{Kullback-Leibler Divergence}
\description{Compute Kullback-Leibler divergence.}
\usage{
  KL.divergence(X, Y, k = 10, algorithm=c("VR", "brute", "kd_tree", "cover_tree"))
  KLx.divergence(X, Y, k = 10, algorithm="kd_tree")
}

\arguments{
  \item{X}{An input data matrix.}
  \item{Y}{An input data matrix.}
  \item{k}{The maximum number of nearest neighbors to search. The default value
  is set to 10.}
  \item{algorithm}{nearest neighbor search algorithm.}
}

\value{
Return the Kullback-Leibler divergence from \code{X} to \code{Y}.
}

\details{
   If \code{p(x)} and \code{q(x)} are two continuous probability density functions,
   then the Kullback-Leibler divergence from \code{p} to \code{q} is defined as
   \eqn{E_p[\log \frac{p(x)}{q(x)}]}{E_p[log p(x)/q(x)]}.

   \code{KL.*} versions return divergences from \code{C} code to \code{R} but \code{KLx.*} do not.

}
\seealso{
\code{\link{KL.dist}}
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

    KL.divergence(X, Y, k=5)
    #theoretical divergence = log(0.2/0.4)+(0.4-0.2)-1 = 1-log(2) = 0.307
}
\keyword{manip}
