\name{mutinfo}
\alias{mutinfo}
\title{Mutual Information}
\description{KNN Mutual Information Estimators.}
\usage{
  mutinfo(X, Y, k=10, direct=TRUE)
}

\arguments{
  \item{X}{an input data matrix.}
  \item{Y}{an input data matrix.}

  \item{k}{the maximum number of nearest neighbors to search. The default value is set to 10.}
  \item{direct}{Directly compute or via entropies.}
}

\details{
   The direct computation is based on the first estimator of A. Kraskov, H. Stogbauer and P.Grassberger (2004) and 
   the indirect computation is done via entropy estimates, i.e., I(X, Y) = H (X) + H(Y) - H(X, Y).
   The direct method has smaller bias and variance but the indirect method is faster, see Evans (2008).
}
\value{
  For direct method,  one mutual information estimate;
  For indirect method,a vector of length \code{k} for mutual information estimates using \code{1:k} nearest neighbors, respectively.
}


\author{Shengqiao Li. To report any bugs or suggestions please email: \email{lishengqiao@yahoo.com}}

\references{

A. Kraskov, H. Stogbauer and P.Grassberger (2004).
\dQuote{Estimating mutual information}.
\emph{Physical Review E}, \bold{69}:066138, 1--16.

D. Evans (2008).
\dQuote{A Computationally efficient estimator for mutual information}.
\emph{Proc. R. Soc. A}, \bold{464}, 1203--1215.

}

\keyword{manip}
