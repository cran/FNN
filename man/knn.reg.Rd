\name{knn.reg}
\alias{knn.reg}
\title{k Nearest Neighbor Regression}
\description{
  k-nearest neighbor regression
}
\usage{
knn.reg(train, test = NULL, y, k = 3, algorithm=c("kd_tree", "cover_tree", "brute"))
}

\arguments{
  \item{train}{
  matrix or data frame of training set cases.
}
  \item{test}{
  matrix or data frame of test set cases. A vector will be interpreted
  as a row vector for a single case. If not supplied, cross-validataion will be done.
}
  \item{y}{reponse of each observation in the training set.}
  \item{k}{ number of neighbours considered.}
  \item{algorithm}{nearest neighbor search algorithm.}
}
\details{
  If test is not supplied, Leave one out cross-validation is performed and \emph{R-square} is the predicted R-square.
}
\value{

\code{knn.reg} returns an object of \code{class} \code{"knnReg"} or \code{"knnRegCV"} 
if \code{test} data is not supplied.

The returnedobject is a list containing at least the following components:

  \item{call}{the match call.}
  \item{k}{number of neighbours considered.}
  \item{n}{number of predicted values, either equals test size or train size.}
  \item{pred}{a vector of predicted values.}
  \item{residuals}{predicted residuals. \code{NULL} if \code{test} is supplied.}
  \item{PRESS}{the sums of squares of the predicted residuals. \code{NULL} if \code{test} is supplied.}
  \item{R2Pred}{predicted R-square. \code{NULL} if \code{test} is supplied.}
 
}
\seealso{
 \code{\link{knn}}.
}
\note{
  The code for \dQuote{VR} nearest neighbor searching is taken from \code{class} source
}
\author{Shengqiao Li. To report any bugs or suggestions please email: \email{shli@stat.wvu.edu.}}

\examples{
  if(require(chemometrics)){
    data(PAC);
    pac.knn<- knn.reg(PAC$X, y=PAC$y, k=3);
    
    plot(PAC$y, pac.knn$pred, xlab="y", ylab=expression(hat(y)))
  } 
}

\keyword{regression}
\keyword{nonparametric}
