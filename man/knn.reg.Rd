\name{knn.reg}
\alias{knn.reg}
\title{k Nearest Neighbor Regression}
\description{
  k-nearest neighbor regression
}
\usage{
knn.reg(train, test = NULL, y, k = 3, use.all = FALSE, 
        algorithm=c("cover_tree", "kd_tree", "VR"))
}

\arguments{
  \item{train}{
  matrix or data frame of training set cases.
}
  \item{test}{
  matrix or data frame of test set cases. A vector will be interpreted
  as a row vector for a single case.
}
  \item{y}{reponse of each observation in the training set.}
  \item{k}{ number of neighbours considered.}
  \item{use.all}{only implemented for "VR" algorithm for handling of ties. If true, all distances equal to the \code{k}th
  largest are included. If false, a random selection of distances
  equal to the \code{k}th is chosen to use exactly \code{k} neighbours.
}
  \item{algorithm}{nearest neighbor search algorithm.}
}
\details{
  If test is not supplied, Leave one out cross-validation is performed and \emph{R-square} is the predicted R-square.
}
\value{
  A list with a vector of predicted values for the testing dataset.
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
    pac.knn.VR<- knn.reg(PAC$X, y=PAC$y, k=3, algorith="VR");
    
    plot(PAC$y, pac.knn$pred, xlab="y", ylab=expression(hat(y)))
    points(PAC$y, pac.knn.VR$pred, col=3);
  }  

}

\keyword{regression}
\keyword{nonparametric}
