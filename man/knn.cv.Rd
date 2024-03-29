\name{knn.cv}
\alias{knn.cv}

\title{k-Nearest Neighbour Classification Cross-Validation}

\description{k-nearest neighbour classification cross-validation from training set.}

\usage{
knn.cv(train, cl, k = 1, prob = FALSE, algorithm=c("kd_tree",
       "cover_tree", "brute"))
}
\arguments{
\item{train}{matrix or data frame of training set cases.}
\item{cl}{factor of true classifications of training set}
\item{k}{number of neighbours considered.}
\item{prob}{if this is true, the proportion of the votes for the winning class
are returned as attribute \code{prob}.
}

\item{algorithm}{nearest neighbor search algorithm.}
}
\details{
This uses leave-one-out cross validation.
For each row of the training set \code{train}, the \code{k} nearest
(in Euclidean distance) other training set vectors are found, and the classification 
is decided by majority vote, with ties broken at random. If there are ties for the
\code{k}th nearest vector, all candidates are included in the vote.
}
\value{
factor of classifications of training set. \code{doubt} will be returned as \code{NA}.
distances and indice of k nearest neighbors are also returned as attributes.
}
\author{Shengqiao Li. To report any bugs or suggestions please email: \email{lishengqiao@yahoo.com}}

\references{
  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks.} Cambridge.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}

\seealso{
\code{\link{knn}} and \code{\link[class]{knn.cv}} in \pkg{class}.
}
\examples{
  data(iris3)
  train <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
  cl <- factor(c(rep("s",50), rep("c",50), rep("v",50)))
  knn.cv(train, cl, k = 3, prob = TRUE)
  attributes(.Last.value)
}
\keyword{classif}
\keyword{nonparametric}
