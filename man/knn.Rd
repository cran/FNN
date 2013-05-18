\name{knn}
\alias{knn}
\title{
  k-Nearest Neighbour Classification
}
\description{
  k-nearest neighbour classification for test set from training set. For
  each row of the test set, the \code{k} nearest (in Euclidean distance)
  training set vectors are found, and the classification is decided by
  majority vote, with ties broken at random. If there are ties for the
  \code{k}th nearest vector, all candidates are included in the vote.
}
\usage{
  knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE,
      algorithm=c("VR", "brute", "kd_tree", "cover_tree"))
}
\arguments{
  \item{train}{matrix or data frame of training set cases.}
  \item{test}{matrix or data frame of test set cases. A vector will be interpreted
  as a row vector for a single case.}
  \item{cl}{factor of true classifications of training set.}
  \item{k}{number of neighbours considered.}
  \item{l}{only implemented for "VR" algorithm. It is the minimum vote for definite decision, otherwise \code{doubt}. (More
  precisely, less than \code{k-l} dissenting votes are allowed, even if \code{k}
  is increased by ties.)
}
  \item{prob}{if this is true, the proportion of the votes for the winning class
  are returned as attribute \code{prob}.
}
  \item{use.all}{only implemented for "VR" algorithm for handling of ties. If true, all distances equal to the \code{k}th
  largest are included. If false, a random selection of distances
  equal to the \code{k}th is chosen to use exactly \code{k} neighbours.
}
  \item{algorithm}{nearest neighbor search algorithm.}
}
\value{
factor of classifications of test set. \code{doubt} will be returned as \code{NA}.
}
\author{Shengqiao Li. To report any bugs or suggestions please email: \email{shli@stat.wvu.edu.}}
\references{
  B.D. Ripley (1996). \emph{Pattern Recognition and Neural Networks.} Cambridge.

  M.N. Venables and B.D. Ripley (2002).
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\note{
  The code for \dQuote{VR} nearest neighbor searching is taken from \code{class} source.
  Here distances and indice of k nearest neighbors are also returned.

}
\seealso{
  \code{\link{ownn}}, \code{\link{knn.cv}} and \code{\link[class]{knn}} in \pkg{class}.
}
\examples{
    data(iris3)
    train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
    test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
    cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
    knn(train, test, cl, k = 3, prob=TRUE)
    attributes(.Last.value)
}
\keyword{classif}
\keyword{nonparametric}
