\name{ownn}
\alias{ownn}
\title{
  Optimal Weighted Nearest Neighbor Classification
}
\description{
  This function implements Samworth's optimal weighting scheme for k nearest neighbor classification. The performance improvement is greatest when the dimension is 4 as reported in the reference.
}
\usage{
  ownn(train, test, cl, testcl=NULL, k = NULL, prob = FALSE,
      algorithm=c("kd_tree", "cover_tree", "brute"))
}
\arguments{
  \item{train}{matrix or data frame of training set cases.}
  \item{test}{matrix or data frame of test set cases. A vector will be interpreted
  as a row vector for a single case.}
  \item{cl}{factor of true classifications of training set.}
  \item{testcl}{factor of true classifications of testing set for error rate calculation.}
  \item{k}{number of neighbours considered, chosen by 5-fold cross-validation if not supplied.}
  \item{prob}{if this is true, the proportion of the weights for the winning class
  are returned as attribute \code{prob}.}
  \item{algorithm}{nearest neighbor search algorithm.}
}  
\value{
  a list includes k, predictions by ordinary knn, optimal weighted knn and bagged knn, and accuracies if class labels of test data set are given. 
}
\author{Shengqiao Li. To report any bugs or suggestions please email: \email{shli@stat.wvu.edu.}}
\references{
  Richard J. Samworth (2012),
  \dQuote{Optimal Weighted Nearest Neighbor Classifiers,} \emph{Annals of Statistics},  \bold{40:5}, 2733-2763.
}
\seealso{
  \code{\link{knn}} and \code{\link[class]{knn}} in \pkg{class}.
}
\examples{
    data(iris3)
    train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
    test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
    cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
    testcl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
    out <- ownn(train, test, cl, testcl)
    out
}
\keyword{classif}
\keyword{nonparametric}
