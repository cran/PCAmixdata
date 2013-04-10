\name{predict.PCAmix}
\alias{predict.PCAmix}
\title{Scores of new objects on the principal components of PCAmix or PCArot}
\usage{
  \method{predict}{PCAmix}(object, X.quanti = NULL, X.quali = NULL,...)
}
\arguments{
  \item{object}{an object of class PCAmix (output of the
  function PCAmix or PCArot)}

  \item{X.quanti}{numeric matrix of data for the new
  objects}

  \item{X.quali}{a categorical matrix of data for the new
  objects}
  
  \item{...}{futher arguments pased to or from other methods}
}
\value{
  Returns the matrix of the scores of the new objects of
  the ndim principal PC or rotated PC.
}
\description{
  This function calculates the scores of a new set of data
  on the principal components of PCA. If the components
  have been rotated, the function gives the scores of the
  new objects on the rotated PC. The new objects must have
  the same variables than the learning set.
}
\examples{
data(decathlon)
n <- nrow(decathlon)
sub <- sample(1:n,20)
pca<-PCAmix(decathlon[sub,1:10], graph=FALSE)
predict(pca,decathlon[-sub,1:10])
rot <- PCArot(pca,dim=4)
predict(rot,decathlon[-sub,1:10])
}
\author{
  Marie Chavent \email{marie.chavent@math.u-bordeaux1.fr},
  Vanessa Kuentz, Benoit Liquet, Jerome Saracco
}

