\name{summary.PCAmix}
\alias{summary.PCAmix}
\title{
Summary of a 'PCAmix' object
}
\description{
This is a method for the function summary for objects of the class \code{PCAmix}.
}
\usage{
\method{summary}{PCAmix}(object, ...)
}
\arguments{
  \item{object}{
an object of class PCAmix obtained with the function \code{PCAmix} or \code{PCArot}.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}

\value{
Returns the matrix of squared loadings. For quantitative variables (resp. qualitative), squared loadings are the squared correlations (resp. the correlation ratios) with the scores or with the rotated (standardized) scores.
}
\references{
Chavent, M., Kuentz, V., Saracco, J. (2011), Orthogonal Rotation in PCAMIX

Kiers, H.A.L., (1991), Simple structure in Component Analysis Techniques for mixtures of
qualitative and quantitative variables, Psychometrika, 56, 197-212.
}
\author{
Marie Chavent <marie.chavent@u-bordeaux2.fr>, Vanessa Kuentz, Benoit Liquet, Jerome Saracco
}

\seealso{
\code{\link{plot.PCAmix}},\code{\link{PCAmix}},\code{\link{PCArot}}, 
}

\examples{
data(wine)
X.quanti <- wine[,c(3:29)] 
X.quali <- wine[,c(1,2)] 
pca<-PCAmix(X.quanti,X.quali,ndim=4, graph=FALSE)
summary(pca)

rot<-PCArot(pca,3,graph=FALSE)
summary(rot)
}

