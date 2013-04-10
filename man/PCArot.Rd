\name{PCArot}
\alias{PCArot}
\title{
Varimax rotation in PCAmix
}
\description{
Orthogonal rotation in PCAmix by maximization of the varimax function expressed in terms of PCAmix squared loadings (correlation ratios for qualitative variables and squared correlations for quantitative variables).
PCArot includes the ordinary varimax rotation in Principal Component Analysis (PCA) and rotation in Multiple Correspondence Analysis (MCA) as special cases.
}
\usage{
PCArot(obj, dim, itermax = 100, graph = TRUE)
}
\arguments{
  \item{obj}{
an object of class \code{PCAmix}.
}
  \item{dim}{
number of rotated Principal Components.
}
  \item{itermax}{
maximum number of iterations in the Kaiser's practical optimization algorithm based on successive pairwise planar rotations. 
}
  \item{graph}{
 boolean, if TRUE the following graphs are displayed for the first two dimensions after rotation: plot of the observations (the scores), plot of the variables (squared loadings)
 plot of the correlation circle (if quantitative variables are available), plot of the categories (if qualitative variables are available).
}
}

\value{
\item{theta}{angle of rotation if \code{dim} is equal to 2. }
\item{T}{matrix of rotation.}
\item{eig}{variances of the \code{ndim} dimensions after rotation.}
\item{scores}{a n by \code{ndim} numerical matrix which contains the rotated scores of the n observations.}
\item{scores.stand}{a n by \code{dim} numerical matrix which contains the rotated standardized scores of the n observations.}
\item{sload}{a p by \code{ndim} numerical matrix which contains the rotated squared loadings of the p variables. 
For quantitative variables (resp. qualitative), the rotated squared loadings are the squared correlations (resp. the correlation ratios) with the rotated standardized scores.}
\item{categ.coord}{'NULL' if \code{X.quali} is 'NULL'. Otherwise a m by \code{dim} numerical matrix which contains the coordinates of the m categories after rotation.
The coordinates of the categories after rotation are the averages of the rotated standardized scores of the observations in those categories.}
\item{quanti.cor}{'NULL' if \code{X.quanti} is 'NULL'. Otherwise a p1 by \code{dim} numerical matrix which contains the coordinates (the loadings) of the p1 quantitative variables after rotation.
The coordinates of the quantitative variables after rotation are correlations with the rotated standardized scores.}
\item{quali.eta2}{'NULL' if \code{X.quali} is 'NULL'. Otherwise a p2 by \code{dim} numerical matrix which contains the squared loadings of the p2 quantitative variables after rotation.
The squared loadings of the qualitative variables after rotation are correlation ratio with the rotated standardized scores.}
\item{coef}{Coefficients of the linear combinations of the quantitative variables and the categories for constructing the rotated principal components of PCAmix.}
\item{V}{The rotated standardized loadings.}
}
\references{
Chavent, M., Kuentz, V., Saracco, J. (2011), Orthogonal Rotation in PCAMIX. Advances in Classification and Data Analysis, Vol. 6, pp. 131-146.

Kiers, H.A.L., (1991), Simple structure in Component Analysis Techniques for mixtures of
qualitative and quantitative variables, Psychometrika, 56, 197-212.
}
\author{
Marie Chavent <marie.chavent@u-bordeaux2.fr>, Vanessa Kuentz, Benoit Liquet, Jerome Saracco
}

\seealso{
\code{\link{plot.PCAmix}}, \code{\link{summary.PCAmix}},\code{\link{PCAmix}},\code{\link{predict.PCAmix}}
}
\examples{

#PCAMIX:
data(wine)
X.quanti <- wine[,c(3:29)] 
X.quali <- wine[,c(1,2)] 
pca<-PCAmix(X.quanti,X.quali,ndim=4, graph=FALSE)
pca

rot<-PCArot(pca,3)
rot
rot$eig #percentages of variances after rotation

plot(rot,choice="ind",quali=wine[,1],
	posleg="bottomleft", main="Rotated scores")
plot(rot,choice="var",main="Squared loadings after rotation")
plot(rot,choice="categ",main="Categories after rotation")
plot(rot,choice="cor",main="Correlation circle after rotation")



#PCA:
data(decathlon)
quali<-decathlon[,13]
pca<-PCAmix(decathlon[,1:10], graph=FALSE)

rot<-PCArot(pca,3)
plot(rot,choice="ind",quali=quali,cex=0.8,
	posleg="topright",main="Scores after rotation")
plot(rot, choice="var", main="Squared correlations after rotation")
plot(rot, choice="cor", main="Correlation circle after rotation")

#MCA
data(flower)
mca <- PCAmix(X.quali=flower[,1:4],graph=FALSE)

rot<-PCArot(mca,2)
plot(rot,choice="ind",main="Scores after rotation")
plot(rot, choice="var", main="Correlation ratios after rotation")
plot(rot, choice="categ", main="Categories after rotation")


}

\keyword{ multivariate }