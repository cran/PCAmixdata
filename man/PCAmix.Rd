\name{PCAmix}
\alias{PCAmix}
\title{Principal Component Analysis for a mixture of qualitative and quantitative variables}
\usage{
  PCAmix(X.quanti = NULL, X.quali = NULL, ndim = 5,
    weight.col = NULL, weight.row = NULL, graph = TRUE)
}
\arguments{
  \item{X.quanti}{a numeric matrix of data, or an object
  that can be coerced to such a matrix (such as a numeric
  vector or a data frame with all numeric columns).}

  \item{X.quali}{a categorical matrix of data, or an object
  that can be coerced to such a matrix (such as a character
  vector, a factor or a data frame with all factor
  columns).}

  \item{ndim}{number of dimensions kept in the results (by
  default 5).}

  \item{graph}{boolean, if TRUE the following graphs are
  displayed for the first two dimensions of PCAmix: plot of
  the observations (the scores), plot of the variables
  (squared loadings) plot of the correlation circle (if
  quantitative variables are available), plot of the
  categories (if qualitative variables are available).}

  \item{weight.col}{a vector of weights for the
  quantitatives variables and for the indicator of qualitatives
  variables}

  \item{weight.row}{a vector of weights for the
  individuals}
}
\value{
  \item{eig}{eigenvalues (i.e. variances) of the Principal
  Components (PC).}

  \item{scores}{scores a n by ndim numerical matrix which
  contains the scores of the n observations on the ndim
  first Principal Components (PC).}

  \item{scores.stand}{a n by ndim numerical matrix which
  contains the standardized scores of the n observations on
  the ndim first Principal Components (PC).}

  \item{sload}{a p by ndim numerical matrix which contains
  the squared loadings of the p variables on the ndim first
  PC. For quantitative variables (resp. qualitative),
  squared loadings are the squared correlations (resp. the
  correlation ratios) with the PC scores.}

  \item{categ.coord}{ 'NULL' if X.quali is 'NULL' .
  Otherwise a m by ndim numerical matrix which contains the
  coordinates of the m categories of the qualitative
  variables on the ndim first PC. The coordinates of the
  categories are the averages of the standardized PC scores
  of the observations in those categories.}

  \item{quanti.cor}{ 'NULL' if X.quanti is 'NULL'.
  Otherwise a p1 by ndim numerical matrix which contains
  the coordinates (the loadings) of the p1 quantitative
  variables on the ndim first PC. The coordinates of the
  quantitative variables are correlations with the PC
  scores.}

  \item{quali.eta2}{ 'NULL' if X.quali is 'NULL' .
  Otherwise a p2 by ndim numerical matrix which contains
  the squared loadings of the p2 qualitative variables on
  the ndim first PC. The squared loadings of the
  qualitative variables are correlation ratios with the PC
  scores.}

  \item{res.ind}{Results for the individuals (coord,contrib
  in percentage,cos2)}

  \item{res.quanti}{Results for the quantitatives variables
  (coord,contrib in percentage,cos2)}

  \item{res.categ}{Results for the categories of the
  categorials variables (coord,contrib in percentage,cos2)}

  \item{coef}{Coefficients of the linear combinations of
  the quantitative variables and the categories for
  constructing the principal components of PCAmix.}

  \item{V}{The standardized loadings.}

  \item{rec}{Results of the fonction
  recod(X.quanti,X.quali).}

  \item{M}{Metric used in the svd for the weights of the
  variables.}
}
\description{
  PCAmix is a principal component method for a mixture of
  qualitative and quantitative variables. PCAmix includes
  the ordinary principal component analysis (PCA) and
  multiple correspondence analysis (MCA) as special cases.
  Squared loadings are correlation ratios for qualitative
  variables and squared correlation for quantitative
  variables. Missing values are replaced by means for
  quantitative variables and by zeros in the indicator
  matrix for qualitative variables. Note that when all the
  p variables are qualitative, the scores of the n
  observations are equal to the usual scores of MCA times
  square root of p and the eigenvalues are then equal to
  the usual eigenvalues of MCA times p.
}
\examples{
#PCAMIX:
data(wine)
X.quanti <- wine[,c(3:29)]
X.quali <- wine[,c(1,2)]
pca<-PCAmix(X.quanti,X.quali,ndim=4)
pca<-PCAmix(X.quanti,X.quali,ndim=4,graph=FALSE)
pca$eig

#Scores on dim 1-2
plot(pca,choice="ind",quali=wine[,1],
    posleg="bottomleft",main="Scores")
#Scores on dim 2-3
plot(pca,choice="ind",axes=c(2,3),quali=wine[,1],
    posleg="bottomleft",main="Scores")
#Other graphics
plot(pca,choice="var",main="Squared loadings")
plot(pca,choice="categ",main="Categories")
plot(pca,choice="cor",xlim=c(-1.5,2.5),
    main="Correlation circle")
#plot with standardized scores:
plot(pca,choice="ind",quali=wine[,1],stand=TRUE,
    posleg="bottomleft",main="Standardized Scores")
plot(pca,choice="var",stand=TRUE,main="Squared loadings")
plot(pca,choice="categ",stand=TRUE,main="Categories")
plot(pca,choice="cor",stand=TRUE,main="Correlation circle")


#PCA:
data(decathlon)
quali<-decathlon[,13]
pca<-PCAmix(decathlon[,1:10])
pca<-PCAmix(decathlon[,1:10], graph=FALSE)
plot(pca,choice="ind",quali=quali,cex=0.8,
    posleg="topright",main="Scores")
plot(pca, choice="var",main="Squared correlations")
plot(pca, choice="cor",main="Correlation circle")


#MCA
data(flower)
mca <- PCAmix(X.quali=flower[,1:4])
mca <- PCAmix(X.quali=flower[,1:4],graph=FALSE)
plot(mca,choice="ind",main="Scores")
plot(mca,choice="var",main="Correlation ratios")
plot(mca,choice="categ",main="Categories")

#Missing values
data(vnf)
PCAmix(X.quali=vnf)
vnf2<-na.omit(vnf)
PCAmix(X.quali=vnf2)
}
\author{
  Marie Chavent \email{marie.chavent@math.u-bordeaux1.fr},
  Vanessa Kuentz, Benoit Liquet, Jerome Saracco
}
\references{
  {Chavent, M., Kuentz, V., Saracco, J. (2011), Orthogonal
  Rotation in PCAMIX. Advances in Classification and Data
  Analysis, Vol. 6, pp. 131-146.

  Kiers, H.A.L., (1991), Simple structure in Component
  Analysis Techniques for mixtures of qualitative and
  quantitative variables, Psychometrika, 56, 197-212.}
}
\keyword{multivariate}

