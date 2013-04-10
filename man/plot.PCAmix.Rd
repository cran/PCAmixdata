\name{plot.PCAmix}
\alias{plot.PCAmix}
\title{
Graphs of a PCAmix analysis before or after rotation
}
\description{
Graphs of PCAmix analysis before or after rotation: plot of the observations (the scores or the standardized scores), plot of the variables (squared loadings)
 correlation circle of the available quantitative variables, plot of the categories of the available qualitative variables. 
}
\usage{
\method{plot}{PCAmix}(x,axes = c(1, 2), choice = "ind", stand=FALSE,label=TRUE,
quali=NULL,posleg="topleft",xlim=NULL,ylim=NULL,cex=1,col.var=NULL,...)
}
\arguments{
  \item{x}{
  an object of class PCAmix obtained with the function \code{PCAmix} or \code{PCArot}.
}
  \item{axes}{
 a length 2 vector specifying the components to plot.
}
  \item{choice}{
 the graph to plot ("ind" for the observations, "var" for the variables (plot of the squared loadings), "cor" for the correlation circle if quantitative variables are available,
 "categ" for the categories if qualitative variables are available.
}
  \item{stand}{
  if 'TRUE' the standardized scores are used in the plot of the observations.
}
  \item{label}{
  if 'FALSE' the labels of the points are not plotted
}
  \item{quali}{
a qualitative variable such as a character vector or a factor of size n (the number of observation). 
The observations are colored according to the categories of this variable.
}
  \item{posleg}{
position of the legend if \code{quali} is not 'NULL'.
}
  \item{xlim}{
numeric vectors of length 2, giving the x coordinates range.
}
  \item{ylim}{
numeric vectors of length 2, giving the y coordinates range.
}

  \item{cex}{cf. function \code{par} in the \bold{graphics}
package}
  
 \item{col.var}{
a vector of size p (the total n umber of variables) which allows to colorate each variable with a different color
}


  \item{\dots}{
further arguments passed to or from other methods.
}
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
\code{\link{summary.PCAmix}},\code{\link{PCAmix}},\code{\link{PCArot}}, 
}

\examples{

#PCAMIX:
data(wine)
X.quanti <- wine[,c(3:29)] 
X.quali <- wine[,c(1,2)] 
pca<-PCAmix(X.quanti,X.quali,ndim=4, graph=FALSE)

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

rot<-PCArot(pca,3,graph=FALSE)
plot(rot,choice="ind", main="Rotated scores",label=FALSE)
plot(rot,choice="var",main="Squared loadings after rotation")
plot(rot,choice="categ",main="Categories after rotation")
plot(rot,choice="cor",main="Correlation circle after rotation")


#PCA:
data(decathlon)
quali<-decathlon[,13]
pca<-PCAmix(decathlon[,1:10], graph=FALSE)
plot(pca,choice="ind",quali=quali,stand=TRUE,cex=0.8,
	posleg="topright",main="Standardized scores")
plot(pca, choice="cor", stand=TRUE,main="Correlation circle")

}

