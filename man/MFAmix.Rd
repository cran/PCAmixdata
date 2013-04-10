\name{MFAmix}
\alias{MFAmix}
\title{Multiple Factor Analysis for a mixture of qualitative and quantitative variables inside the groups}
\usage{
  MFAmix(data, group, name.group, ndim, graph = TRUE,
    axes = c(1, 2))
}
\arguments{
  \item{data}{a data frame with \code{n} rows and \code{p}
  colums containing all the variables. This data frame
  will be split into \code{G} groups according to the vector
  \code{group}}

  \item{group}{a vector of size \code{p} whose values
  indicate at which group each variable belongs}

  \item{name.group}{a vector of size \code{G} which
  contains names for each group we want to create. Each
  name must be written as a charactor chain without any
  spaces and any special characters. Only letters, numbers and "_" allowed.}

  \item{ndim}{number of dimensions kept in the results}

  \item{graph}{boolean, if TRUE (default) a graph is
  displayed}

  \item{axes}{a length 2 vector specifying the axes to
  plot}
}
\value{
  \item{eig}{a matrix containing all the eigenvalues, the
  percentage of variance and the cumulative percentage of
  variance}

  \item{separate.analyses}{the results for the separate
  analyses for each group}

  \item{group}{a list of matrices containing all the
  results for the groups (Lg and RV coefficients,
  coordinates, square cosine, contributions, distance to
  the origin)}

  \item{partial.axes}{a list of matrices containing all the
  results for the partial axes (coordinates, correlation
  between variables and axes)}

  \item{ind}{a list of matrices containing all the results
  for the individuals (coordinates, square cosine,
  contributions)}

  \item{ind.partiel}{a matrice containing the coordinates
  of the partial individuals}

  \item{quanti.var}{a list of matrices containing all the
  results for the quantitative variables (coordinates,
  contribution, cos2)}

  \item{quali.var}{a list of matrices containing all the
  results for the categorical variables (coordinates,
  contribution, cos2)}

  \item{global.pca}{The results of the MFAmix considered as
  a unique weighted PCAmix}
}
\description{
  Performs Multiple Factor Analysis in the sense of
  Escofier-Pages. Groups of variables can be quantitative,
  categorical or contain both quantitatives and
  categoricals variables.
}
\examples{
#MFAmix:
data(INSEE)

class.var<-c(1,1,1,1,1,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,
             5,5,1,1,1,1,6,7,7,7,7,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,8,8,1)
nom.groupes<-c("Log_Services","Population","Emplois","Condit_Famil",
      "Educ","Revenu","Environnement","Secu_et_Soins")
res<-MFAmix(data=INSEE,group=class.var,name.group=nom.groupes,ndim=5,graph=FALSE)
summary(res)
res$eig
res$group$Lg

#Eigenvalues of the separate analyses
res$recap.eig.separate

#Individuals map on dim 1-2 colored by MedGen
plot(res,choice="ind",invisible="quali",habillage="MedGen_R",
     cex=0.6,title="Individuals colored by  the Qualitative variable MedGen_R") 
#Categories maps on dim 2-3
plot(res,choice="ind",invisible="ind",cex=0.6,axes=c(2,3)) 
#Partial axes on dim 1-2
plot(res,choice="axes",invisible="ind",cex=0.6,habillage="group",
     col.hab=c("green","blue","red","skyblue1","orange","grey","purple","brown"))
#Groups representation on dim 1-2
plot(res,choice="group",cex=0.6,habillage="group")
#Correlation circle on dim 1-2
plot(res,choice="var",cex=0.6,habillage="group")
#Squared loadings on dim 1-2
plot(res,choice="loadings",cex=0.6,habillage="group")
#Some partial individuals
plot(res,choice="ind",invisible="quali",habillage="group",partial=c("33119","17410"),cex=0.6) 
#All partial individuals
plot(res,choice="ind",invisible="quali",habillage="group",partial="all",cex=0.6) 
}
\author{
  Marie Chavent <marie.chavent@u-bordeaux2.fr>, Vanessa Kuentz, Amaury Labenne, Benoit Liquet, Jerome Saracco
}
\references{
  {Escofier B et Pages J (1983), Methode pour l'analyse de plusieurs groupes de variables. Application a la caracterisation des vins rouges du Val de Loire, Revue de statistique appliquee, 31(2) : 43-59.
  
  Escofier B et Pages J (1998), Analyses factorielles simples et multiples, Dunod, 3e ed.}
}
\keyword{multivariate}

