\name{predict.MFAmix}
\alias{predict.MFAmix}
\title{Scores of new objects on the principal components of MFAmix}
\usage{
  \method{predict}{MFAmix}(object, data, group,name.group,...)
}
\arguments{
  \item{object}{an object of class MFAmix (output of the
  function MFAmix)}

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
  
  \item{...}{further arguments passed to or from othe
  methods}
}
\value{
  Returns the matrix of the scores of the new objects of
  the ndim principal PC.
}
\description{
  This function calculates the scores of a new set of data
  on the principal components of MFAmix. The new objects must have
  the same variables than the learning set.
}
\examples{
#MFAmix:
data(INSEE)
class.var<-c(1,1,1,1,1,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,
             5,5,1,1,1,1,6,7,7,7,7,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,8,8,1)
nom.groupes<-c("Log_Services","Population","Emplois","Condit_Famil",
      "Educ","Revenu","Environnement","Secu_et_Soins")
res<-MFAmix(data=INSEE,group=class.var,name.group=nom.groupes,ndim=5,graph=FALSE)

#Predict on the same data, we obtain the same scores
res.pred<-predict(res,data=INSEE,group=class.var,name.group=nom.groupes)
res$ind$coord/res.pred

}
\author{
  Amaury Labenne \email{amaury.labenne@irstea.fr},
  Marie Chavent, Vanessa Kuentz, Benoit Liquet, Jerome Saracco
}

