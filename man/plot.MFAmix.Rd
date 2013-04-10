\name{plot.MFAmix}
\alias{plot.MFAmix}
\title{Graphs of a MFAmix analysis}
\usage{
\method{plot}{MFAmix}(x, axes = c(1, 2), choice = "axes",
    lab.grpe = TRUE, lab.var = TRUE, lab.ind = TRUE,
    lab.par = FALSE, habillage = "ind", col.hab = NULL,
    invisible = NULL, partial = NULL, lim.cos2.var = 0,
    chrono = FALSE, xlim = NULL, ylim = NULL, cex = 1,
    title = NULL, palette = NULL, new.plot = FALSE,leg=TRUE, ...)
}
\arguments{
  \item{x}{an object of class MFAmix}

  \item{axes}{alength 2 vector specifying the components to
  plot}

  \item{choice}{a string corresponding to the graph that you
  want to do ("ind" for the individual or categorical
  variables graph, "var" for the correlation circle of the
  quantitative variables, "axes" for the graph of the
  partial axes, "group" for the groups representation,"loadings" for the squared loadings : squared correlation coefficient between numeric variables and axis and correlation ration between catogorical variables and axis)}

  \item{lab.grpe}{boolean, if TRUE, the labels of the
  groups are drawn}

  \item{lab.var}{boolean, if TRUE, the labels of the
  variables are drawn}

  \item{lab.ind}{boolean, if TRUE, the labels of the
  individuals and of the categories are drawn}

  \item{lab.par}{boolean, if TRUE, the labels of the
  partial points are drawn}

  \item{habillage}{string corresponding to the color which
  are used. If "ind", one color is used for each
  individual; if "group" the individuals are colored
  according to the group; else if it is the name of a
  categorical variable, it colors according to the
  different categories of this variable}

  \item{col.hab}{the colors to use. By default, colors are
  chosen}

  \item{invisible}{a string; for choix ="ind", the
  individuals can be omit (invisible = "ind"), or the
  categories of the the categorical variables (invisible=
  "quali")}

  \item{partial}{list of the individuals or of the
  categories for which the partial points should be drawn
  (by default, partial = NULL and no partial points are
  drawn)}

  \item{lim.cos2.var}{value of the square cosinus below
  whixh the points are not drawn}

  \item{chrono}{boolean, if TRUE, the partial points of a
  same point are linked (useful when groups correspond to
  different dates)}

  \item{xlim}{numeric vectors of length 2, giving the x
  coordinates range}

  \item{ylim}{numeric vectors of length 2, giving the y
  coordinates range}

  \item{cex}{cf. function \code{par} in the \bold{graphics}
  package}

  \item{title}{string corresponding to the title of the
  graph you want to draw (by default NULL and a title is
  chosen)}

  \item{new.plot}{boolean, if TRUE, a new graphical device
  is created}
  
  \item{palette}{the color palette used to draw the points.
  By default colors are chosen. If you want to define the colors:
  palette=palette(c("black","red","blue")); or you can use: 
  palette=palette(rainbow(30)), or in black and white for example:
  palette=palette(gray(seq(0,.9,len=25)))}
  
  \item{leg}{boolean, if TRUE, a legend is displayed}
  
  \item{...}{further arguments passed to or from othe
  methods}
}
\value{
  Returns the graph you want to plot
}
\description{
  Graphs of MFAmix analysis : plot of the individuals, the
  partial individuals, the groups, the numeric variables on
  the correlation circle, the categories of the categorials
  variables, the partial axes of each separates analyses
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

