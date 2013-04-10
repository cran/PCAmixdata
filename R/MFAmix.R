#' @export
#' @name MFAmix
#' @title Multiple Factor Analysis for a mixture of qualitative and quantitative variables inside the groups
#' @description Performs Multiple Factor Analysis in the sense of Escofier-Pages. Groups of variables can be quantitative,
#' categorical or contain both quantitatives and categoricals variables.
#' @param data a data frame with \code{n} rows and \code{p} colums containing all the variables.
#' This data data frame will be split in \code{G} groups according to the vector \code{group}
#' @param group a vector of size \code{p} whose values indicate at which group belongs each variable
#' @param name.group a vector of size \code{G} which contains names for each group we want to create.
#' Each name must be written as a charactor chain without any spaces
#' @param ndim number of dimensions kept in the results
#' @param graph boolean, if TRUE (default) a graph is displayed
#' @param axes a length 2 vector specifying the axes to plot
#' @return \item{eig}{a matrix containing all the eigenvalues, the percentage of variance and the cumulative percentage of variance}
#' @return \item{separate.analyses}{the results for the separate analyses for each group}
#' @return \item{group}{a list of matrices containing all the results for the groups 
#' (Lg and RV coefficients, coordinates, square cosine, contributions, 
#' distance to the origin)}
#' @return \item{partial.axes}{a list of matrices containing all the results for the 
#' partial axes (coordinates, correlation between variables and axes)}
#' @return \item{ind}{a list of matrices containing all the results for the
#' individuals (coordinates, square cosine, contributions)}
#' @return \item{ind.partiel}{a matrice containing the coordinates of the partial individuals}
#' @return \item{quanti.var}{a list of matrices containing all the results for the quantitative variables
#' (coordinates, contribution, cos2)}
#' @return \item{quali.var}{a list of matrices containing all the results for the categorical variables
#' (coordinates, contribution, cos2)}
#' @return \item{global.pca}{The results of the MFAmix considered as a unique weighted PCAmix} 
#' @examples
#' #blablabla
#' blablablza
#'@keywords multivariate
MFAmix<-function(data,group,name.group,ndim,graph=TRUE,axes=c(1,2)){
  
  cl <- match.call()
  
  
  n<-nrow(data)
  
  nbr.group<-length(unique(group))
  Lst.group<-Cut.Group(base=data,group=group,name.group=name.group) 
  long.group<-sapply(Lst.group,ncol)
  typ.group<-unlist(sapply(Lst.group,Tri.Data)[3,])
  
  #Si group n est pas dans l ordre (ex: c(1,1,1,2,2,1,3,3))
  #On rearrange data et le vect group
  DATA.ord<-data.frame(matrix(NA,ncol=ncol(data),nrow=nrow(data)))
  init<-0
  for(g in 1:nbr.group){
    DATA.ord[,c((1+init):(init+ncol(Lst.group[[g]])))]<-Lst.group[[g]]
    init<-init+ncol(Lst.group[[g]])
  }
  colnames(DATA.ord)<-unlist(sapply(Lst.group,colnames))
  rownames(DATA.ord)<-rownames(data)
  group.ord<-NULL
  for(g in 1:nbr.group){
    group.ord<-c(group.ord,rep(g,ncol(Lst.group[[g]])))
  }
  group<-group.ord
  data<-DATA.ord
  
  ordre.var.data<-data.frame(colnames(data),seq(from=1,by=1,to=ncol(data)))
  colnames(ordre.var.data)<-c("nom.var","ordre.var")
  
  
  
  tab.indic.names<-cbind(colnames(data),rep(name.group,long.group),rep(typ.group,long.group),rep(long.group,long.group))
  rownames(tab.indic.names)<-NULL
  tab.indic.names<-data.frame(tab.indic.names)
  colnames(tab.indic.names)<-c("var","group","type","nb.var")
  
  
  
  Res.separe.pcamix<-list()
  
  for(i in 1:nbr.group){
    base.qt<-Tri.Data(Lst.group[[i]])$X.quanti
    base.ql<-Tri.Data(Lst.group[[i]])$X.quali
    Res.separe.pcamix[[i]]<-PCAmix(X.quanti=base.qt,X.quali=base.ql,ndim=ndim,graph=F)
  }
  names(Res.separe.pcamix)<-name.group
  
  eig.group<-list()
  for(i in 1:nbr.group){
    eig.group[[i]]<-Res.separe.pcamix[[i]]$eig[1]
  }
  eig.group<-unlist(eig.group)
  names(eig.group)<-paste(name.group)
  
  
  ponde.qt<-NULL
  base.qt<-Tri.Data(data)$X.quanti
  indic.qt.group<-tab.indic.names[match(colnames(base.qt),tab.indic.names$var),2]
  ponde.qt<-eig.group[match(indic.qt.group,names(eig.group))]

  ponde.ql<-NULL
  base.ql<-Tri.Data(data)$X.quali
  if (is.null(base.ql)==FALSE){
    indic.ql.group<-tab.indic.names[match(colnames(base.ql),tab.indic.names$var),2]
    ponde.ql<-eig.group[match(indic.ql.group,names(eig.group))]
    ponde.ql<-rep(ponde.ql,apply(base.ql,2,nb.level))
  }
  
  ponderation<-c(ponde.qt,ponde.ql)
  ponderation<-1/ponderation
  Res.total<-PCAmix(X.quanti=base.qt,X.quali=base.ql,ndim=ndim,graph=FALSE,weight.col=ponderation)
  
  
  #On rearrange les squared loadings pour qu'ils soient dans l ordre en suivant les groupes
  sload.order<-data.frame(matrix(NA,ncol=ncol(Res.total$sload),nrow=nrow(Res.total$sload)))
  for (i in 1:nrow(Res.total$sload)){
    index<-as.character(ordre.var.data[i,1])
    sload.order[i,]<-Res.total$sload[index,]
  }
  rownames(sload.order)<-ordre.var.data[,1]
  colnames(sload.order)<-colnames(Res.total$sload)
  Res.total$sload<-sload.order
  ##################
  ponderation.group<-ponderation[match(name.group,names(ponderation))]
  ndim<-ncol(Res.total$scores)
  
  
  #Individus partiels
  data.partiel <- vector(mode = "list", length = nbr.group)
  names(data.partiel) <- name.group
  
  for (g in 1:nbr.group){
    col.interet<-rownames(Res.separe.pcamix[[g]]$V)
    data.partiel[[g]]<-data.frame(Res.total$W[,col.interet])
    colnames(data.partiel[[g]])<-col.interet
  }
  
  V<-Res.total$V
  M<-Res.total$M
  
  res.ind.partiel <- vector(mode = "list", length = nbr.group)
  names(res.ind.partiel) <- name.group
  for (g in 1:nbr.group) {
    coord.ind.sup<-nbr.group*data.partiel[[g]]
    coord.ind.sup<-sweep(coord.ind.sup,2,STATS=M[colnames(coord.ind.sup)],FUN="*")
    coord.ind.sup<-as.matrix(coord.ind.sup)%*%V[colnames(coord.ind.sup),]
    res.ind.partiel[[g]]$coord.sup <- coord.ind.sup
  }
  
  ndim.max.group<-NULL
  for (g in 1:nbr.group){
    ndim.max.group<-c(ndim.max.group,ncol(res.ind.partiel[[g]]$coord.sup))
  }
  
  nom.ligne <- NULL
  for (i in 1:n) {
    ind.tmp <- rownames(data)[i]
    nom.ligne <- c(nom.ligne, paste(ind.tmp, name.group, sep = "."))
  }
  coord.ind.partiel <- as.data.frame(matrix(NA, (n *nbr.group), ndim))
  rownames(coord.ind.partiel) <- nom.ligne
  colnames(coord.ind.partiel) <- paste("Dim", c(1:ndim), sep = ".")  
  liste.ligne <- seq(1, n* nbr.group, by = nbr.group)  
  for (g in 1:nbr.group){
    coord.ind.partiel[liste.ligne +  g - 1, ] <- res.ind.partiel[[g]]$coord.sup[,1:ndim.max.group[g]]
  }
  
  #Rapports d inertie
  Inertie.tot <- vector(length = ndim)
  for (g in 1:nbr.group){
    Inertie.tot <- Inertie.tot + apply(res.ind.partiel[[g]]$coord.sup^2 * n, 2, sum)}
  
  rap.inertie <- apply(Res.total$res.ind$coord^2 * n, 2, sum) * nbr.group/Inertie.tot
  

  #Resultats sur les axes partiels
  Res.partial.axes.group<-function(nom.group){
    score.sep<-eval(parse(text=paste("Res.separe.pcamix$",nom.group,"$scores",sep="")))
    colnames(score.sep)<-paste(colnames(score.sep),nom.group,sep=".")
    score.global<-Res.total$scores
    cor(score.sep,score.global)  
  }
  
  partial.axes.coord<-NULL
  for (g in 1:nbr.group) {
    a<-Res.partial.axes.group(name.group[g])
    partial.axes.coord<-rbind(partial.axes.coord,a)
  }
  partial.axes.coord
  
  
  #Contribution de toutes les variables                                     
  contrib.total<-rbind(Res.total$res.quanti$contrib,Res.total$res.categ$contrib.quali)
  contrib.total<-contrib.total[match(as.vector(tab.indic.names$var),rownames(contrib.total)),]
  
  
  #Resultats pour les groupes
  contrib.group<-data.frame(matrix(NA,nrow=nbr.group,ncol=ndim))
  a<-0
  for (i in 1:nbr.group){
    if (is.vector(contrib.total[(a+1):(a+long.group[i]),]))
    {contrib.group[i,]<-contrib.total[(a+1):(a+long.group[i]),]} else
    { contrib.group[i,]<-apply(contrib.total[(a+1):(a+long.group[i]),],2,sum)
    }
    a<-a+long.group[i]
  }
  rownames(contrib.group)<-name.group
  colnames(contrib.group)<-colnames(partial.axes.coord)
  
  
  coord.group<-sweep(contrib.group/100,2,STATS=Res.total$eig[1:ndim,1],FUN="*")
  
  dist.group<-NULL
  for (i in 1:nbr.group){
    valP<-Res.separe.pcamix[[i]]$eig[,1]
    dis<-valP/valP[1]
    dis<-sum(dis^2)
    dist.group<-c(dist.group,dis)
  }
  names(dist.group)<-name.group
  
  cos2.group<-sweep(coord.group^2,1,STATS=dist.group,FUN="/")
  
  Lg.group<-Lg.pond(Lst.group,ponderation.group)
  RV.group<-RV.pond(Lst.group,ponderation.group)
  
  #Reorganisation des résultats
  res.group<-list(Lg=Lg.group,RV=RV.group,coord=coord.group,contrib=contrib.group,dist2=dist.group,cos2=cos2.group)
  res.partial.axes<-list(coord=partial.axes.coord,cor=partial.axes.coord)  
  Res.total$res.ind$coord.partiel<-coord.ind.partiel
  
  #Recapitulatif des valeurs propres des analyses séparées
  Recap.eig<-list()
  for (i in 1:nbr.group){
    if (nrow(Res.separe.pcamix[[i]]$eig)<ndim){
      Recap.eig[[i]]<-c(Res.separe.pcamix[[i]]$eig[,1],rep(NA,ndim-nrow(Res.separe.pcamix[[i]]$eig)))
    } else       {
      Recap.eig[[i]]<-c(Res.separe.pcamix[[i]]$eig[1:ndim,1])
    }
  }

  Recap.eig.frame<-matrix(NA,ncol=nbr.group,nrow=ndim)
  for (i in 1:nbr.group){
    Recap.eig.frame[,i]<-Recap.eig[[i]]
  }
  colnames(Recap.eig.frame)<-name.group
  rownames(Recap.eig.frame)<- paste("dim", 1:ndim, sep =" ")
  
  
  res<-list(call=cl,eig=Res.total$eig,separate.analyses=Res.separe.pcamix,group=res.group,partial.axes=res.partial.axes,inertia.ratio=rap.inertie,
            ind=Res.total$res.ind,quanti.var=Res.total$res.quanti,quali.var=Res.total$res.categ,
            global.pca=Res.total,ind.partiel=res.ind.partiel,lst.group=group,recap.eig.separate=Recap.eig.frame)
  
  
  class(res)<-c("MFAmix","list")
  print.MFAmix(res)
  
  if (graph) {  
    plot.MFAmix(res,axes=axes,choice="axes",habillage="group")
    plot.MFAmix(res,axes=axes,choice="group",habillage="ind")
    plot.MFAmix(res,axes=axes,choice="ind",invisible="quali")
    plot.MFAmix(res,axes=axes,choice="loadings",habillage="group")
    
    if (!is.null(Res.total$res.quanti$coord)){
      plot.MFAmix(res,axes=axes,choice="var",habillage="group")
    }
    if (!is.null(Res.total$res.categ$coord)){
      plot.MFAmix(res,axes=axes,choice="ind",invisible="ind")
    }
  }
  
  return(res)
}
