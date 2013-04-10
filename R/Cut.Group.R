#' @export
#' @title Cut.Group
#' @name Cut.Group
#' @description Cut a data frame into differents \code{G} data frames 
#' @param base the data.frame to be split with \code{n} rows and \code{p} columns
#' @param group a vector of size \code{p} whose values indicate at which group belongs each variable
#' @param name.group a vector of size \code{G} which contains names for each group we want to create
#' @return \item{list.group}{a list with each group created}
#' 
#' @examples
#' data(decathlon) 
#' Cut.Group(decathlon,group=c(rep(1,10),2,2,3),name.group=c("Epreuve","Classement","Competition"))
#' 


Cut.Group<-function(base,group,name.group){
  nbr.group<-length(unique(group))
  list.group<-list()
  for (i in 1:nbr.group){
    list.group[[i]]<-data.frame(base[,which(group==i)],check.names=T)
    colnames(list.group[[i]])<-colnames(base)[(group==i)]
  }
  names(list.group)<-name.group
  return(list.group)
}
