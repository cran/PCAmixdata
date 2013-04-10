#' @export
#' @title Recoding of the quantitative data matrix
#' @name recodquant
#' @param X the quantitative data matrix
#' @return \item{Z}{the standardized quantitative data matrix (centered and reduced with the standard deviations)}
#' @return \item{g}{the means of the columns of X}
#' @return \item{s}{the standard deviations of the columns of X (population version with 1/n)}
#' @return \item{Xcod}{The quantitative matix X with missing values replaced with the column's mean values}
#' @examples 
#'data(decathlon)
#'X <- decathlon[1:5,1:5]
#'X[1,2] <- NA
#'X[2,3] <-NA
#'rec <- recodquant(X)

recodquant <-
  function(X)
  {
    X <- as.matrix(X)
    missing.mean <-
      function(C1){
        moy <- mean(C1,na.rm=T)
        ind <- which(is.na(C1)==T)
        if(length(ind)>=1){C1[ind]<-moy
        }
        return(C1)
      }
    Xcod <- apply(X,2,missing.mean)
    red <- sqrt((nrow(X)-1)/nrow(X))
    sd.Xcod <- apply(Xcod,2,sd)*red
    mean.Xcod <- apply(Xcod,2,mean)
    Z<- scale(Xcod,scale=sd.Xcod) 
    apply(Z,1,function(x) sum(is.na(x))) 
    if (sum(is.na(Z))!= 0) stop("There are columns in X.quanti where all the values are identical")
    return(list(Z=Z,g=mean.Xcod,s=sd.Xcod,Xcod=Xcod))
  }
