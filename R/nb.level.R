#' @export
#' @title nb.level
#' @name nb.level
#' @description Return the number of levels of a factor
#' @param fact a factor
#' @return The number of levels of the factor

nb.level<-function(fact){
  return(length(levels(factor(fact))))
} 
