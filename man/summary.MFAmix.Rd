\name{summary.MFAmix}
\alias{summary.MFAmix}
\title{
  Summary of a 'MFAmix' object
}
\description{
  This is a method for the function summary for objects of the class \code{MFAmix}.
}
\usage{
  \method{summary}{MFAmix}(object, ...)
}
\arguments{
  \item{object}{
    an object of class MFAmix obtained with the function \code{MFAmix}.
  }
  \item{\dots}{
    further arguments passed to or from other methods.
  }
}

\value{
  Returns the total number of observations, the number of numerical variables, the number of categorical variables with the total number of categories. And all those values are declinated by groups.
}


\seealso{
  \code{\link{plot.MFAmix}},\code{\link{MFAmix}} 
}


