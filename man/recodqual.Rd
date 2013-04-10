\name{recodqual}
\alias{recodqual}
\title{Recoding of the qualitative data matrix}
\usage{
  recodqual(X)
}
\arguments{
  \item{X}{the qualitative data matrix}
}
\value{
  \item{G}{The indicator matix of X with missing values
  replaced by 0}
}
\description{
  Recoding of the qualitative data matrix
}
\examples{
data(vnf)
X <- vnf[1:10,9:12]
tab.disjonctif.NA(X)
recodqual(X)
}

