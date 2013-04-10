\name{tab.disjonctif.NA}
\alias{tab.disjonctif.NA}
\title{Construction of the indicator matrix of a qualitative data matrix}
\usage{
  tab.disjonctif.NA(tab)
}
\arguments{
  \item{tab}{a qualitative data matrix}
}
\value{
  res the indicator matrix with NA for the categories of
  the variable which is missing in tab
}
\description{
  Construction of the indicator matrix of a qualitative
  data matrix
}
\examples{
data(vnf)
X <- vnf[1:10,9:12]
tab.disjonctif.NA(X)
recodqual(X)
}

