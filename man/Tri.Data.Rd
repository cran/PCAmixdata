\name{Tri.Data}
\alias{Tri.Data}
\title{Tri.Data}
\usage{
  Tri.Data(base)
}
\arguments{
  \item{base}{the data.frame with numeric variables and
  factors}
}
\value{
  \item{X.quanti}{The sub data.frame with only numeric
  variables}

  \item{X.quali}{The sub data.frame with only categorial
  variables}
}
\description{
  Cut a data frame with numeric variables and factors into
  two data frames, one with only numeric variables and the
  other one with only factors variables.
}
\examples{
data(decathlon)
Tri.Data(decathlon)
}

