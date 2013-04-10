\name{Cut.Group}
\alias{Cut.Group}
\title{Cut.Group}
\usage{
  Cut.Group(base, group, name.group)
}
\arguments{
  \item{base}{the data.frame to be split with \code{n} rows
  and \code{p} columns}

  \item{group}{a vector of size \code{p} whose values
  indicate at which group belongs each variable}

  \item{name.group}{a vector of size \code{G} which
  contains names for each group we want to create}
}
\value{
  \item{list.group}{a list with each group created}
}
\description{
  Cut a data frame into differents \code{G} data frames
}
\examples{
data(decathlon)
Cut.Group(decathlon,group=c(rep(1,10),2,2,3),name.group=c("Epreuve","Classement","Competition"))
}

