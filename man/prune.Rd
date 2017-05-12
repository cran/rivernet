\name{prune}

\alias{prune}

\title{
Prune a river network at specified locations
}

\description{
Prune a river network at specified locations.
}

\usage{
prune(net,x.up=NA,y.up=NA,x.dn=NA,y.dn=NA,verbose=TRUE)
}

\arguments{
  \item{net}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{x.up}{
Numerical vector of x coordinates of points from which to remove upstream reaches (needs to be of the same length as argument \code{y.up}).
}
  \item{y.up}{
Numerical vector of y coordinates of points from which to remove upstream reaches (needs to be of the same length as argument \code{x.up}).
}
  \item{x.dn}{
Numerical vector of x coordinates of points from which to remove downstream reaches (needs to be of the same length as argument \code{y.dn}).
}
  \item{y.dn}{
Numerical vector of y coordinates of points from which to remove downstream reaches (needs to be of the same length as argument \code{x.dn}).
}
  \item{verbose}{
Logical argument to specify whether there should be (minimal) output over what has been done.
}
}

\value{
Returns pruned object of class "rivernet".
}

\seealso{
\code{\link{rivernet.read}}, \code{\link{rivernet.write}}, \cr
\code{\link{upstreamconnectivity.rivernet}}, \code{\link{adjacentreaches.rivernet}},\cr
\code{\link[utility:utility-package]{utility}}.
}


