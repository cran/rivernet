\name{splitreach.rivernet}

\alias{splitreach.rivernet}

\title{
Split a reach into two sub-reaches
}

\description{
Splits a reach at a given part of its length into two subreaches
}

\usage{
\method{splitreach}{rivernet}(net,reachind,fract,...)
}

\arguments{
  \item{net}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{reachind}{
Index of a reach in the rivernet object.
}
  \item{fract}{
Fraction of the length at which the reach should be split. 
Needs to be between 0 and 1.
The fraction is counted from the logical start point of the reach.
}
  \item{...}{
.
}
}

\value{
The complete rivernet with the splitted reach.
}

\seealso{
\code{\link{rivernet.read}}, \code{\link{getnodeind.rivernet}}, \code{\link{upstreamconnectivity.rivernet}}, \code{\link{adjacentreaches.rivernet}},\cr
\code{\link[utility:utility-package]{utility}}.
}
