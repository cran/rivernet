\name{getnodeind}

\alias{getnodeind}

\title{
Gets indices of nodes that are closest to given locations
}

\description{
Gets indices of nodes that are closest to given locations
}

\usage{
getnodeind(net,x,y,...)
}

\arguments{
  \item{net}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{x}{
Numerical vector of x coordinates of sites to be analyzed (needs to be of the same length as argument \code{y}).
}
  \item{y}{
Numerical vector of y coordinates of sites to be analyzed (needs to be of the same length as argument \code{x}).
}
  \item{...}{
.
}
}

\value{
A data frame with the indices in the first and the distances in the second column.
}

\seealso{
\code{\link{rivernet.read}}, \code{\link{getreachind.rivernet}}, \code{\link{upstreamconnectivity.rivernet}}, \code{\link{adjacentreaches.rivernet}},\cr
\code{\link[utility:utility-package]{utility}}.
}
