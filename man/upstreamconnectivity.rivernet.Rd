\name{upstreamconnectivity.rivernet}

\alias{upstreamconnectivity.rivernet}

\title{
Calculates connectivity from outlet to upstream reaches
}

\description{
Calculates connectivity from the outlet to upstream reaches, in particular to stream order 1 reaches. Criteria can be provided for reaches as well as nodes.
}

\usage{
\method{upstreamconnectivity}{rivernet}(x,crit.reach,crit.node,thresh.length=0,...)
}

\arguments{
  \item{x}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{crit.reach}{
Logical vector of the same length as the number of reaches. Indicates whether the reach can be counted as connecting between adjacent reaches.
}
  \item{crit.node}{
Logical vector of the same length as the number of (internal) nodes of the river network. Indicates wheter the node can be counted as connecting between adjacent reaches (e.g. small drop height that allows for fish migration).
}
  \item{thresh.length}{
Threshold lenght below which a reach (or a sequence of reaches) can be accepted as connecting even if their criterion \code{crit.reach} is not fulfilled.
}
  \item{...}{
.
}
}

\value{
List with the following entries:\cr
\code{paths.reachable}: List of numeric vectors specifying the indices of the reaches of a reachable path. Note that this is a subset of the reaches given in the same component of the element \code{paths} of the river network.\cr
\code{firstorder.reachable}: Vector of logicals indicating if the corresponding paths given above reach first order rivers.
\code{fract.firstorder.reachable}: fraction of first order segments that can be reached (under the conditions specified by \code{crit.reach}, \code{crit.node} and \code{thresh.length}).\cr
\code{streamorder.reachable}: vector of reachable rivers of order 1, 2, etc.
}

\seealso{
\code{\link{rivernet.read}}, \code{\link{mean.rivernet}}, \code{\link{adjacentreaches.rivernet}},\cr 
\code{\link[utility:utility-package]{utility}}.
}


