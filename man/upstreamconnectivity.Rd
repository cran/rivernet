\name{upstreamconnectivity}

\alias{upstreamconnectivity}

\title{
Calculates connectivity from outlet to upstream reaches
}

\description{
Calculates connectivity from the outlet to upstream reaches, in particular to stream order 1 reaches. Criteria can be provided for reaches as well as nodes.
}

\usage{
upstreamconnectivity(x,...)
}

\arguments{
  \item{x}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{...}{
.
}
}

\value{
List with the following entries:\cr
\code{paths.reachable}: List of numeric vectors specifying the indices of the reaches of a reachable path. Note that this is a subset of the reaches given in the same component of the element \code{pahts} of the river network.\cr
\code{firstorder.reachable}: Vector of logicals indicating if the corresponding paths given above reach first order rivers.
\code{fract.firstorder.reachable}: fraction of first order segments that can be reached (under the conditions specified by \code{crit.reach}, \code{crit.node} and \code{thresh.length}).\cr
\code{streamorder.reachable}: vector of reachable rivers of order 1, 2, etc.
}

\seealso{
\code{\link{rivernet.read}}, \code{\link{mean.rivernet}}, \code{\link{adjacentreaches.rivernet}},\cr 
\code{\link[utility:utility-package]{utility}}.
}


