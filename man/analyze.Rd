\name{analyze}

\alias{analyze}

\title{
Analyzes the Network Structure of a River Network
}

\description{
Analyzes the network structure of a river network stored as an object of type \code{rivernet}.
}

\usage{
analyze(net,verbose=TRUE,...)
}

\arguments{
  \item{net}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{verbose}{
Logical variable to turn on and off summary output about the network structure read.
}
  \item{...}{
Optional further arguments.
}
}

\value{
List with the following entries:\cr
\code{paths.reachable}: List of numeric vectors specifying the indices of the reaches of a reachable path. Note that this is a subset of the reaches given in the same component of the element \code{pahts} of the river network.\cr
\code{firstorder.reachable}: Vector of logicals indicating if the corresponding paths given above reach first order rivers.
\code{fract.firstorder.reachable}: fraction of first order segments that can be reached (under the conditions specified by \code{crit.reach}, \code{crit.node} and \code{thresh.length}).
}

\seealso{
\code{\link{rivernet.read}}, \code{\link{mean.rivernet}}, \code{\link{adjacentreaches.rivernet}},\cr 
\code{\link[utility:utility-package]{utility}}.
}


