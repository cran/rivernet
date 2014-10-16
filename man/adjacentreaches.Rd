\name{adjacentreaches}

\alias{adjacentreaches}

\title{
Finds regions of adjacent reaches
}

\description{
Finds regions of adjacent reaches.
}

\usage{
adjacentreaches(x,...)
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
Numerical vector of the same length as the number of reaches in the river network with unique numbering corresponding to regions of adjacent reaches (under the condition specified by \code{crit.reach} and \code{thresh.length}). The segment labelled 0 corresponds to reaches that do not fulfill the criteria. The other regions are labelled with 1, 2, 3, ...
}

\seealso{
\code{\link{rivernet.read}}, \code{\link{mean.rivernet}}, \code{\link{upstreamconnectivity.rivernet}},\cr 
\code{\link[utility:utility-package]{utility}}.
}


