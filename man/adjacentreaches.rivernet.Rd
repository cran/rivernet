\name{adjacentreaches.rivernet}

\alias{adjacentreaches.rivernet}

\title{
Finds Regions of Adjacent Reaches
}

\description{
Finds regions of adjacent reaches that fulfill given criteria.
}

\usage{
\method{adjacentreaches}{rivernet}(x,crit.reach,thresh.length=0,...)
}

\arguments{
  \item{x}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{crit.reach}{
Logical vector of the same length as the number of reaches. Indicating whether the criterion for reaches is fulfilled.
}
  \item{thresh.length}{
Threshold length above which not fulfillment of the criterion separates reaches as not being adjacent.
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


