\name{adjacentreaches.rivernet}

\alias{adjacentreaches.rivernet}

\title{
Finds Regions of Adjacent Reaches
}

\description{
Finds regions of adjacent reaches that fulfill given criteria.
}

\usage{
\method{adjacentreaches}{rivernet}(x,crit.reach,crit.node=TRUE,thresh.length=0,...)
}

\arguments{
  \item{x}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{crit.reach}{
Logical vector of the same length as the number of reaches. Indicating whether the criterion for reaches is fulfilled. A single value of \code{TRUE} indicates the criterion is fulfilled for all reaches.
}
  \item{crit.node}{
Logical vector of the same length as the number of nodes. Indicating whether the criterion for nodes is fulfilled. A single value of \code{TRUE} indicates the criterion is fulfilled for all nodes.
}
  \item{thresh.length}{
Threshold length above which not fulfillment of the criterion separates reaches as not being adjacent.
}
  \item{...}{
.
}
}

\value{
Numerical vector of the same length as the number of reaches in the river network with unique numbering corresponding to regions of adjacent reaches (under the conditions specified by \code{crit.reach}, \code{crit.node} and \code{thresh.length}). The segment labelled 0 corresponds to reaches which do not fulfill the criteria and are further away than \code{thresh.length} from reaches that fulfill the criteria or which are longer than \code{thresh.length}. The other regions are labelled with 1, 2, 3, ...
}

\seealso{
\code{\link{rivernet.read}}, \code{\link{mean.rivernet}}, \code{\link{upstreamconnectivity.rivernet}},\cr 
\code{\link[utility:utility-package]{utility}}.
}


