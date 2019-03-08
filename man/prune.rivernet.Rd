\name{prune.rivernet}

\alias{prune.rivernet}

\title{
Prune a river network at specified reaches
}

\description{
Prune a river network at specified reaches.
}

\usage{
\method{prune}{rivernet}(net,reach.up=numeric(0),reach.dn=numeric(0),verbose=TRUE)
}

\arguments{
  \item{net}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{reach.up}{
Numerical vector of reach indices from which to prune all upstream reaches (the indicated reaches to prune from will be kept in the pruned network). Note that you can get reach indices from coordinates with the function \code{\link{getreachind.rivernet}}.
}
  \item{reach.dn}{
Numerical vector of reach indices from which to prune all downstream reaches and branches from downstream reaches (the indicated reaches to prune from will be kept in the pruned network). Note that you can get reach indices from coordinates with the function \code{\link{getreachind.rivernet}}.
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


