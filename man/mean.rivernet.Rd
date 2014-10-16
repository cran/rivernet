\name{mean.rivernet}

\alias{mean.rivernet}

\title{
Calculates the length and stream order - weighted average of a given reach property
}

\description{
Calculates the length and stream order - weighted average of a given reach property.
}

\usage{
\method{mean}{rivernet}(x,y=NA,na.rm=FALSE,...)
}

\arguments{
  \item{x}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{y}{
Numerical vector of properties of reaches to be averaged. If no values are provided, the function returns the average length of the reaches.
}
  \item{na.rm}{
Indication whether NAs should be removed.
}
  \item{...}{
.
}
}

\value{
A scalar representing the weighted mean.
}

\seealso{
\code{\link{rivernet.read}}, \code{\link{upstreamconnectivity.rivernet}}, \code{\link{adjacentreaches.rivernet}},\cr
\code{\link[utility:utility-package]{utility}}.
}


