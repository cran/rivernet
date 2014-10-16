\name{analyze.rivernet}

\alias{analyze.rivernet}

\title{
Analyzes the Network Structure of a River Network
}

\description{
Analyzes the network structure of a river network stored as an object of type \code{rivernet}.
}

\usage{
\method{analyze}{rivernet}(net,
                           verbose          = TRUE,
                           ...)
}

\arguments{
  \item{net}{
Object of type \code{rivernet} usually read by \code{\link{rivernet.read}}.
}
  \item{verbose}{
Logical variable to turn on and off summary output about the network structure read.
}
  \item{...}{
Optional further arguments.
}
}

\value{
The function returns an object of type \code{rivernet} that adds the following columns to the
data frame \code{attrib.reach} of the object:
\code{subnet}: index of sub-network,\cr
\code{n_start}: number of reaches connected at the start end of the reach,\cr
\code{n_end}: number of reaches connected at the end end of the reach,\cr
\code{endreach}: logical variable indicating if the reach is only connected at one end,\cr
\code{outlet}: logical variable indicating if the reach represents the outlet of the network,\cr
\code{headwater}: logical variable indicating if the reach is a headwater,\cr
\code{downstream}: logical variable indicating if the reach coordinates represent the downstream direction,\cr
\code{reach_down}: index of reach downstream of the reach,\cr
\code{streamorder}: stream order of the reach.\cr\cr
In addition, a list of all paths from the headwaters to the outlet is provided;\cr
\code{paths}: list of vectors of reach indices of the paths from all sources to the outlet.
}

\seealso{
\code{\link{rivernet.read}},\cr
\code{\link{mean.rivernet}}, \code{\link{upstreamconnectivity.rivernet}}, \code{\link{adjacentreaches.rivernet}},\cr 
\code{\link[utility:utility-package]{utility}}.
}


