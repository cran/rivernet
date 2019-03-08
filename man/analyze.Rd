\name{analyze}

\alias{analyze}

\title{
Analyzes the Network Structure of a River Network
}

\description{
Analyzes the network structure of a river network stored as an object of type \code{rivernet}.
}

\usage{
analyze(net,outlet.reach=NA,calc.streamorder=TRUE,verbose=TRUE)
}

\arguments{
  \item{net}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{outlet.reach}{
Index or, in case of multiple subnets index vector, of outlet reach or outlet reaches.
If not provided, the function identifies the outlet reach either by assuming coordinates are provided in downstream direction or from elevation information.
}
  \item{calc.streamorder}{
Logical variable to indicate whether streamorder should be calculated
(after pruning top reaches (see \code{\link{prune.rivernet}}) it may be unwanted to calculate stream order by setting (new) top reaches to order one).
}
  \item{verbose}{
Logical variable to turn on and off summary output about the network structure read.
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
In addition, the list of reaches (\code{reaches}) and the list of nodes (\code{nodes}) are appended by indices
\code{from_node}, \code{to_node} and \code{from_reach}, \code{to_reach}, respectively.
Finally, a list of all paths from the headwaters to the outlet is provided;\cr
\code{paths}: list of vectors of reach indices of the paths from all sources to the outlet.
}

\seealso{
\code{\link{rivernet.read}}, \code{\link{mean.rivernet}}, \code{\link{adjacentreaches.rivernet}},\cr 
\code{\link[utility:utility-package]{utility}}.
}


