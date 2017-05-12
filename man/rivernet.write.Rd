\name{rivernet.write}

\alias{rivernet.write}

\title{
Write a river network to three data files
}

\description{
Writes a river network to three data files: coordinates of river reaches, attributes of reaches, and attributes of nodes.
}

\usage{
rivernet.write(x,
               file.reachcoord  = NA,
               file.reachattrib = NA,
               file.nodeattrib  = NA,
               sep              = "\t",
               subnets          = NA)
}

\arguments{
  \item{x}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{file.reachcoord}{
File name for writing reach coordinates.
}
  \item{file.reachattrib}{
File name for writing reach attributes.
}
  \item{file.nodeattrib}{
File name for writing node attributes.
}
  \item{sep}{
Separator on data files.
}
  \item{subnets}{
Optional vector of sub-networks to write (with the default NA all sub-networks are written..
}
}

\value{
No return value.
}

\seealso{
\code{\link{rivernet.read}}, \cr
\code{\link{prune.rivernet}}, \cr
\code{\link{upstreamconnectivity.rivernet}}, \code{\link{adjacentreaches.rivernet}},\cr
\code{\link[utility:utility-package]{utility}}.
}


