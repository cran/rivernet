\name{plot.rivernet}

\alias{plot.rivernet}

\title{
Plots a River Network Stored in an Object of Class "rivernet"
}

\description{
Plots a river network.
}

\usage{
\method{plot}{rivernet}(x,margin=0,
                        main=NA,cex.main=1,pos="topleft",
                        col=NA,lwd=1,
                        pch.nodes=NA,cex.nodes=0.2,col.nodes="black",
                        ...)
}

\arguments{
  \item{x}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{margin}{
Relative margin size.
}
  \item{main}{
Optional title of the plot.
}
  \item{cex.main}{
Font scaling factor of the title.
}
  \item{pos}{
Position of legend. Either "topleft" or "topright".
}
  \item{col}{
Optional single color or vector of colors of the different reaches either in the same order as the reaches in the rivernet object or labeled by the reach identifiers.
}
  \item{lwd}{
Optional single line width or vector of line widths of the different reaches either in the same order as the reaches in the rivernet object or labeled by the reach identifiers.
}
  \item{pch.nodes}{
Plot marker for nodes. See \code{\link{points}} for an explanation of codes.
}
  \item{cex.nodes}{
Scaling of markers for nodes.
}
  \item{col.nodes}{
Single color or vector of colors used for node markers. The order must be the same as in the rivernet object.
}
  \item{...}{
Further arguments are passed to the plotting function.
}
}

\seealso{
\code{\link{rivernet.read}},
\code{\link[utility:utility-package]{utility}}.
}


