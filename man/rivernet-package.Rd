\name{rivernet-package}

\alias{rivernet-package}
\alias{rivernet}

\docType{package}

\title{
Read, Analyse and Plot River Networks
}

\details{
\tabular{ll}{
Package: \tab rivernet\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2017-05-11\cr
License: \tab GPL (>= 2)\cr
}
}

\description{
Functions for reading, analysing and plotting river networks.\cr\cr
For this package, river networks consist of sections and nodes with associated attributes,
e.g. to characterise their morphological, chemical and biological state.
The package provides functions to read this data from text files, to analyse the network
structure and network paths and regions consisting of sections and nodes that fulfill
prescribed criteria, and to plot the river network and associated properties.\cr\cr
Important functions:\cr\cr
\code{\link{rivernet.read}}: Read river network and attribute data from text files.\cr
\code{\link{rivernet.write}}: Write river network and attribute data to text files.\cr
\code{\link{plot.rivernet}}: Plot a river network and visualize properties.\cr
\code{\link{analyze.rivernet}}: Analyse the network structure and calculate network properties.\cr
\code{\link{prune.rivernet}}: Prune a river network at given points.\cr
\code{\link{getreachind.rivernet}}: Find the reach that is closest to a given point.\cr
\code{\link{getnodeind.rivernet}}: Find the node that is closest to a given point.\cr
\code{\link{mean.rivernet}}: Calculate the length and stream order weighted mean of a property.\cr
\code{\link{upstreamconnectivity.rivernet}}: Find flow paths that fulfill given properties, e.g. that can be reached by fish from downstream.\cr
\code{\link{adjacentreaches.rivernet}}: Find regions of river sections that fulfill given properties and are (nearly) adjacent to each other.
}

\author{
Peter Reichert

Maintainer: Peter Reichert <peter.reichert@eawag.ch>
}

\keyword{ package }

\examples{
coord <- data.frame(Reach_ID=c(1,1,2,2,2,2,2,3,3,4,4),
                    X=c(5,5,5,7,8,9,10,5,0,0,2),
                    Y=c(0,2,2,4,7,6, 8,2,6,6,7),
                    Z=c(0,1,1,2,3,4, 5,1,2,2,3))
attrib.reach <- data.frame(Reach_ID=c(1,2,3,4),
                           State   =c(0,0.2,0.8,0.8),
                           Flow    =c(4,2,2,2))
attrib.node  <- data.frame(X=c(5,5,0,10,2),
                           Y=c(0,2,6, 8,7),
                           Height=c(0,0,1,0,0))
write.table(coord       ,"rivernet_example_coord.csv",sep=";",col.names=TRUE,row.names=FALSE)
write.table(attrib.reach,"rivernet_example_reach.csv",sep=";",col.names=TRUE,row.names=FALSE)
write.table(attrib.node ,"rivernet_example_node.csv" ,sep=";",col.names=TRUE,row.names=FALSE)
net <- rivernet.read("rivernet_example_coord.csv",
                     "rivernet_example_reach.csv",
                     "rivernet_example_node.csv",
                     sep=";")
plot(net,col=ifelse(net$attrib.reach$State<0.5,"red","blue"),lwd=2,
     col.nodes=ifelse(net$attrib.node$Height<0.1,"black","red"),pch=19,cex.nodes=1.5)
}


