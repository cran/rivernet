\name{getregionconnections}

\alias{getregionconnections}

\title{
Gets indices of reaches that are closest to given locations
}

\description{
Gets indices of reaches that are closest to given locations
}

\usage{
getregionconnections(net,regions)
}

\arguments{
  \item{net}{
Object of class "rivernet" e.g. read by the function \code{\link{rivernet.read}}.
}
  \item{regions}{
Numerical vector of the length of the number of regions containing the region coding as calculated e.g. by the function \code{\link{adjacentreaches.rivernet}}:
Reaches not belonging to a region should be encoded by the value of zero; reaches belonging to regions should be coded with the index of the region, a natural number from 1 to the number of reaches.
}
}

\value{
A list with an entry for each region. The entry for each region is again a list with the folowing entries (some may be empty):\cr
\code{downstream.path}: A single vector of indices of reaches describing the path downstream of the given region to the outlet of the river network irrespective of potential other regions to be crossed. Note that the vector will be of length zero if the given region extends to the outlet reach.\cr
\code{upstream.paths}: A list of numeric vectors of the paths from all upstream headwaters to the given region irrespective of potential other regions to be crossed. Note that the list will be of length zero if the given region includes all upstream headwaters.\cr
\code{downstream.region}: If there on the path downstream to the outlet, this will be an empty list. If there is a downstream region, this is a list with the following elements:
\code{region}: index of downstream region;
\code{path}: vector of reach indices of the path from the given region to the downstream region;
\code{dist}: length of the path to the downstream region.\cr
\code{upstream.regions}: Empty list if there is no upstream region to the given region. Otherwise list of upstream regions with list entries as in \code{downstream.region} for each upstream regtion.\cr
\code{downupstream.regions}: List of regions that can be reached from the given region by first moving downstream and then upstream without crossing another region. If there are no such regions, this is an empty list. Otherwise this is a list of such regions and for each of these regions again a list with the following entries: 
\code{region}: index of (down-upstream) region;
\code{downstream.path}: vector of reach indices of the downstream path from the given region to the junction from which the region can be reached upstream;
\code{upstream.path}: vector of reach indices of the path from thejunction where the downstream path ends to the region;
\code{dist}: length of the path between the regions.\cr
}

\seealso{
\code{\link{rivernet.read}}, \code{\link{adjacentreaches.rivernet}},\cr
\code{\link[utility:utility-package]{utility}}.
}
