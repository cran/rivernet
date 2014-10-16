\name{rivernet.read}

\alias{rivernet.read}

\title{
Reads Geographical Information of River Network
}

\description{
Reads a river network and attributes from text files.
}

\usage{
rivernet.read(file.reachcoord,
              file.reachattrib = NA,
              file.nodeattrib  = NA,
              colnames         = c(reach = "Reach_ID",
                                   node  = "Node_ID",
                                   x     = "X",
                                   y     = "Y",
                                   z     = "Z"),
              sep              ="\t",
              tol              = 1,
              analyze          = FALSE,
              verbose          = TRUE,
              ...)
}

\arguments{
  \item{file.reachcoord}{
Name of text file or vector of names that contain(s) columns with reach id, x, y, and z coordinates.
An arbitrary number of rows per reach is possible to allow for a reasonable geographical resolution of the river reach, but the rows corresponding to the same reach id are interpreted as a sequential series of points between which the river is constructed by linear interpolation.
}
  \item{file.reachattrib}{
Name of text file or vector of names that contain(s) a column with the same reach reach ids as used in the file \code{file.reachcorrd} and an arbitrary number of attributes of the reaches.
}
  \item{file.nodeattrib}{
Name of text file or vector of names that contain(s) columns with x and y coordinates of the node (must be identical with start or end coordinats of the connecting reaches) and an arbitrary number of attributes of the nodes.
}
  \item{colnames}{
Labelled vector containing strings to identify the headers of the colums for reach and node identifiers and x, y and z coordinates along the river reaches.
}
  \item{sep}{
Column separator used for the files.
}
  \item{tol}{
Spatial tolerance for identifying nodes.
}
  \item{analyze}{
Logical variable to indicate whether the function \code{\link{analyze.rivernet}} should be called after reading.
}
  \item{verbose}{
Logical variable to turn on and off summary output about the network structure read.
}
  \item{...}{
Optional further arguments are passed to read.table.
}
}

\value{
The function returns an object of type "rivernet" that contains the geographical representation of the river.
This object contains the following elements\cr\cr
\code{reaches}: List of river reaches with the following elements: \cr
\code{n}: number of coordinates,\cr
\code{x}: vector of x-coordinates,\cr
\code{y}: vector of y-coordinates,\cr
\code{z}: vector of z-coordinates,\cr
\code{length}: length of the reach),\cr\cr
\code{xlim}: range of river network in x direction.\cr\cr
\code{ylim}: range of river network in y direction.\cr\cr
\code{zlim}: vertical range of river network (z direction).\cr\cr
\code{htow}: ratio of y to x extension; to be used for the height to widht ratio of network plots.\cr\cr
\code{total.length}: sum of length of all river reaches in the network.\cr\cr
\code{attrib.reach}: data frame of reaches with columns\cr
\code{Reach_ID}: reach identifier,\cr
\code{Reach}: reach index,\cr
\code{x_start}: start x-coordinate of reach,\cr
\code{y_start}: start y-coordinate of reach,\cr
\code{z_start}: start elevation of reach,\cr
\code{x_end}: end x-coordinate of reach,\cr
\code{y_end}: end y-coordinate of reach,\cr
\code{z_end}: end elevation of reach,\cr
\code{node_start}: index of node at the start of the reach,\cr
\code{node_end}: index of node at the end of the reach,\cr
\code{length}: length of reach.\cr
If the argument \code{analyze} is true, this data frame contains the additional columns:
\code{subnet}: index of sub-network,\cr
\code{n_start}: number of reaches connected at the start end of the reach,\cr
\code{n_end}: number of reaches connected at the end end of the reach,\cr
\code{endreach}: logical variable indicating if the reach is only connected at one end,\cr
\code{outlet}: logical variable indicating if the reach represents the outlet of the network,\cr
\code{headwater}: logical variable indicating if the reach is a headwater,\cr
\code{downstream}: logical variable indicating if the reach coordinates represent the downstream direction,\cr
\code{reach_down}: index of reach downstream of the reach,\cr
\code{streamorder}: stream order of the reach.\cr
If a file \code{file.reachattrib} was provided, its columns are added to the colums of this data frame.\cr\cr
\code{attrib.node}: data frame of nodes with columns \cr
\code{node}: node index,\cr
\code{x}: x-coordinate of the node,\cr
\code{y}: y-coordinate of the node.\cr\cr
\code{paths}: list of vectors of reach indices of the paths from all headwaters to the outlet.
}

\seealso{
\code{\link{analyze.rivernet}}\cr
\code{\link{plot.rivernet}}\cr
\code{\link{getreachind.rivernet}}\cr
\code{\link{mean.rivernet}}\cr
\code{\link{upstreamconnectivity.rivernet}}\cr
\code{\link{adjacentreaches.rivernet}}.
}


