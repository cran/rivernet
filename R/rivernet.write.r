rivernet.write <- function(x,
                           file.reachcoord  = NA,
                           file.reachattrib = NA,
                           file.nodeattrib  = NA,
                           sep              ="\t",
                           subnets          = NA)
{
  rivernet <- x
  
  # get reach and node indices to write:
  
  reach.ind <- 1:length(rivernet$reaches)
  if ( !is.na(subnets[1]) )
  {
    reach.ind <- numeric(0)
    if ( !is.na(match("subnet",names(rivernet$attrib.reach))) )
    {
      for ( i in subnets )
      {
        ind <- rivernet$attrib.reach$subnet == i
        if ( sum(ind) > 0 ) reach.ind <- c(reach.ind,which(ind))
      }
    }
  }
  if ( length(reach.ind) == 0 )
  {
    cat("*** subnet(s) not found\n")
    return()
  }
  node.ind <- sort(unique(c(rivernet$attrib.reach$node_start[reach.ind],
                            rivernet$attrib.reach$node_end[reach.ind])))
  
  # write coordinates of reaches:
  
  if ( !is.na(file.reachcoord) )
  {
    data <- data.frame(matrix(ncol=4,nrow=0))
    for ( i in reach.ind )
    {
      data <- rbind(data,data.frame(id = rep(names(rivernet$reaches)[i],rivernet$reaches[[i]]$n),
                                    x  = rivernet$reaches[[i]]$x,
                                    y  = rivernet$reaches[[i]]$y,
                                    z  = rivernet$reaches[[i]]$z))
    }
    colnames(data) <- c(rivernet$colnames["reach"],
                        rivernet$colnames["x"],
                        rivernet$colnames["y"],
                        rivernet$colnames["z"])
    write.table(data,file.reachcoord,col.names=TRUE,row.names=FALSE,sep=sep)
  }
  
  # write reach attributes:
  
  if ( !is.na(file.reachattrib) )
  {
    attrib <- rivernet$attrib.reach[reach.ind,]
    colnames(attrib)[1:2] <- c("Reach",rivernet$colnames["reach"])
    write.table(attrib,file.reachattrib,col.names=TRUE,row.names=FALSE,sep=sep)
  }
  
  # write node attributes:
  
  if ( !is.na(file.nodeattrib) )
  {
    attrib <- rivernet$attrib.node[node.ind,]
    colnames(attrib)[1:4] <- c("Node",rivernet$colnames["reach"],rivernet$colnames["x"],rivernet$colnames["y"])
    write.table(attrib,file.nodeattrib,col.names=TRUE,row.names=FALSE,sep=sep)
  }
}
  