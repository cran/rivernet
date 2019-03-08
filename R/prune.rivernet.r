prune <- function(net,reach.up=numeric(0),reach.dn=numeric(0),verbose=TRUE) UseMethod("prune")

prune.rivernet <- function(net,reach.up=numeric(0),reach.dn=numeric(0),verbose=TRUE)
{
  # check input:
  
  n.reach <- length(net$reaches)
  if ( length(reach.dn) > 0 )
  {
    ind.illegal <- which(reach.dn < 1 | reach.dn > n.reach)
    if ( length(ind.illegal) > 0 )
    {
      cat("*** reach(es)",paste(reach.dn[ind.illegal],collapse=","),"to prune at do(es) not exist\n")
      reach.dn <- reach.dn[-ind.illegal]
    }
  }
  if ( length(reach.up) > 0 )
  {
    ind.illegal <- which(reach.up < 1 | reach.up > n.reach)
    if ( length(ind.illegal) > 0 )
    {
      cat("*** reach(es)",paste(reach.up[ind.illegal],collapse=","),"to prune at do(es) not exist\n")
      reach.up <- reach.up[-ind.illegal]
    }
  }
  if ( length(reach.dn)==0 & length(reach.up)==0 )
  {
    cat("*** no reaches found to prune from\n")
    return(net)
  }
  
  # get network structure:
  
  if ( length(net$paths) == 0 ) net <- analyze(net,verbose=verbose)        # analyze network structure
  if ( length(reach.dn) > 0 )   net$attrib.reach$outlet[reach.dn] <- TRUE  # define new outlets

  # pruning network:

  remove.reach.up <- rep(FALSE,length(net$reaches))
  remove.reach.dn <- rep(FALSE,length(net$reaches))
  if ( length(reach.dn) > 0 ) 
  {
    subnets <- unique(net$attrib.reach$subnet[reach.dn])
    remove.reach.dn[net$attrib.reach$subnet %in% subnets] <- TRUE
  }
  for ( i in 1:length(net$paths) )
  {
    if ( length(reach.up) > 0 )
    {
      inds <- match(reach.up,net$paths[[i]])
      if ( sum(!is.na(inds)) > 0 )
      {
        ind <- max(inds,na.rm=TRUE)
        if ( ind > 1 ) remove.reach.up[net$paths[[i]][1:(ind-1)]] <- TRUE  # keep prune reach
      }
    }
    if ( length(reach.dn) > 0 )
    {
      inds <- match(reach.dn,net$paths[[i]])
      if ( sum(!is.na(inds)) > 0 )
      {
        ind <- max(inds,na.rm=TRUE)
        remove.reach.dn[net$paths[[i]][1:ind]] <- FALSE  # keep prune reach
      }
    }
  }
  remove.node <- rep(FALSE,length(net$nodes))
  for ( i in 1:length(net$reaches) )
  {
    if ( remove.reach.up[i] )
    {
      remove.node[net$reaches[[i]]$from_node] <- TRUE
    }
    if ( remove.reach.dn[i] )
    {
      remove.node[net$reaches[[i]]$to_node] <- TRUE
      if ( is.na(match(i,reach.dn)) )  # remove upstream nodes in side branches
      {
        remove.node[net$reaches[[i]]$from_node] <- TRUE
        if ( length(reach.dn) > 0 )   # keep downstream node of reach to keep
        {
          for ( r in reach.dn )
          {
            remove.node[net$reaches[[r]]$to_node] <- FALSE
          }
        }
      }
    }
  }
  remove.reach <- remove.reach.up | remove.reach.dn
  if ( verbose )
  {
    cat("Deleting",sum(remove.reach),"of",length(net$reaches),"reaches \n")
    cat("Deleting",sum(remove.node),"of",length(net$nodes),"nodes\n")
  }
  if ( sum(!remove.reach) == 0 ) return(NA)
  nodeind          <- (1:length(net$nodes))[!remove.node]
  net$reaches      <- net$reaches[!remove.reach]
  net$nodes        <- net$nodes[!remove.node]
  net$attrib.reach <- net$attrib.reach[!remove.reach,]
  net$attrib.node  <- net$attrib.node[!remove.node,]
  # correct indices of start and end node of each reach:
  net$attrib.reach[,"node_start"] <- match(net$attrib.reach[,"node_start"],nodeind)
  net$attrib.reach[,"node_end"]   <- match(net$attrib.reach[,"node_end"],nodeind)
  
  # updating entries:
    
  net$total.length <- sum(net$attrib.reach[,"length"])
  x.min   <- NA
  x.max   <- NA
  y.min   <- NA
  y.max   <- NA
  z.min   <- NA
  z.max   <- NA
  for ( i in 1:length(net$reaches) )
  {
    x.min <- min(x.min,net$reaches[[i]]$x,na.rm=TRUE)
    y.min <- min(y.min,net$reaches[[i]]$y,na.rm=TRUE)
    z.min <- min(z.min,net$reaches[[i]]$z,na.rm=TRUE)
    x.max <- max(x.max,net$reaches[[i]]$x,na.rm=TRUE)
    y.max <- max(y.max,net$reaches[[i]]$y,na.rm=TRUE)
    z.max <- max(z.max,net$reaches[[i]]$z,na.rm=TRUE)
  }
  net$xlim <- c(x.min,x.max)
  net$ylim <- c(y.min,y.max)
  net$zlim <- c(z.min,z.max)
  net$htow <- (y.max-y.min)/(x.max-x.min)
  
  # reanalyzing network structure:
  
  net <- analyze(net,outlet.reach=which(net$attrib.reach$outlet),calc.streamorder=FALSE,verbose=verbose)
  
  return(net)
}
  