adjacentreaches <- function(x, ...) UseMethod("adjacentreaches")


adjacentreaches.rivernet <- function(x,crit.reach,crit.node=TRUE,thresh.length=0,...)
{
  
  # function to extend a region; to be applied iteratively:
  
  extend.region <- function(ind,crit.reach,crit.node,thresh.length,dist,
                            regions,attrib.reach)
  {
    if ( crit.reach[ind] ) d <- 0
    else                   d <- dist + attrib.reach$length[ind]
    if ( d > thresh.length ) return(regions)
    
    r <- numeric(0)
    if ( crit.node[attrib.reach$node_start[ind]] )
    {
      r <- c(r,which(attrib.reach$node_start==attrib.reach$node_start[ind] |
                     attrib.reach$node_end  ==attrib.reach$node_start[ind]))
    }
    if ( crit.node[attrib.reach$node_end[ind]] )
    {
      r <- c(r,which(attrib.reach$node_start==attrib.reach$node_end[ind] |
                     attrib.reach$node_end  ==attrib.reach$node_end[ind]))
    }
    if ( length(r) > 0 )
    {
      for ( i in r )
      {
        if ( is.na(regions[i]) )
        {
          if ( crit.reach[i] | d+attrib.reach$length[i] < thresh.length )
          {
            regions[i] <- regions[ind]
            regions <- extend.region(i,crit.reach,crit.node,thresh.length,d,
                                     regions,attrib.reach)
          }
        }
      }
    }

    return(regions)
  }
  
  # initial checks:
  
  if ( anyNA(crit.reach) )
  {
    cat("*** variable \"crit.reach\" contains NA ***\n")
    return(NA)
  }
  if ( anyNA(crit.node) )
  {
    cat("*** variable \"crit.node\" contains NA ***\n")
    return(NA)
  }

  rivernet <- x
  n.reach <- length(rivernet$reaches)
  n.node  <- length(rivernet$nodes)
  if ( length(crit.reach) != n.reach ) 
  {
    if ( length(crit.reach) != 1 )
    {
      cat("*** length of reach criteria (\"crit.reach\") is not equal to number of reaches\n")
      return(NA)
    }
    crit.reach <- rep(crit.reach,n.reach)
  }
  if ( length(crit.node) != n.node )
  {
    if ( length(crit.node) != 1 )
    {
      cat("*** length of node criteria (\"crit.node\") is not equal to number of nodes\n")
      return(NA)
    }
    crit.node <- rep(crit.node,n.node)
  }
  
  # main section to determine regions:

  regions <- rep(NA,n.reach)
  region <- 0
  while ( sum( is.na(regions) & crit.reach ) > 0 )
  {
    ind <- which(is.na(regions) & crit.reach)[1]
    region <- region + 1
    regions[ind] <- region
    regions <- extend.region(ind,crit.reach,crit.node,thresh.length,0,
                             regions,rivernet$attrib.reach)
  }
  if ( sum(is.na(regions)) > 0 ) regions[is.na(regions)] <- 0
  
  # remove non-internal reaches that do not fulfill the criterion:
  
  n.regions <- max(regions)
  if ( n.regions > 0 )
  {
    for ( reg in 1:n.regions )
    {
      r <- which(regions==reg & !crit.reach)   # reaches in region reg that do not fulfill the criterion
      if ( length(r) > 0 )
      {
        # determine adjacent reaches:
        r.adj <- list()
        for ( j in 1:length(r) )
        {
          r.adj[[j]] <- list()
          r.adj[[j]]$start <- which(rivernet$attrib.reach$node_start==rivernet$attrib.reach$node_start[r[j]] |
                                    rivernet$attrib.reach$node_end  ==rivernet$attrib.reach$node_start[r[j]] )
          r.adj[[j]]$end   <- which(rivernet$attrib.reach$node_start==rivernet$attrib.reach$node_end[r[j]] |
                                    rivernet$attrib.reach$node_end  ==rivernet$attrib.reach$node_end[r[j]])
        }
        while ( TRUE )
        {
          found <- FALSE
          for ( j in 1:length(r) )
          {
            if ( regions[r[j]] == reg )
            {
              if ( sum(regions[r.adj[[j]]$start] == reg) < 2 | sum(regions[r.adj[[j]]$end] == reg) < 2 )  # the region itself counts as adjacent too
              {
                regions[r[j]] <- 0
                found <- TRUE
              }
            }
          }
          if ( !found ) break
        }
      }
    }
  }

  return(regions)
}



