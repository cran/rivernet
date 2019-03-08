getregionconnections <- function(net,regions)
{
  # consistency checks:
  
  if ( length(net$paths) == 0 ) return(NA)                                      # network analysis was not done
  if ( length(regions) != length(net$reaches) ) return(NA)                      # rivernet does not match regions
  if ( sum(sort(unique(regions))[-1] == 1:max(regions)) != max(regions) ) return(NA) # inconsistent region vector
  
  # initialization:
  
  connections <- list()   # list over regions
  
  # loop over regions to get downstream and upstream paths:
  
  for ( region in 1:max(regions) )
  {
    connections[[region]] <- list()                     # list for each region
    connections[[region]]$downstream.path <- numeric(0) # maximum one downstream path
    connections[[region]]$upstream.paths  <- list()     # potentially multiple upstream paths
    
    reaches.region <- which(regions==region)   # get indices of reaches of the current region
    
    k <- 1
    for ( i in 1:length(net$paths) )   # collect downstream and upstream paths of the current region
    {
      # indices in path[[i]] that match the region (paths is indexed downstream):
      ind.region <- which(!is.na(match(net$paths[[i]],reaches.region)))
      
      # search for paths only if the current path intersects the region:
      if ( length(ind.region) > 0 )
      {
        # get downstream path:
        
        if ( max(ind.region) < length(net$paths[[i]]) )  # potentially multiple time the same path
        {
          connections[[region]]$downstream.path <- net$paths[[i]][(max(ind.region)+1):length(net$paths[[i]])]
        }
        
        # get upstream paths:
        
        if( min(ind.region) > 1 )
        {
          connections[[region]]$upstream.paths[[k]] <- net$paths[[i]][1:(min(ind.region)-1)]
          k <- k+1
        }
      }
    }
  }
  
  # another loop over regions to get downstream, upstream and down-upstream reaches:
  
  for ( region in 1:max(regions) )
  {
    # get downstream region and path:
    
    connections[[region]]$downstream.region <- list()
    if ( length(connections[[region]]$downstream.path) > 0 )
    {
      ind.reach <- match(TRUE,regions[connections[[region]]$downstream.path] != 0)
      if ( !is.na(ind.reach) )
      {
        connections[[region]]$downstream.region$region <- regions[connections[[region]]$downstream.path][ind.reach]
        connections[[region]]$downstream.region$path   <- connections[[region]]$downstream.path[1:(ind.reach-1)]
        connections[[region]]$downstream.region$dist   <- sum(net$attrib.reach$length[connections[[region]]$downstream.region$path])
      }
    }
    
    # get upstream regions and paths:
   
    connections[[region]]$upstream.regions <- list()
    if ( length(connections[[region]]$upstream.paths) > 0 )
    {
      k <- 1
      r <- numeric(0)
      for ( i in 1:length(connections[[region]]$upstream.paths) )
      {
        ind.reach <- match(TRUE,rev(regions[connections[[region]]$upstream.paths[[i]]]) != 0)
        if ( !is.na(ind.reach) )
        {
          reg <- rev(regions[connections[[region]]$upstream.paths[[i]]])[ind.reach]
          if ( is.na(match(reg,r)) )
          {
            connections[[region]]$upstream.regions[[k]] <- list()
            connections[[region]]$upstream.regions[[k]]$region <- reg
            connections[[region]]$upstream.regions[[k]]$path   <- rev(rev(connections[[region]]$upstream.paths[[i]])[1:(ind.reach-1)])
            connections[[region]]$upstream.regions[[k]]$dist   <- sum(net$attrib.reach$length[connections[[region]]$upstream.regions[[k]]$path])
            k <- k+1
            r <- c(r,reg)
          }
        }
      }
    }
    
    # get down- and upstream regions and paths:
    
    connections[[region]]$downupstream.regions <- list()
    dspath1 <- connections[[region]]$downstream.path
    if ( length(dspath1) > 0 )
    {
      k <- 1
      for ( region2 in 1:max(regions))
      {
        if ( region2 != region)
        {
          dspath2 <- connections[[region2]]$downstream.path
          if ( length(dspath2) > 0 )
          {
            ind1 <- match(dspath2,dspath1)
            ind2 <- match(dspath1,dspath2)
            ind1 <- ind1[!is.na(ind1)]
            ind2 <- ind2[!is.na(ind2)]
            if ( length(ind1) > 0 ) dspath1cut <- dspath1[-ind1]
            if ( length(ind2) > 0 ) dspath2cut <- dspath2[-ind2]
            if ( sum(regions[dspath1cut]) == 0 & sum(regions[dspath2cut]) == 0 )  # no regions in between
            {
              connections[[region]]$downupstream.regions[[k]]                 <- list()
              connections[[region]]$downupstream.regions[[k]]$region          <- region2
              connections[[region]]$downupstream.regions[[k]]$downstream.path <- dspath1cut
              connections[[region]]$downupstream.regions[[k]]$upstream.path   <- dspath2cut
              connections[[region]]$downupstream.regions[[k]]$dist <- sum(net$attrib.reach$length[dspath1]) +
                                                                      sum(net$attrib.reach$length[dspath2])
              k <- k+1
            }
          }
        }
      }
    }
  }
  
  return(connections)
}
