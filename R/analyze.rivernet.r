analyze <- function(net,outlet.reach=NA,calc.streamorder=TRUE,verbose=TRUE) UseMethod("analyze")

analyze.rivernet <- function(net,outlet.reach=NA,calc.streamorder=TRUE,verbose=TRUE)
{
  # definitions:
  # ------------
  
  n.reach    <- nrow(net$attrib.reach)
  n.node     <- nrow(net$attrib.node)
  node.start <- net$attrib.reach$node_start
  node.end   <- net$attrib.reach$node_end
  z.start    <- net$attrib.reach$z_start
  z.end      <- net$attrib.reach$z_end
  
  # identify subnets:
  # -----------------
  
  subnet <- rep(NA,n.reach)
  n.subnet <- 0
  for ( i in 1:n.reach)
  {
    if ( is.na(subnet[i]) )
    {
      n.subnet <- n.subnet + 1
      reaches <- i
      
print(paste("i =",i))
      while ( length(reaches) > 0 )
      {
        subnet[reaches] <- n.subnet
        nodes <- unique(c(net$attrib.reach$node_start[reaches],net$attrib.reach$node_end[reaches]))
        reaches <- which( !is.na(match(net$attrib.reach$node_start,nodes)) |
                          !is.na(match(net$attrib.reach$node_end,nodes)) )
        reaches <- reaches[is.na(subnet[reaches])]  # only consider new reaches
      }
    }
  }
  net$attrib.reach$subnet <- subnet
  if ( verbose )
  {
    cat("Number of sub-nets:                 ",n.subnet,"\n")
  }

  # identify end nodes and end reaches:
  # -----------------------------------
  
  n.start <- rep(NA,n.reach)
  n.end   <- rep(NA,n.reach)
  nodes <- c(net$attrib.reach$node_start,net$attrib.reach$node_end)
  for ( i in 1:n.reach )
  {
    n.start[i] <- sum(nodes==net$attrib.reach$node_start[i]) - 1
    n.end[i]   <- sum(nodes==net$attrib.reach$node_end[i])   - 1
  }
  endreach <- n.start==0 | n.end==0
  net$attrib.reach$n_start  <- n.start
  net$attrib.reach$n_end    <- n.end
  net$attrib.reach$endreach <- endreach
  if ( verbose )
  {
    cat("Number of end reaches:              ",sum(endreach),"\n")
    cat("Number of internal reaches:         ",n.reach-sum(endreach),"\n")
  }

  # find outlets:
  # -------------
  
  outlet <- rep(FALSE,n.reach)
  for ( s in 1:n.subnet )
  {
    i.out <- 0
    i.endreach <- which(subnet == s & endreach)  # outlet candidates within subnet
    if ( length(i.endreach) == 0 )
    {
      cat("No potential outlet found")
      if ( n.subnet > 1 ) cat(" for subnet",subnet)
      cat("(circular connections?)\n")
    }
    else
    {
      ind <- which(outlet.reach %in% i.endreach)
      if ( length(ind) != 0 )
      {
        if ( length(ind) > 1 )
        {
          cat("Multiple outlets specified")
          if ( n.subnet > 1 ) cat(" for subnet",subnet)
          cat("\n")
          outlet[ind] <- NA
        }
        else
        {
          i.out <- outlet.reach[ind]
          outlet[i.out] <- TRUE
        }
      }
      else  # search for potential outlet
      {
        if ( any(!is.na(outlet.reach)) )
        {
          cat("Prescribed outlet not identified as an end reach")
          if ( n.subnet > 1 ) cat(" in subnet ",subnet)
          cat("; trying to identify other outlet\n")
        }
        if ( length(n.end[i.endreach]==0) == 1 )  # unique reach with no end connection is assumed to be outlet 
        {                                         # (river coordinates downstream)
          i.out <- which(n.end[i.endreach]==0)
          outlet[i.out] <- TRUE
        }
        else
        {
          if ( length(n.start[i.endreach]==0) == 1 ) # unique reach with no start connection is assumed to be outlet 
          {                                          # only if no elevation information is present
            i.out <- which(n.start[i.endreach]==0)
            if ( is.na(z.start[i.out]) )
            {
              outlet[i.out] <- TRUE
            }
            else
            {
              i.out <- 0
            }
          }
        }
        if ( i.out == 0 ) # if not uniquely identified, choose end with lowest elevation
        {                 # (this obviously requires elevation information)
          ind.start <- i.endreach[n.start[i.endreach]==0]
          ind.end   <- i.endreach[n.end[i.endreach]==0]
          if ( sum(is.na(c(z.start[ind.start],z.end[ind.end]))) > 0 )
          {
            cat("*** Unable to dermine outlet")
            if ( n.subnet > 1 ) cat(" for subnet",s)
            cat(" (provide elevation information or use downstream coordinate order) ***\n")
            outlet[i.endreach] <- NA
          }
          else
          {
            if ( min(z.end[ind.end]) < min(z.start[ind.start]) )
            {
              outlet[ind.end[which.min(z.end[ind.end])]] <- TRUE
            }
            else
            {
              outlet[ind.start[which.min(z.start[ind.start])]] <- TRUE
            }
          }
        }
      }
    }
  }
  headwater <- endreach & !outlet
  net$attrib.reach$outlet    <- outlet
  net$attrib.reach$headwater <- headwater
  
  if ( sum(is.na(outlet)) > 0 | n.node != n.reach + n.subnet )
  {
    cat("*** Unable to complete network analysis (circular connections?) ***\n")
    return(net)
  }
  
  # identify downstream direction (by moving upstream from outlet):
  # ---------------------------------------------------------------
  
  downstream <- rep(NA,n.reach)
  reach.down <- rep(NA,n.reach)
  for ( s in 1:n.subnet )
  {
    ind.subnet <- subnet == s
    ind.outlet <- which(ind.subnet & outlet)
    if ( length(ind.outlet) != 1 )
    {
      cat("*** Unable to complete network analysis (circular connections?) ***\n")
      return(net)
    }
    if ( n.end[ind.outlet] == 0 ) downstream[ind.outlet] <- TRUE
    else                          downstream[ind.outlet] <- FALSE
    to.visit    <- ind.outlet
    counter <- 0
    while ( length(to.visit) > 0 )
    {
      ind.current <- to.visit[1]
      to.visit <- to.visit[-1]
      if ( downstream[ind.current] )
      {
        upstream.node <- node.start[ind.current]
      }
      else
      {
        upstream.node <- node.end[ind.current]
      }
      upstream.reaches <- which(node.start==upstream.node) # connected by start
      if ( length(upstream.reaches) > 0 )
      {
        for ( r in upstream.reaches )
        {
          if ( r != ind.current )
          {
            downstream[r] <- FALSE
            to.visit <- c(to.visit,r)
            reach.down[r] <- ind.current
          }
        }
      }
      upstream.reaches <- which(node.end==upstream.node) # connected by end
      if ( length(upstream.reaches) > 0 )
      {
        for ( r in upstream.reaches )
        {
          if ( r != ind.current )
          {
            downstream[r] <- TRUE
            to.visit <- c(to.visit,r)
            reach.down[r] <- ind.current
          }
        }
      }
      counter <- counter + 1
      if ( counter > n.reach )
      {
        cat("*** Unable to complete network analysis (circular connections?) ***\n")
        return(net)
      }
    }    
  }
  if ( sum(is.na(downstream)) > 0 )
  {
    cat("*** Unable to complete network analysis (circular connections?) ***\n")
    return(net)
  }
  net$attrib.reach$downstream <- downstream  
  net$attrib.reach$reach_down <- reach.down
  
  # establish linking:
  # ------------------
  
  for ( i in 1:n.node ) 
  { 
    net$nodes[[i]]$from_reach <- numeric(0)
    net$nodes[[i]]$to_reach   <- numeric(0)
  }
  for ( i in 1:n.reach )
  {
    if ( downstream[i] )
    {
      net$reaches[[i]]$from_node <- node.start[i]
      net$reaches[[i]]$to_node   <- node.end[i]
      net$nodes[[node.start[i]]]$to_reach <- c(net$nodes[[node.start[i]]]$to_reach,i)
      net$nodes[[node.end[i]]]$from_reach <- c(net$nodes[[node.end[i]]]$from_reach,i)
    }
    else
    {
      net$reaches[[i]]$from_node <- node.end[i]
      net$reaches[[i]]$to_node   <- node.start[i]
      net$nodes[[node.end[i]]]$to_reach     <- c(net$nodes[[node.end[i]]]$to_reach,i)
      net$nodes[[node.start[i]]]$from_reach <- c(net$nodes[[node.start[i]]]$from_reach,i)
    }
  }
      
  # find paths from headwater to outlet:
  # ------------------------------------
    
  paths <- list()
  ind <- which(headwater)
  for ( i in 1:length(ind) )
  {
    paths[[i]] <- ind[i]
    ind.current <- ind[i]
    counter <- 0
    while ( TRUE )
    {
      ind.current <- reach.down[ind.current]
      if ( is.na(ind.current) ) break
      paths[[i]] <- c(paths[[i]],ind.current)
      counter <- counter + 1
      if ( counter > n.reach )
      {
        cat("*** Problems identifying network paths ***")
        return(net)
      }
    }
  }
  net$paths <- paths
  
  # determine stream order:
  # -----------------------
  
  if ( calc.streamorder )
  {
    streamorder <- rep(NA,n.reach)
    ind <- which(headwater)
    streamorder[ind] <- 1            # orders = 1 at headwaters
    for ( i in 1:length(ind) )
    {
      for ( j in 1:n.reach ) # loop for moving downstream; will be terminated earlier
      {
        if( is.na(reach.down[ind[i]]) )
        {
          break
        }
        else
        {
          cur.order <- streamorder[ind[i]]        
          if ( downstream[ind[i]] )
          {
            downstream.node <- node.end[ind[i]]
          }
          else
          {
            downstream.node <- node.start[ind[i]]
          }        
          downstream.or.joining <- c(which(node.start==downstream.node),which(node.end==downstream.node))
          downstream.or.joining <- downstream.or.joining[-match(ind[i],downstream.or.joining)]
          if ( length(downstream.or.joining) == 1 ) # no junction, proceed with same order
          {
            ind[i] <- reach.down[ind[i]]
            streamorder[ind[i]] <- cur.order
          }
          else # junction, increase order if other order is the same or larger
          {
            max.order <- 0
            for ( k in 1:length(downstream.or.joining) )
            {
              if ( downstream.or.joining[k] != reach.down[ind[i]] )
              {
                k.order <- streamorder[downstream.or.joining[k]]
                max.order <- max(max.order,k.order)
              }
            }
            if ( is.na(max.order) ) break
            ind[i] <- reach.down[ind[i]]
            if ( cur.order == max.order ) cur.order <- cur.order + 1
            else                          cur.order <- max(cur.order,max.order)
            streamorder[ind[i]] <- cur.order
          }
        }
      }
    }
    net$attrib.reach$streamorder <- streamorder  
    if ( verbose )
    {
      cat("Maximum stream order:               ",max(streamorder),"\n")
    }
  }
  
  return(net)
}
  
  
  