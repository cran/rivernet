rivernet.read <- function(file.reachcoord,
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
{
  # read river reaches:
  # ===================
  
  if ( verbose )
  {
    cat("Read data ...\n")
    flush.console()
  }
  
  # read structure file(s) (containing columns reach, x, y, (z): 
  # ------------------------------------------------------------
  
  # read coordinate data:
  
  coord <- read.csv(file.reachcoord[1],header=TRUE,sep=sep,stringsAsFactors=FALSE,...)
  if ( length(file.reachcoord) > 1 )
  {
    for ( i in 2:length(file.reachcoord) )
    {
      coord <- rbind(coord,read.table(file.reachcoord[i],header=TRUE,sep=sep,stringsAsFactors=FALSE,...))   
    }
  }
  if ( nrow(coord) < 1 )
  {
    cat("No coordinate data found","\n")
    return(NA)
  }
  
  # check presence of column names:
  # -------------------------------
  
  colnames.missing <- character(0)
  if ( is.na(colnames["reach"]) ) colnames.missing <- c(colnames.missing,"reach")
  if ( is.na(colnames["node"]) )  colnames.missing <- c(colnames.missing,"node")
  if ( is.na(colnames["x"]) )     colnames.missing <- c(colnames.missing,"x")
  if ( is.na(colnames["y"]) )     colnames.missing <- c(colnames.missing,"y") 
  if ( is.na(colnames["z"]) )     colnames.missing <- c(colnames.missing,"z") 
  if ( length(colnames.missing) > 0 )
  {
    cat("Argument \"colnames\" must contain element(s) \"",paste(colnames.missing,collapse="\", \""),"\"\n",sep="")
    return(NA)
  }
  
  # check presence of columns reach, x and y:
  # -----------------------------------------
  
  col.reach <- match(colnames["reach"],colnames(coord))
  col.x     <- match(colnames["x"],colnames(coord))
  col.y     <- match(colnames["y"],colnames(coord))
  col.z     <- match(colnames["z"],colnames(coord))
  columns.missing <- character(0)
  if ( is.na(col.reach) ) columns.missing <- c(columns.missing,colnames["reach"])
  if ( is.na(col.x) ) columns.missing <- c(columns.missing,colnames["x"])
  if ( is.na(col.y) ) columns.missing <- c(columns.missing,colnames["y"])
  if ( length(columns.missing) > 0 )
  {
    cat("File \"",file.reachcoord,"\" must contain column(s) \"",paste(columns.missing,collapse="\", \""),"\"\n",sep="")
    return(NA)
  }
  
  # ensure that reach coordinates are numeric (factors must first be converted to char)
  # and identifiers are characters and extract unique identifiers:
  # -----------------------------------------------------------------------------------
  
  if ( !is.numeric(coord[,col.x]) )
  {
    coord[,col.x] <- as.numeric(as.character(coord[,col.x]))
  }
  if ( !is.numeric(coord[,col.y]) )
  {
    coord[,col.y] <- as.numeric(as.character(coord[,col.y]))
  }
  if ( !is.na(col.z) ) 
  {
    if ( !is.numeric(coord[,col.z]) )
    {
      coord[,col.z] <- as.numeric(as.character(coord[,col.z]))
    }
  }
  coord[,col.reach] <- as.character(coord[,col.reach])
  reach.ids <- unique(coord[,col.reach])
  n.reach <- length(reach.ids)
  
  # read reach attribute data:
  # --------------------------
  
  if ( is.na(file.reachattrib[1]) )
  {
    if ( verbose )
    {
      cat("No reach attributes provided\n")
      flush.console()
    }
  }
  else
  {
    attrib.reach.read <- read.csv(file.reachattrib[1],header=TRUE,sep=sep,stringsAsFactors=FALSE,...)
    if ( length(file.reachattrib) > 1 )
    {
      for ( i in 2:length(file.reachattrib) )
      {
        attrib.tmp <- read.table(file.reachattrib[i],header=TRUE,sep=sep,stringsAsFactors=FALSE,...)
        attrib.reach.read <- merge(attrib.reach.read,attrib.tmp,by=colnames["reach"],all=TRUE,sort=FALSE)
      }
    }
    col.reach.attrib <- match(colnames["reach"],colnames(attrib.reach.read))
    if ( is.na(col.reach.attrib) )
    {
      cat("File \"",file.reachattrib,"\" must contain column \"",colnames["reach"],"\"\n",sep="")
      cat("No reach attributes read\n")
      flush.console()
      file.reachattrib[1] <- NA
    }
  }

  # read node attribute file(s) (containing columns for x, y and additional columns for attributes): 
  # ------------------------------------------------------------------------------------------------
  
  if ( is.na(file.nodeattrib[1]) )
  {
    if ( verbose )
    {
      cat("No node attributes provided\n")
      flush.console()
    }
  }
  else
  {
    attrib.node.read <- read.csv(file.nodeattrib[1],header=TRUE,sep=sep,stringsAsFactors=FALSE,...)
    if ( length(file.nodeattrib) > 1 )
    {
      for ( i in 2:length(file.nodeattrib) )
      {
        attrib.tmp <- read.table(file.nodeattrib[i],header=TRUE,sep=sep,stringsAsFactors=FALSE,...)
        attrib.node.read <- merge(attrib.node.read,attrib.tmp,by=c(colnames["x"],colnames["y"]),all=TRUE,sort=FALSE)
      }
    }
    
    # check presence of columns x and y:
    
    columns.missing <- character(0)
    col.x.node <- match(colnames["x"],colnames(attrib.node.read))
    col.y.node <- match(colnames["y"],colnames(attrib.node.read))
    if ( is.na(col.x.node) ) columns.missing <- c(columns.missing,colnames["x"])
    if ( is.na(col.y.node) ) columns.missing <- c(columns.missing,colnames["y"])
    if ( length(columns.missing) > 0 )
    {
      cat("File \"",file.nodeattrib,"\" must contain column(s) \"",paste(columns.missing,collapse="\", \""),"\"\n",sep="")
      cat("No node attributes read\n")
      flush.console()
      file.nodeattrib[1] <- NA
    }
  }
      
  # extract reach coordinates and calculate lengths:
  # ------------------------------------------------
  
  x.min   <- NA
  x.max   <- NA
  y.min   <- NA
  y.max   <- NA
  z.min   <- NA
  z.max   <- NA
  lengths <- rep(0,n.reach)
  x.start <- rep(NA,n.reach)
  x.end   <- rep(NA,n.reach)
  y.start <- rep(NA,n.reach)
  y.end   <- rep(NA,n.reach)
  z.start <- rep(NA,n.reach)
  z.end   <- rep(NA,n.reach)
  reaches <- list()
  errors  <- character(0)
  if ( verbose & n.reach > 10000 )
  {
    cat("Reading coordinates of",n.reach,"reaches ...\n")
    flush.console()
  }
  for ( i in 1:n.reach )
  {
    # initialize empty structure:
    
    reaches[[i]] <- list()
    
    # assign coordinates and update min and max:
    
    ind.reach <- coord[,col.reach] == reach.ids[i]
    n.coord   <- sum(ind.reach)
    reaches[[i]]$n <- n.coord
    reaches[[i]]$x <- coord[ind.reach,col.x]
    reaches[[i]]$y <- coord[ind.reach,col.y]
    if ( is.na(col.z) )
    {
      reaches[[i]]$z <- rep(NA,n.coord)
    }
    else
    {
      reaches[[i]]$z <- coord[ind.reach,col.z]
    }
    if ( sum(is.na(c(reaches[[i]]$x,reaches[[i]]$y))) > 0 )
    {
      errors <- c(errors,paste("x or y coordinates of reach \"",reach.ids[i],"\" contain NAs",sep=""))
    }
    x.min <- min(x.min,reaches[[i]]$x,na.rm=TRUE)
    x.max <- max(x.max,reaches[[i]]$x,na.rm=TRUE)
    y.min <- min(y.min,reaches[[i]]$y,na.rm=TRUE)
    y.max <- max(y.max,reaches[[i]]$y,na.rm=TRUE)
    if ( sum(!is.na(reaches[[i]]$z)) > 0 )
    {
      z.min <- min(z.min,reaches[[i]]$z,na.rm=TRUE)
      z.max <- max(z.max,reaches[[i]]$z,na.rm=TRUE)
    }
    
    # calculate length:
    
    length <- 0
    if ( n.coord > 1 )
    {
      length <- sum(sqrt(diff(reaches[[i]]$x)^2+diff(reaches[[i]]$y)^2))
    }
    lengths[i] <- length
    reaches[[i]]$length <- length
    
    # assign start and end coordinates (according to order from file):

    x.start[i] <- reaches[[i]]$x[1]
    y.start[i] <- reaches[[i]]$y[1]
    z.start[i] <- reaches[[i]]$z[1]
    x.end[i]   <- reaches[[i]]$x[n.coord]
    y.end[i]   <- reaches[[i]]$y[n.coord]
    z.end[i]   <- reaches[[i]]$z[n.coord]
    
    if ( verbose & 10000*floor(i/10000) == i )
    {
      cat("Coordinates of",i,"reaches read\n")
      flush.console()
    }
  }
  if (length(errors) > 0 )
  {
    cat("Problems reading reach coordinates:\n",paste(errors,collapse="\n"),"\n")
    return(NA)
  }
  names(reaches) <- reach.ids

  # identify nodes:
  # ---------------
  
  dist2 <- (x.end-x.start)^2 + (y.end-y.start)^2  # squared distances between start and end points
  ind.0 <- dist2 == 0
  if ( sum(ind.0) > 0 )
  {
    cat("*** Reach(es) has(ve) same start and end point:",paste(reach.ids[ind.0],collapse=","),"\n")
    cat("*** These reaches are omitted from the network.\n")
    n.reach <- n.reach - sum(ind.0)
    if ( n.reach == 0 )
    {
      cat("No remaining reaches")
      return(NA)
    }
    reaches   <- reaches[!ind.0]
    x.start   <- x.start[!ind.0]
    x.end     <- x.end[!ind.0]
    y.start   <- y.start[!ind.0]
    y.end     <- y.end[!ind.0]
    z.start   <- z.start[!ind.0]
    z.end     <- z.end[!ind.0]
    lengths   <- lengths[!ind.0]
    reach.ids <- reach.ids[!ind.0]
    dist2     <- dist2[!ind.0]
  }
  tol2 <- min(tol^2,0.01*min(dist2))  # tolerance^2; torerance is not larger than 10 % of minimum distance start-end
  
  if ( n.reach == 1 )
  {
    node.start <- 1
    node.end   <- 2
    n.node     <- 2
  }
  else
  {
    node.start <- rep(NA,n.reach)
    node.end   <- rep(NA,n.reach)
    n.node <- 0
    if ( verbose & n.reach > 10000 )
    {
      cat("Extracting node coordinates ...\n")
      flush.console()
    }
    for ( i in 1:n.reach )
    {
      if ( is.na(node.start[i]) )
      {
        n.node <- n.node + 1
        node.start[i] <- n.node
      }
      if ( is.na(node.end[i]) )
      {
        n.node <- n.node + 1
        node.end[i] <- n.node
      }
      if ( i < n.reach )
      {
        dist2 <- (x.start[i]-x.start[(i+1):n.reach])^2 + (y.start[i]-y.start[(i+1):n.reach])^2
        if ( sum(dist2<tol2) > 0 )
        {
          offset <- which(dist2<tol2)
          node.start[i+offset] <- node.start[i] 
        }
        dist2 <- (x.start[i]-x.end[(i+1):n.reach])^2 + (y.start[i]-y.end[(i+1):n.reach])^2
        if ( sum(dist2<tol2) > 0 )
        {
          offset <- which(dist2<tol2)
          node.end[i+offset] <- node.start[i] 
        }
        dist2 <- (x.end[i]-x.start[(i+1):n.reach])^2 + (y.end[i]-y.start[(i+1):n.reach])^2
        if ( sum(dist2<tol2) > 0 )
        {
          offset <- which(dist2<tol2)
          node.start[i+offset] <- node.end[i] 
        }
        dist2 <- (x.end[i]-x.end[(i+1):n.reach])^2 + (y.end[i]-y.end[(i+1):n.reach])^2
        if ( sum(dist2<tol2) > 0 )
        {
          offset <- which(dist2<tol2)
          node.end[i+offset] <- node.end[i] 
        }
      }
      if ( verbose & 10000*floor(i/10000) == i )
      {
        cat("Node coordinates of",i,"reaches extracted\n")
        flush.console()
      }
    }
  }

  # construct reach attributes:
  # ---------------------------
  
  attrib.reach <- data.frame(Reach      = 1:n.reach,
                             Reach_ID   = reach.ids,
                             x_start    = x.start,
                             y_start    = y.start,
                             z_start    = z.start,
                             x_end      = x.end,
                             y_end      = y.end,
                             z_end      = z.end,
                             node_start = node.start,
                             node_end   = node.end,
                             length     = lengths)
  rownames(attrib.reach) <- reach.ids

  # construct node attributes:
  # --------------------------
  
  node.ids <- 1:n.node
  x.node   <- rep(NA,n.node)
  y.node   <- rep(NA,n.node)
  nodes    <- list()
  if ( verbose & n.reach > 10000 )
  {
    cat("Constructing node attributes...\n")
    flush.console()
  }
  for ( i in 1:n.node )
  {
    x1 <- numeric(0) 
    y1 <- numeric(0)
    ind.start <- node.start == i
    if ( sum(ind.start) > 0 ) 
    {
      x1 <- x.start[ind.start]
      y1 <- y.start[ind.start]
    }
    x2 <- numeric(0) 
    y2 <- numeric(0)
    ind.end <- node.end == i
    if ( sum(ind.end) > 0 ) 
    {
      x2 <- x.end[ind.end]
      y2 <- y.end[ind.end]
    }
    x.node[i] <- mean(c(x1,x2))
    y.node[i] <- mean(c(y1,y2))
    nodes[[i]] <- list()
    nodes[[i]]$x <- x.node[i]
    nodes[[i]]$y <- y.node[i]
  }
  names(nodes) <- node.ids
  
  attrib.node <- data.frame(Node    = 1:n.node,
                            Node_ID = 1:n.node,
                            x       = x.node,
                            y       = y.node)
  rownames(attrib.node) <- node.ids
  
  # summarize input statistics:
  # ---------------------------

  if ( verbose )
  {
    cat("Number of reaches read:             ",n.reach,"\n")
    cat("Number of nodes identified:         ",n.node,"\n")
    cat("Reach lengths:                      ",min(lengths),"-",max(lengths),"\n")
    cat("Total network length:               ",sum(lengths),"\n")
    flush.console()
  }

  # merge reach attributes:
  # =======================
  
  if ( !is.na(file.reachattrib[1]) )
  {
    # check for the presence of additional data
    
    n.attrib <- ncol(attrib.reach.read)-1
    if ( n.attrib == 0 )
    {
      cat("File \"",file.reachattrib,"\" does not contain reach attributes\n",sep="")
      cat("No reach attributes read\n")
      flush.console()
    }
    else
    {
      # combine attributes read from file with those constructed above:
      
      reach.ids.file <- as.character(attrib.reach.read[,col.reach.attrib])
      attrib.reach.read <- attrib.reach.read[,-col.reach.attrib]   # remove reach identifiers from additional attrib
      attrib.new <- data.frame(matrix(NA,nrow=length(reach.ids),ncol=n.attrib))
      colnames(attrib.new) <- colnames(attrib.reach.read)
      rows.file  <- match(reach.ids,reach.ids.file)
      ind.attrib <- !is.na(rows.file)
      if ( sum(ind.attrib) > 0 )
      {
        attrib.new[ind.attrib,] <- attrib.reach.read[rows.file[ind.attrib],]          
      }
      attrib.reach <- cbind(attrib.reach,attrib.new)
      if ( verbose ) 
      {
        cat("Number of reach attributes read:    ",n.attrib,"\n")
        if ( sum(!ind.attrib) > 0 )
        {
          cat("*** No attributes found for",sum(!ind.attrib),"reach(es)\n")
        }
        flush.console()
      }
    }
  }
  
  # merge node attributes:
  # ======================
  
  # read node attribute file(s) (containing columns for x, y and additional columns for attributes): 
  
  read.nodeattrib.none     <- data.frame(index=numeric(0),x=numeric(0),y=numeric(0))
  read.nodeattrib.multiple <- list() 
  if ( !is.na(file.nodeattrib[1]) )
  {
    # ensure that node coordinates are numeric (factors must first be converted to char)
    # and identifiers are characters:
    
    if ( !is.numeric(attrib.node.read[,col.x.node]) ) attrib.node.read[,col.x.node] <- as.numeric(as.character(attrib.node.read[,col.x.node]))
    if ( !is.numeric(attrib.node.read[,col.y.node]) ) attrib.node.read[,col.y.node] <- as.numeric(as.character(attrib.node.read[,col.y.node]))
    col.node <- match(colnames["node"],colnames(attrib.node.read))
    if ( !is.na(col.node) ) attrib.node.read[,col.node] <- as.character(attrib.node.read[,col.node])
    
    # match nodes:
    
    rows <- rep(NA,n.node)
    errors <- character(0)
    for ( i in 1:n.node )
    {
      ind <- which( (x.node[i]-attrib.node.read[,col.x.node])^2 + (y.node[i]-attrib.node.read[,col.y.node])^2 < tol2 )
      if ( length(ind) == 1 )
      {
        rows[i] <- ind
      }
      else
      {
        if ( length(ind) > 1 )
        {
          indlist <- length(read.nodeattrib.multiple) + 1
          dist <- sqrt( (x.node[i]-attrib.node.read[ind,col.x.node])^2 + (y.node[i]-attrib.node.read[ind,col.y.node])^2 )
          read.nodeattrib.multiple[[indlist]] <- list(x       = x.node[i],
                                                      y       = y.node[i],
                                                      Node_ID = names(nodes)[i],
                                                      data    = data.frame(x       = attrib.node.read[ind,col.x.node],
                                                                           y       = attrib.node.read[ind,col.y.node],
                                                                           Node_ID = rep(NA,length(ind)),
                                                                           dist    = dist))
          if ( !is.na(col.node) ) read.nodeattrib.multiple[[indlist]]$data$Node_ID = attrib.node.read[ind,col.node]
          rows[i] <- ind[1]
        }
      }
    }
    if (length(errors) > 0 )
    {
      cat("Problems reading node attributes:\n",paste(errors,collapse="\n"),"\n",sep="")
      flush.console()
    }
    
    if ( sum(!is.na(rows)) == 0 )
    {
      if ( verbose )
      {
        cat("No node attributes found\n")
        flush.console()
      }
    }
    else
    {    
      n.attrib <- ncol(attrib.node.read)-2
      rows.noattrib <- which(is.na(rows))
      if ( length(rows.noattrib) > 0 )
      {
        dist  <- rep(NA,length(rows.noattrib) )
        index <- rep(NA,length(rows.noattrib) )
        id    <- rep(NA,length(rows.noattrib) )
        for ( i in 1:length(rows.noattrib) )
        {
          for ( j in 1:nrow(attrib.node.read) )
          {
            d <- sqrt ( (x.node[rows.noattrib[i]]-attrib.node.read[j,col.x.node])^2 + (y.node[rows.noattrib[i]]-attrib.node.read[j,col.y.node])^2 )
            if ( is.na(dist[i]) | d<dist[i] )
            {
              dist[i]  <- d
              index[i] <- j
              if ( !is.na(col.node) ) id[i] <- attrib.node.read[j,col.node]
            }
          }
        }
        read.nodeattrib.none <- data.frame(index=rows.noattrib,Node_ID=names(nodes)[rows.noattrib],
                                           closest_ind=index,closest_ID=id,closest_dist=dist)
      }
      attrib.node.read <- rbind(attrib.node.read,rep(NA,ncol(attrib.node.read)))
      rows <- ifelse(is.na(rows),nrow(attrib.node.read),rows)
      if ( is.na(col.node) )
      {
        attrib.node <- cbind(attrib.node,attrib.node.read[rows,])
      }
      else
      {
        attrib.node <- cbind(attrib.node,attrib.node.read[rows,-col.node])
        attrib.node$Node_ID <- attrib.node.read[rows,col.node]
        names(nodes) <- attrib.node.read[rows,col.node]
      }
      if ( verbose ) 
      {
        cat("Number of node attributes read:     ",n.attrib,"\n")
        if ( nrow(read.nodeattrib.none) > 0 )
        {
          cat("*** No attributes found for",nrow(read.nodeattrib.none),"node(s)\n")
          cat("    (see [value]$read.nodeattrib.none for more details)\n")
        }
        if ( length(read.nodeattrib.multiple) > 0)
        {
          cat("*** Multiple attributes found for",length(read.nodeattrib.multiple),"node(s)\n")
          cat("    (see [value]$read.nodeattrib.multiple for more details)\n")
        }
        flush.console()
      }
    }
  }
  
  # construct rivernet object:
  # ==========================
  
  rivernet <- list()
  rivernet$reaches                  <- reaches
  rivernet$nodes                    <- nodes
  rivernet$xlim                     <- c(x.min,x.max)
  rivernet$ylim                     <- c(y.min,y.max)
  rivernet$zlim                     <- c(z.min,z.max)
  rivernet$htow                     <- (y.max-y.min)/(x.max-x.min)
  rivernet$total.length             <- sum(lengths)
  rivernet$colnames                 <- colnames
  rivernet$attrib.reach             <- attrib.reach
  rivernet$attrib.node              <- attrib.node
  rivernet$read.nodeattrib.none     <- read.nodeattrib.none
  rivernet$read.nodeattrib.multiple <- read.nodeattrib.multiple 
  class(rivernet) <- "rivernet"
  
  # analyze network:
  # ================
  
  if ( analyze ) rivernet <- analyze(rivernet,verbose=verbose)
  
  return(rivernet)
}

