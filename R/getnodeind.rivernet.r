getnodeind <- function(net,x,y, ...) UseMethod("getnodeind")


getnodeind.rivernet <- function(net,x,y,...)
{
  n <- length(x)
  if ( n != length(y) ) return(NA)
  if ( n == 0 )         return(NA)
  
  ind  <- rep(NA,n)
  dist <- rep(NA,n)
  for ( i in 1:n )
  {
    for ( j in 1:length(net$nodes) )
    {
      dist.new <- sqrt( (x[i]-net$nodes[[j]]$x)^2 + (y[i]-net$nodes[[j]]$y)^2 )
      if ( !is.na(dist.new) )
      {
        if ( is.na(dist[i]) )
        {
          dist[i] <- dist.new
          ind[i]  <- j
        }  
        else
        {
          if ( dist.new < dist[i] )
          {
            dist[i] <- dist.new
            ind[i]  <- j
          }
        }
      }
    }
  }
  return(data.frame(node.ind=ind,dist=dist))
}
