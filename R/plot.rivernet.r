plot.rivernet <- function(x,margin=0,
                          main=NA,cex.main=1,pos="topleft",
                          col=NA,lwd=1,
                          pch.nodes=NA,cex.nodes=0.2,col.nodes="black",
                          ...)
{
   rivernet <- x
   w <- rivernet$xlim[2]-rivernet$xlim[1]
   h <- rivernet$ylim[2]-rivernet$ylim[1]
   plot(numeric(0),numeric(0),xaxt="n",yaxt="n",xlab="",ylab="",type="n",
        xlim=rivernet$xlim+margin*w*c(-1,1),
        ylim=rivernet$ylim+margin*h*c(-1,1))
   if ( !is.na(main) ) 
   {
     if ( pos == "topleft" ) text(rivernet$xlim[1]+0.01*w,rivernet$ylim[2]-0.01*h,main,pos=4,cex=cex.main)
     else                    text(rivernet$xlim[2]-0.01*w,rivernet$ylim[2]-0.01*h,main,pos=2,cex=cex.main)
   } 
   if ( length(col) == 1 ) col <- rep(col,length(rivernet$reaches))
   if ( length(names(col)) > 0 ) col <- col[names(rivernet$reaches)]
   col <- ifelse(is.na(col),"black",col)
   if ( length(lwd) == 1 ) lwd <- rep(lwd,length(rivernet$reaches))
   if ( length(names(lwd)) > 0 ) lwd <- lwd[names(rivernet$reaches)]
   if ( !is.na(pch.nodes) > 0 )
   {
     points(rivernet$attrib.node$x,rivernet$attrib.node$y,
            pch=pch.nodes,cex=cex.nodes,col=col.nodes)
   }
   for ( i in 1:length(rivernet$reaches) )
   {
      lines(rivernet$reaches[[i]],col=col[i],lwd=lwd[i],lend=2,...)
   }
}
