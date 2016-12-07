#' Plot ELGP
#' @details Plotting elgp result
#' @param x variable
#' @param y respons variale
#' @param t exposyre variable
#' @param addlegend boolean for add legend on graphics
#' @param add boolean for adding other plot
#' @param ... other graphical parameter
#' @param elgpres elgp result
#' @export
plotElgp<- function(x, y, t, elgpres, addlegend=F, add=F, ...)
{
  rt <- log(y/t)
  if (add ==F)
   plot(x,rt, pch=16, ...)
  h <- NULL
  for (i in 1:length(elgpres)) {
    ypred <- elgpres[[i]]$sx0
    lines(x,ypred, lwd=2,col=i+1)
    h[i] <- elgpres[[i]]$h
  }
  if (addlegend==T)
  legend("bottomright", legend=as.character(h), col=h+1, lty=1,
                           title = "Bandwidth", cex=0.8)

}
