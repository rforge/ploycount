#' ELGP
#' @export ELGP
#' @import MASS
#' @export
#' @param x variable
#' @param y response var
#' @param t variable Exposure (t>>y)
#' @param h bandwith
#' @param p degree of polynomial (0,1)

ELGP <- function (x, y, t, h, p) {

  dt <- cbind(x,y,t)
  ###Sort based on x
  dt <- dt[order(x),]

  result=list(NULL)
  for (i in 1:length(h)) {

    if (p==0) {
      fit <-   ELGP.0 (x,y,t, h=h[i])
    }
    else {
      fit <-   ELGP.1 (x,y,t, h=h[i])
    }

    result[[i]] <- fit
  }
  return(result)
}