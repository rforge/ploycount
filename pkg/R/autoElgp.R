#' AUTO-ELGP
#' @details autoElgp
#' @param x variable
#' @param y response variable
#' @param t variable exposure (t>>y)
#' @param min minimum bandwidth
#' @param max maximum bandwidth
#' @param s increment for bandwidth searching
#' @param p degree of polynomial
#' @export
autoELGP <- function (x, y, t, p, min=NULL, max=NULL, s=NULL) {

  dt <- cbind(x,y,t)
  ## Sort based on x
  dt <- dt[order(x),]

  if (is.null(min)) {
    min=1
    max=5
    s=0.01
  }

  rescv <- Makcv (x,y,t,min, max,s, p=p)
  fit <- ELGP (x, y, t, h=rescv , p=p)
  fit
}
