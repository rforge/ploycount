#' MAKCV
#' @details MAKCV: find best bandwith via cross validation
#' @export
#' @param x variable
#' @param y response variable
#' @param t exposure variable (t>>y)
#' @param min minimum bandwidth
#' @param max maximum bandwidth
#' @param s step bandwidth range
#' @param p degree of polynomial (0,1)

Makcv <- function (x, y, t, min=1, max=5, s=1, p=0) {
  dt <- cbind(x,y,t)
  ## Sort based on x
  dt <- dt[order(x),]

  if (p==0) {
    fit <-   Makcv.0(x,y,t,min, max, s)
  }
  else {
    fit <-   Makcv.1(x,y,t,min, max, s)
  }

  fit
}
