#Program menghitung nilai MLCV untuk vektor h
Makcv.0 <- function (x,y,t,min,max,s)
{
  h	<- seq(min,max,s)
  mcv   <-NULL
  for (j in 1:length(h))
  {
    mcvj	<- MLCV.0(x,y,t,h[j])
    mcv	<- c(mcv,mcvj)
    cat("=")
  }
  mcv
  result=cbind(h,mcv)
  result
}

