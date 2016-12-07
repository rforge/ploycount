Kgaus<-function(x)
{ gauss<-(1/sqrt(2*3.142857))*exp(-(x^2)/2)
  return(gauss)
}

ELGP.0 <-function(x,y,t,h) {

  x0 <- NULL
  sx0 <- NULL
  mux0 <- NULL
  rate <- NULL
  invpar <- NULL
  mpar <- NULL
  vkon <-rep(0,length(x))
  invpar <-matrix(0,nrow=2,ncol=length(x)+1)

  ## Set initial value for parameter b0 and p ##
  invpar[,1]<- c(0.1,1)
  for (i in 2:(length(x)+1))
      {
        j <- i-1
        x0 <- x[j]
        u <- (x-x0)
        ## Kernel/penimbang p
        K <- as.vector(Kgaus(u/h)/h)

        likelihood <- function(param)
            {
            ## param: initial value ##
            b0 <- param[1]
            p <- param[2]
            m <- as.vector(t*exp(b0))
            value <- -sum((y*log(m/(1+p*m))+(y-1)*log(1+p*y)-m*(1+p*y)/(1+p*m)-
                 lfactorial(y))*K)

            }
        parameter<- nlminb(start=invpar[,j],likelihood,lower= c(-Inf,0), upper=c(Inf,1))

        ## best parameter
        vpar <- parameter$par
        invpar[,i] <- vpar

        ## Check for convergence
        vkon[j] <- parameter$convergence
  }

    mpar=invpar[,-1]
    sx0 <- as.vector(mpar[1,])
    s1x0 <- as.vector(mpar[2,])
    rate <- exp(sx0)
    mux0 <- t*rate
    mse <- sum((y-mux0)^2)/length(x)
    result <- list(matrikspar=mpar,vektorkonvergensi=vkon,sx0=sx0,s1x0=s1x0,
            ratex0=rate,mux0=mux0,mse=mse, h=h)
result
}



