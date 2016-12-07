ELGP.1 <- function(x,y,t,h) #p=1 (lokal linier)
{
  x0 <-NULL
  sx0 <-NULL
  mux0<-NULL
  rate <-NULL
  invpar<-NULL
  mpar<-NULL
  vkon<-rep(0,length(x))
  invpar<-matrix(0,nrow=3,ncol=length(x)+1)
  invpar[,1]<- c(0.1,0.1,1)
  for (i in 2:(length(x)+1))
    { j <- i-1
    x0 <- x[j]
    u <- (x-x0)
    K <- as.vector(Kgaus(u/h)/h)
    likelihood<- function(param)
    { b0<- param[1]
      b1<- param[2]
      p<- param[3]
      m<- as.vector(t*exp(b0+b1*u))
      value <- -sum((y*log(m/(1+p*m))+(y-1)*log(1+p*y)-m*(1+p*y)/(1+p*m)-
                lfactorial(y))*K)
    }
    parameter<- nlminb(start=invpar[,j],likelihood,lower=c(-Inf,-Inf,0),
                   upper=c(Inf,Inf,Inf))
    vpar <- parameter$par
    invpar[,i]<- vpar
    vkon[j] <- parameter$convergence
  }
  mpar=invpar[,-1]
  vkon
  sx0=as.vector(mpar[1,])
  s1x0=as.vector(mpar[2,])
  rate=exp(sx0)
  mux0=t*rate
  mse =sum((y-mux0)^2)/length(x)
  result=list(matrikspar=mpar,vektorkonvergensi=vkon,sx0=sx0,s1x0=s1x0,
            ratex0=rate, mux0=mux0,mse=mse,h=h)
  return(result)
}

