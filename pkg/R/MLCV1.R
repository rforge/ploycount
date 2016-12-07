# Fungsi untuk menghtung MLCV p=1 untuk h tertentu
MLCV.1 <-function(x,y,t,h)
{

  n<-length(x)
  sx0	<-NULL
  phi	<-NULL
  mui	<-NULL
  invpar<-matrix(0,nrow=3,ncol=length(x)+1)
  invpar[,1]<- c(-7,0.1,1)
  data	    <-cbind(x,y,t)
  for (i in 2:(length(x)+1))
  {
    j	<- i-1
    x0	<- x[j]
    datab <- data[-j,]
    xb<-datab[,1]
    yb<-datab[,2]
    tb<-datab[,3]
    u   	<- (xb-x0)
    K	<- as.vector(Kgaus(u/h)/h)
    likelihood	<- function(param)
    {
      b0	<- param[1]
      b1	<- param[2]
      p	<- param[3]
      m	<- tb*exp(b0+b1*u)
      value	<- -sum((yb*log(m/(1+p*m))+(yb-1)*log(1+p*yb)-m*(1+p*yb)/(1+p*m)-lfactorial(yb))*K)
    }


    parameter	<- nlminb(start=invpar[,j],likelihood,lower=c(-Inf,-Inf,0),upper=c(Inf,Inf,1))
    # parameter
    vpar		<- parameter$par
    phii		<- vpar[3]
    invpar[,i]	<- vpar
    phi		<-c(phi,phii)
    muii		<- t[j]*exp(vpar[1])
    mui		<-c(mui,muii)

  }
  phi
  mui
  mlcv <-sum(y*log(mui/(1+phi*mui))+(y-1)*log(1+phi*y)-mui*(1+phi*y)/(1+phi*mui)-lfactorial(y))
  mlcv
}

