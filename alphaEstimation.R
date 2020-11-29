A=read.table("C:\\Users\\alireza\\Desktop\\Fallahi.txt")
nc=108
nr=602
M=matrix(0,ncol=nc,nrow=nr)
      for(i in 1:nc){
      M[,i]=A[,i]
    }
Hillalpha<-function(x)
         {
             ordered <- rev(sort(x))
             ordered <- ordered[ordered[] > 0]
             n <- length(ordered)
             loggs <- log(ordered)
             hill <- cumsum(loggs[1:(n - 1)])/(1:(n - 1)) - loggs[2:n]
             hill <- 1/(hill)
             plot(1:length(hill), hill, type = "l",
             xlab = "number of order statistics",
             ylab = "Hill estimate of alpha", main="Hill plot")
              return(hill[601])
                 }


 HE1=rep(0,0)
      for(i in 1:nc){
         x=M[,i]
HE1[i]<-Hillalpha(x)
}
mean(na.omit(HE1))



require(graphics)
y <- c(M)
qqnorm(y)

