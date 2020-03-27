#EXERCISE 2
library(ggplot2)

#generator function
generator <- function(n,lambda){
  S <- as.data.frame(rep(0,n))
  names(S)[1] <- "sample"

  for(i in 1:n){
    P <- 1; N <- 0
    while (1) {
      U <- runif(1)
      P <- P*U
      N <- N+1
      if(P<exp(-lambda)){
        S$sample[i] <- N-1
        break
      }
    }
  }
return(S)
}

#plots and chi sq test
Allplots <- function(Plots){
  comparison <- as.data.frame(table(Plots))
  comparison$Plots <- as.numeric(comparison$Plots)
  comparison$Expectedf <- dpois(comparison$Plots,lambda)*sum(comparison$Freq)
    
  ggobj <- ggplot(comparison) + 
  geom_point(aes(Plots,Freq)) +
  ggtitle("Poisson")+
  labs(x = "N")
  print(ggobj)
  print(chisq.test(comparison$Freq,comparison$Expectedf)$p.value)
}


set.seed(3110)
#lambda poisson
lambda <- c(2,5,10)
#number of samples
n <- c(10,100,200,1000,100000)

AllSamples <- sapply(n,generator,lambda[1])
sapply(AllSamples,Allplots)

AllSamples <- sapply(n,generator,lambda[2])
sapply(AllSamples,Allplots)

AllSamples <- sapply(n,generator,lambda[3])
sapply(AllSamples,Allplots)


#plots Poisson with different lambda
grid <- 0:25
DfPoisson <- as.data.frame(grid)
DfPoisson$two <- dpois(lambda[1],grid)
DfPoisson$five <- dpois(lambda[2],grid)
DfPoisson$ten <- dpois(lambda[3],grid)

#Poisson lambda 2
ggplot(DfPoisson,aes(grid,two))  + geom_point(col = "brown")+ ggtitle("Poisson lambda = 2") + 
  labs(x= "x",y= "y") + geom_line()
#Poisson lambda 5
ggplot(DfPoisson,aes(grid,five))  + geom_point(col = "brown")+ ggtitle("Poisson lambda = 5") + 
  labs(x= "x",y= "y") + geom_line()
#Poisson lambda 10
ggplot(DfPoisson,aes(grid,ten))  + geom_point(col = "brown")+ ggtitle("Poisson lambda = 10") + 
  labs(x= "x",y= "y") + geom_line()


