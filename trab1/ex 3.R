#EXERCISE 3
##########                Method1       ####################
#function we are working on f(x) = max(0, 1 - |x|)
fx <- function(x){
  k <- 1 - abs(x)
  return(ifelse(k>0,k,0))
}

#cumulative function of f(x)
CummF <- function(x){
  condition1 <- x<0 & x> -1
  condition2 <- x>=0 & x<1 
  condition3 <- x>= 1
  condition4 <- x<= -1
  x <- ifelse(condition1,x^2/2 + x + 1/2,x)
  x <- ifelse(condition2,-x^2/2 + x + 1/2,x)
  x <- ifelse(condition3,1,x)
  x <- ifelse(condition4,0,x)
  return(x)
}

#inverse of cumulative function 
InverseF <- function(x){
  condition1 <- x<1/2 & x>= 0
  condition2 <- x>= 1/2 & x<=1 
  x <- ifelse(condition1, -1 + sqrt(2*x),x)
  x <- ifelse(condition2,1 - sqrt(2*(1-x)),x)
  return(x)
} 

#generate the samples
generator <- function(n){
  sample <- rep(0,n)
  for (i in 1:n) {
    U <- runif(1)
    sample[i] <- InverseF(U)
  }
  return(sample)
}

set.seed(3110)
n <- c(10,100,200,1000,100000)
AllSamples <- sapply(n,generator)

plotSamples <- function(x){
  x <- as.data.frame(x)
  names(x)[1] <- "x"
  print(ggplot() + geom_density(data = x,aes(x)))
  print(ks.test(x,CummF)$p.value)
}

sapply(AllSamples,plotSamples)

#Plots from cumulative function and f(x)
grid <- seq(-2,2,length = 100)
df <- as.data.frame(grid)
df$f <- fx(grid)
df$CumF <- CummF(grid)
ggplot(df,aes(grid,f)) + geom_line() + ggtitle("triangular density") + labs(y= "f(x)", x = "x")
ggplot(df,aes(grid,CumF)) + geom_line() + ggtitle("Cumulative function") + labs(y= "F(x)", x = "x")

##########                Method2       ####################

#Auxiliary function Cauchy, density
dCauchy <- function(x){
  return(1/(pi*(1+x*x)))
}

#Inverse transform of cumulative function of Cauchy
IFCauchy <- function(x){
  return(tan(pi*(x+3/2)))
}

#Take the sample
sampleCauchy <- function(){
  k<- runif(1)
  return(IFCauchy(k))  
}

#f(x) function
fx <- function(x){
  k <- 1 - abs(x)
  return(ifelse(k>0,k,0))
}

#This methods need to compute M, which is max(f(x)/g(x))
#Function f(x)/g(x)
fgx <- function(x){
  return(fx(x)/dCauchy(x))
}

grid <- seq(-4,4,length = 1000)
M <- max(fgx(grid))

#plot f(x) and Mg(x)
df <- as.data.frame(grid)
df$f <- fx(grid)
df$Mgx <- M*dCauchy(grid)
ggplot(df) + geom_line(aes(grid,f)) + geom_line(aes(grid,Mgx),col="brown",linetype = 2)

#take the sample
generator2 <- function(n){
  sample <- rep(0,n)
  for(i in 1:n){
    while(1){
      y <- sampleCauchy()
      u <- runif(1)
      if(u<=fgx(y)/M){
        sample[i] <- y
        break
      }
    }
  }
  return(sample)
}

plotSamples2 <- function(x){
  x <- as.data.frame(x) 
  names(x)[1] <- "samples"
  print(ggplot(x)+
          geom_density(aes(samples)))
  print(ks.test(x,CummF)$p.value)
}

n <- c(10,100,200,1000,100000)
set.seed(3110)
AllSamples2 <- sapply(n,generator2) 
sapply(AllSamples2,plotSamples2)

ggplot(df) + geom_density(aes(sample)) + geom_line(aes(grid,f),col = "brown",linetype=2)




