#EXERCISE 1
library(ggplot2)

generator <- function(n){
  Sample <- rep(0,n)
  for(k in 1:n){
    U <- runif(12)
    Sample[k] <- sum(U) - 6
  }
  return(Sample)
}

plotSample <- function(x){
  df <- as.data.frame(x)
  names(df)[1] <- "x"
  print(ggplot(df,aes(x)) + geom_density())
  print(ks.test(x,"pnorm",0,1)$p.value)  
}

n <- c(10,100,200,1000,100000)
set.seed(3110)
AllSamples <- sapply(n, generator)
sapply(AllSamples,plotSample)


df <- as.data.frame(AllSamples[5])
names(df)[1] <- "x"
df$grid <- seq(-4,4,length=100000)
df$nor <- dnorm(df$grid)

ggplot(df) +
  geom_density(aes(x))+
  geom_line(aes(grid,nor),col = "brown",linetype = 2)+
  ggtitle("Comparison normal and sample 10000. Ex. 1")

df$R <- rnorm(10000)
ks.test(df$x,"pnorm",0,1)

