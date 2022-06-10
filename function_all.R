## all functions
library(rootSolve)
library(flexsurv)
library(plotly)
##expo##

expsolver = function(t1, s1) {
  
  lambda = - log(s1)/t1
  
  params = list(rate=lambda)
  
  return(params)
}

##weibull##


wei1 <- function(x,a1,a2,b1,b2){
  #  X1 <- shape # a
  #  X2 <- scale # mu
  
  # 0 = shape(ln(t)-ln(scale)) - ln(-ln(s))
  
  F1 <- x[1]*(log(b1)-log(x[2])) - log(-log(a1))
  F2 <- x[1]*(log(b2)-log(x[2])) - log(-log(a2))
  c(F1=F1,F2=F2)}


wei2 <- function(t1,t2,s1,s2){
  
  params <- multiroot(f=wei1, start = c(0.1,0.1), a1=s1,a2=s2,b1=t1,b2=t2)$root
  
  shape = params[1]
  scale = params[2]
  
  output = list(shape=shape, scale=scale)
  return(output)
}


##gompertz##

gomp1 <- function(x,a1,a2,b1,b2){
  # X1 <- shape # a
  # X2 <- rate # b
  
  F1 <- log(a1) + ((x[2]/x[1])*(exp(x[1]*b1)-1))
  F2 <- log(a2) + ((x[2]/x[1])*(exp(x[1]*b2)-1))
  c(F1=F1,F2=F2)}

gomp2 <- function(t1,t2,s1,s2){
  
  params = multiroot(f=gomp1, start = c(0.1,0.1),a1=s1,a2=s2,b1=t1,b2=t2)$root
  
  shape = params[1]
  rate = params[2]
  output=list(shape=shape,rate=rate)
  return(output)
}


##log-logistic##
loglog1 <- function(x,a1,a2,b1,b2){
  # X1 <- shape # a
  # X2 <- scale # b
  
  F1 <- x[1]*(log(b1)-log(x[2])) - log((1/a1) - 1)
  F2 <- x[1]*(log(b2)-log(x[2])) - log((1/a2) - 1)
  c(F1=F1,F2=F2)}


loglog2 <- function(t1,t2,s1,s2){
  
  params = multiroot(f=loglog1, start = c(0.1,0.1), a1=s1,a2=s2,b1=t1,b2=t2)$root 
  
  shape = params[1]
  scale = params[2]
  
  output=list(shape=shape, scale=scale)
  return(output)
  
}