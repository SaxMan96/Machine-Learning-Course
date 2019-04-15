# Zad 2
# schemat1
n <- 1000
p <- 2
x <- matrix(0,nrow = n, ncol = p)
y <-rbinom(n,1,0.5)
w1 <- which(y==1)
w0 <- which(y==0)
for(j in 1:p)
{
  x[w1,j] <-rnorm(length(w1),1,1)
}
for(j in 1:p)
{
  x[w0,j] <-rnorm(length(w0),0,1)
}

data <- data.frame(x,y)
library(MASS)
#LDA
l1 <- lda(y~.,data = data)

pred1 <- predict(l1, data)
pred1$class
tab1 <- table(y,pred1$class)

sum(diag(tab1))/n

#QDA
q1 <- qda(y~.,data = data)

pred2 <- predict(q1, data)
pred2$class
tab2 <- table(y,pred2$class)

sum(diag(tab2))/n

#schemat2

n <- 1000
p <- 2
x <- matrix(0,nrow = n, ncol = p)
y <-rbinom(n,1,0.5)
w1 <- which(y==1)
w0 <- which(y==0)

Sigma0 <- matrix(c(1,0.8,0.8,1),ncol=2)
x[w0,] <- mvrnorm(n = length(w0),mu=c(0,0),Sigma = Sigma0)
Sigma1 <- matrix(c(1,-0.8,-0.8,1),ncol=2)
x[w1,] <- mvrnorm(n = length(w1),mu=c(1,1),Sigma = Sigma1)

data <- data.frame(x,y)
library(MASS)
#LDA
l1 <- lda(y~.,data = data)

pred1 <- predict(l1, data)
pred1$class
tab1 <- table(y,pred1$class)

sum(diag(tab1))/n

#QDA
q1 <- qda(y~.,data = data)

pred2 <- predict(q1, data)
pred2$class
tab2 <- table(y,pred2$class)

sum(diag(tab2))/n

