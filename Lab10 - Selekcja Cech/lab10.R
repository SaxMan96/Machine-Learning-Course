n  = 100
p = 1000
x <- matrix(rnorm(n*p), ncol=p)
b <- numeric(p)
true <- c(1,2,3)
b[1] <- 1
b[2] <- 1
b[3] <- 1
# p_i = 1/(1+exp(-()))

eta <- x%*%b
probs <- 1/(1+exp(-eta))
y <- rbinom(n,1,probs)
# p <- b[1,] %*% t(matrix(x))
nrow(P) 
ncol(P)
# ncol(x)
# nrow(x)
#
# nrow(matrix(b[1,]))
# ncol(matrix(b[1,]))
# 
# nrow(matrix(x[1,]))
# ncol(matrix(x[1,]))
library(glmnet)
cv.glmnet1 <- cv.glmnet(x,y,family="binomial")
lambda_opt <- cv.glmnet1$lambda.lse
model1 <- glmnet(x,y,family="binomial",lambda=lambda_opt)
wspolczynniki1 <- model1$beta

est_true1 <- which(wspolczynniki1!=0)
recall1 <- length(intersect(true,est_true1))/length(true)
prec1 <- length(intersect(true,est_true1))/length(est_true1)



d <- data.frame(x,y)
biggest <- formula(glm(y~.,data=d))
min.model <- glm(y~1,data=d)
m2 <- step(min.model,data=d,direction='forward',scope= biggest, k=log(n))

wspolczynniki2 <- m2$coef

est_true2 <- which(wspolczynniki2!=0)
recall2 <- length(intersect(true,est_true2))/length(true)
prec2 <- length(intersect(true,est_true2))/length(est_true2)

library(hdi)
m3 <- multi.split(x,y)



#----- zaad 4
n  = 200
p = 20
B = 100
j <- 0
coefs <- list()
for(p in c(5,10,20,40))
{
  coed1 <- numeric(B)
  true <- c(1,2,3)
  for(k in 1:B){
    x <- matrix(rnorm(n*p), ncol=p)
    b <- numeric(p)
    b[true] <- 1
    
    eta <- x%*%b
    probs <- 1/(1+exp(-eta))
    y <- rbinom(n,1,probs)
    
    d <- data.frame(x,y)
    m <- glm(y~.,data=d)
    coef1[k] <- m$coef[2]
  }
  j <- j+1
  coefs[[j]] <- coef1
}

sapply(coefs,var)
boxplot(coefs)
