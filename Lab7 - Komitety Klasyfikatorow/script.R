library(rpart)
library(ROCR)

wine <- read.csv2("./winequality-white.csv", dec =".")
head(wine)
unique(wine$quality)
wine$quality[wine$quality<=5] <- 0
wine$quality[wine$quality>5] <- 1

wine$quality <- as.factor(wine$quality)
samp <- sample(1:nrow(wine),2*nrow(wine)/3)
wine_tren <- wine[samp,]
wine_test <- wine[-samp,]

m1 <- rpart(quality~.,data = wine_tren)
pred <- predict(m1,wine_test)[,2]
pred_all <- prediction(pred,wine_test$quality)
perf <- performance(pred_all,measure ="auc")
perf@y.values

bagging_pred <- function(num_class,data_tren,data_test)
{
  pred_mean <- numeric(nrow(data_test))
  num_obs <- nrow(data_tren)
  for(i in 1:num_class)
  {
    samp <- sample(1:num_obs,replace=TRUE)
    data_boot <- data_tren[samp,]
    m <- rpart(quality~.,data=data_boot)
    pred <- predict(m,data_test)[,2]
    pred_mean <- pred_mean+pred
  }
  pred_mean <- pred_mean/num_class
  return(pred_mean)
}

pred <- bagging_pred(100,wine_tren,wine_test)
pred_all <- prediction(pred,wine_test$quality)
perf <- performance(pred_all,measure ="auc")
perf@y.values

#zad2
library(ipred)
library(randomForest)
library(adabag)
library(gbm)

wine <- read.csv2("./winequality-white.csv", dec =".")
head(wine)
unique(wine$quality)
wine$quality[wine$quality<=5] <- 0
wine$quality[wine$quality>5] <- 1

wine$quality <- as.factor(wine$quality)
samp <- sample(1:nrow(wine),2*nrow(wine)/3)
wine_tren <- wine[samp,]
wine_test <- wine[-samp,]

m1 <- rpart(quality~.,data = wine_tren)
pred1 <- predict(m1,wine_test)[,2]
pred_all1 <- prediction(pred1,wine_test$quality)
perf <- performance(pred_all1,measure ="auc")
perf@y.values

roc.ROCR1 <- performance(pred_all1,measure ="tpr", x.measure = "fpr")
plot(roc.ROCR1, main = "ROC Curve", col = "orange")
abline(0,1)

m2 <- bagging(quality~.,data=wine_tren)
pred2<- predict(m2,wine_test)$prob[,2]
pred_all2 <- prediction(pred2,wine_test$quality)
perf2 <- performance(pred_all2,measure ="auc")
perf2@y.values

roc.ROCR2 <- performance(pred_all2,measure ="tpr", x.measure = "fpr")
plot(roc.ROCR2, add=TRUE, col = "blue")


m3 <- boosting(quality~.,data=wine_tren)
pred3<- predict(m3,wine_test)$prob[,2]
pred_all3 <- prediction(pred3,wine_test$quality)
perf3 <- performance(pred_all3,measure ="auc")
perf3@y.values

roc.ROCR3 <- performance(pred_all3,measure ="tpr", x.measure = "fpr")
plot(roc.ROCR3,add=TRUE, col = "green")


m4 <- randomForest(quality~.,data=wine_tren)
pred4<- predict(m4,wine_test, type='prob')[,2]
pred_all4 <- prediction(pred4,wine_test$quality)
perf4 <- performance(pred_all4,measure ="auc")
perf4@y.values

roc.ROCR4 <- performance(pred_all4,measure ="tpr", x.measure = "fpr")
plot(roc.ROCR4,add=TRUE, col = "black")


m5 <- gbm(quality~.,data=wine_tren, distribution = "multinomial")
pred5<- matrix(predict(m5,wine_test, n.trees=100, type='response'),ncol=2)[,2]
pred_all5 <- prediction(pred5,wine_test$quality)
perf5 <- performance(pred_all5,measure ="auc")
perf5@y.values

roc.ROCR5 <- performance(pred_all5,measure ="tpr", x.measure = "fpr")
plot(roc.ROCR5,add=TRUE, col = "purple")


#zad3

library(ISLR)
library(MASS)
library(mboost)

Boston
samp <- sample(1:nrow(Boston),2*nrow(Boston)/3)
Boston_tren <- Boston[samp,]
Boston_test <- Boston[-samp,]

B <- 100

err1 <- numeric(B)
err2 <- numeric(B)
err3 <- numeric(B)

for(b in 1:B)
{
  cat("Simulation", b, "out of ", B, "\n")
  del <- sample(1:nrow(Boston_tren),0.1*nrow(Boston_tren))
  Boston_tren1 <- Boston_tren[-del,]
  m1 <- rpart(medv~.,data = Boston_tren1)
  m2 <- randomForest(medv~.,data = Boston_tren1)
  m3 <- glmboost(medv~.,data = Boston_tren1, family = Gaussian())
  
  err1[b] <- sqrt(sum(Boston_test$medv-predict(m1,Boston_test))^2)
  err2[b] <- sqrt(sum(Boston_test$medv-predict(m2,Boston_test))^2)
  err3[b] <- sqrt(sum(Boston_test$medv-predict(m3,Boston_test))^2)
  
}
boxplot(err1,err2,err3,names=c("rpart","randomForest","glmboost"),col = "orange")
mean(err1)
mean(err2)
mean(err3)
var(err1)
var(err2)
var(err3)
