library(rpart)
library(ROCR)
library(ipred)
library(randomForest)
library(adabag)
library(gbm)

data_file = "F:\\MiNI IAD\\Sem 1\\Machine Learning\\Laboratoria\\Machine-Learning-Course\\Lab8\\SAheart.data"

heart_raw <- read.table(data_file, h=T, row.names = 1,sep=",")
heart <- heart_raw
head(heart)

heart$chd <-as.factor(heart$chd)

heart$famh_class <- ifelse(as.factor(heart$famhist)=="Present",1,0)
heart$famhist <- NULL

samp <- sample(1:nrow(heart),2*nrow(heart)/3)
heart_tren <- heart[samp,]
heart_test <- heart[-samp,]

m1 <- rpart(chd~.,data = heart_tren)
pred1 <- predict(m1,heart_test)[,2]
pred_all1 <- prediction(pred1,heart_test$chd)
perf <- performance(pred_all1,measure ="auc")
perf@y.values

roc.ROCR1 <- performance(pred_all1,measure ="tpr", x.measure = "fpr")
plot(roc.ROCR1, main = "ROC Curve", col = "orange")
abline(0,1)

m2 <- bagging(chd~.,data=heart_tren)
pred2<- predict(m2,heart_test)$prob[,2]
pred_all2 <- prediction(pred2,heart_test$chd)
perf2 <- performance(pred_all2,measure ="auc")
perf2@y.values

roc.ROCR2 <- performance(pred_all2,measure ="tpr", x.measure = "fpr")
plot(roc.ROCR2, add=TRUE, col = "blue")


m3 <- boosting(chd~.,data=heart_tren)
pred3<- predict(m3,heart_test)$prob[,2]
pred_all3 <- prediction(pred3,heart_test$chd)
perf3 <- performance(pred_all3,measure ="auc")
perf3@y.values

roc.ROCR3 <- performance(pred_all3,measure ="tpr", x.measure = "fpr")
plot(roc.ROCR3,add=TRUE, col = "green")


m4 <- randomForest(chd~.,data=heart_tren)
pred4<- predict(m4,heart_test, type='prob')[,2]
pred_all4 <- prediction(pred4,heart_test$chd)
perf4 <- performance(pred_all4,measure ="auc")
perf4@y.values

roc.ROCR4 <- performance(pred_all4,measure ="tpr", x.measure = "fpr")
plot(roc.ROCR4,add=TRUE, col = "black")


m5 <- gbm(chd~.,data=heart_tren, distribution = "multinomial")
pred5<- matrix(predict(m5,heart_test, n.trees=100, type='response'),ncol=2)[,2]
pred_all5 <- prediction(pred5,heart_test$chd)
perf5 <- performance(pred_all5,measure ="auc")
perf5@y.values

roc.ROCR5 <- performance(pred_all5,measure ="tpr", x.measure = "fpr")
plot(roc.ROCR5,add=TRUE, col = "purple")


library("extraTrees")
library("xgboost")
heart$chd = heart_raw$chd
m6 <- xgboost(data = data.matrix(heart_tren)[,-9], 
              label = as.numeric(as.vector(heart_tren$chd)), 
              nrounds = 80, max.depth = 16, eta = 1, 
              nthreads = 4, objective = "binary:logistic")
pred6<- predict(m6,data.matrix(heart_test[,-9]))
pred_all6 <- prediction(pred6,heart_test$chd)
perf6 <- performance(pred_all6,measure ="auc")
perf6@y.values

roc.ROCR6 <- performance(pred_all6,measure ="tpr", x.measure = "fpr")
plot(roc.ROCR6,add=TRUE, col = "navyblue")



m7 <- extraTrees(x = heart_tren[,-9], y = heart_tren$chd)
pred7<- predict(m7,as.matrix(heart_test[,-9]), probability = TRUE)[,2]
pred_all7 <- prediction(pred7,heart_test$chd)
perf7 <- performance(pred_all7,measure ="auc")
perf7@y.values

roc.ROCR7 <- performance(pred_all7,measure ="tpr", x.measure = "fpr")
plot(roc.ROCR7,add=TRUE, col = "red")


