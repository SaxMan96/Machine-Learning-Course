library(rpart)
library(rpart.plot)
library(ISLR)
head(Default)
Default$student=ifelse(Default$student=="Yes",1,0)
Default$default=ifelse(Default$default=="Yes",1,0)

smp_size <- floor(0.5 * nrow(Default))

set.seed(123)
train_ind <- sample(seq_len(nrow(Default)), size = smp_size)

train <- Default[train_ind, ]
test <- Default[-train_ind, ]

# Linear
m1 <- glm(default ~ ., data = train, family = "binomial")
summary(m1)
# Tree
tree <- rpart(as.factor(default) ~ ., data = train)
rpart.plot(tree)

prob1 <- predict(m1, test, type = "response")
pred1 <- ifelse(prob1>0.5,1,0)
tab1 <- table(test$default, pred1)

prob2 <- predict(tree, test)[,2]
pred2 <- ifelse(prob2>0.1,1,0)
tab2<- table(test$default, pred2)

ac1 <- sum(diag(tab2))
