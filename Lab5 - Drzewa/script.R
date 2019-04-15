library(rpart)
library(rpart.plot)
SA <- read.table("SAheart.data",
                 h = T,
                 row.names = 1,
                 sep = ",")

#a
SA$chd
tree <- rpart(as.factor(chd) ~ .,
              data = SA,
              cp = 0.01,
              minsplit = 5)
rpart.plot(tree)

print(summary(tree), digits = 4)
par(mar = c(1, 0, 1, 0))
plot(tree)
text(tree)
head(SA)
test_obs <- as.data.frame(t(apply(SA[, -5], 2, mean)))
kl <- names(which(table(SA[, 5]) == max(table(SA[, 5]))))
test_obs <- cbind(test_obs, famhist = kl)
test_obs <- test_obs[,-9]
predict(tree, newdata = test_obs, type = "class")

#d
plotcp(tree)
tree$cptable
z <- prune.rpart(tree, cp = 0.04)
rpart.plot(z)
tree2 <-  rpart(
  as.factor(chd) ~ .,
  data = SA,
  cp = 0.01,
  minsplit = 5,
  parms = list(split = "information")
)
rpart.plot(tree2)

#Zad3
ft <- read.table("fitness.txt", h = TRUE)
head(ft)
#a
tree <- rpart(Oxygen ~ .,
              data = ft,
              cp = 0.01,
              minsplit = 2)
rpart.plot(tree)
#b
#prawy liść - runtim<=8.9, runpulce>=161\

#c
test_obs <- as.data.frame(t(apply(ft, 2, median)))
test_obs <- test_obs[,-3]
predict(tree, newdata = test_obs, type = "vector")

#d
#koszt zlozoność
plotcp(tree)
tree$cptable
z <- prune.rpart(tree, cp = 0.04)
rpart.plot(z)
#1SE
z <- prune.rpart(tree, cp = 0.01)
rpart.plot(z)

#e
tree1 <- rpart(Oxygen ~ .,
               data = ft[, c(1, 3, 4)],
               cp = 0.02,
               minsplit = 2)
rpart.plot(tree1)
n <- 100
min(ft$Age)
max(ft$Age)
Age1 <- seq(37, 58, length.out = n)
min(ft$RunTime)
max(ft$RunTime)
RunTime1 <- seq(8, 15, length.out = n)
newdata <- expand.grid(Age = Age1, RunTime = RunTime1)
newdata$z <- predict(tree1, newdata = newdata)
persp(
  Age1,
  RunTime1,
  matrix(newdata$z, n),
  theta = 50,
  phi = 30,
  expand = 0.5,
  col = "lightblue",
  zlab = "Oxygen prediction"
)
