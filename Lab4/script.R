SA <- read.table("SAheart.data", sep=",", header=T)
SA <- SA[,-1]
SA.logit <- glm(chd~.,data = SA, family = "binomial")
summary(SA.logit)
exp(coef(SA.logit)["age"])
SA.logit.aic <-  step(SA.logit, direction = "backward", k=2)
SA.logit.bic <-  step(SA.logit, direction = "backward", k=log(nrow(SA)))
#zad 3.2
library(ggplot2)
dane <- read.table("earthquake.txt", header = T)
ggplot(data = dane, aes(x = body, y=surface, col = popn))+geom_point()
m1 <- glm(popn~.,data = dane, family = "binomial")
summary(m1)
