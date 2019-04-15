#------------mój przykład
exampler = 500
n <- matrix(rnorm(1000),nc=2)
y1 <- c(rep(1,500),rep(0,500))
n_2 <- jitter(n/sqrt(rowSums(n^2/4)),1,0.2)
n_1 <- jitter(n/sqrt(rowSums(n^2)),1,0.2)
x1 <- rbind(n_1,n_2)
# par(mfrow=c(1,2))
# plot(x1, col=as.factor(y1))

#--------------Przykład prowadzącego

y2 <- c(rep(1,500),rep(0,500))
x2 <- matrix(0,ncol=2,nrow=1000)
x2[1:500,1] <- runif(500,-1,1)
x2[501:1000,1] <- runif(500,-2,2)
x2[1:500,2] <- sample(c(-1,1),size=500,replace=T,prob=c(0.5,0.5))*sqrt(1-x2[1:500,1]^2)
x2[501:1000,2] <- sample(c(-1,1),size=500,replace=T,prob=c(0.5,0.5))*sqrt(4-x2[501:1000,1]^2)
x2[,2] <- x2[,2]+rnorm(1000,0,0.1)
# plot(x,col=as.factor(y2))


library(ggplot2)
library(e1071)

m1 <- svm(x1,as.factor(y1), kernel="linear")
m2 <- svm(x1,as.factor(y1), kernel="radial")
m3 <- svm(x1,as.factor(y1), kernel="polynomial")

yhat1 <- predict(m1,x1)
yhat2 <- predict(m2,x1)
yhat3 <- predict(m3,x1)

p1 <- ggplot(data=data.frame(y=y1,x=x1),aes(x=x1[,1], y=x1[,2], col=as.factor(y1)))+geom_point()+ggtitle("my_example") 
p2 <- ggplot(data=data.frame(y=yhat1,x=x1),aes(x=x1[,1], y=x1[,2], col=as.factor(yhat1)))+geom_point()+ggtitle("linear")
p3 <- ggplot(data=data.frame(y=yhat2,x=x1),aes(x=x1[,1], y=x1[,2], col=as.factor(yhat2)))+geom_point()+ggtitle("radial")
p4 <- ggplot(data=data.frame(y=yhat3,x=x1),aes(x=x1[,1], y=x1[,2], col=as.factor(yhat3)))+geom_point()+ggtitle("polynomial")

m4 <- svm(x2,as.factor(y2), kernel="linear")
m5 <- svm(x2,as.factor(y2), kernel="radial")
m6 <- svm(x2,as.factor(y2), kernel="polynomial")

yhat4 <- predict(m4,x2)
yhat5 <- predict(m5,x2)
yhat6 <- predict(m6,x2)

p5 <- ggplot(data=data.frame(y=y2,x=x2),aes(x=x2[,1], y=x2[,2], col=as.factor(y2)))+geom_point()+ggtitle("kr_example")
p6 <- ggplot(data=data.frame(y=yhat4,x=x2),aes(x=x2[,1], y=x2[,2], col=as.factor(yhat4)))+geom_point()+ggtitle("linear")
p7 <- ggplot(data=data.frame(y=yhat5,x=x2),aes(x=x2[,1], y=x2[,2], col=as.factor(yhat5)))+geom_point()+ggtitle("radial")
p8 <- ggplot(data=data.frame(y=yhat6,x=x2),aes(x=x2[,1], y=x2[,2], col=as.factor(yhat6)))+geom_point()+ggtitle("polynomial")

multiplot(p1, p2, p3, p4, p5, p6, p7, p8, cols=2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
