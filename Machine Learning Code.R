###Load the required packages
library(dplyr)
library(ggplot2)
library(ggthemes)
library(multcomp)
library(caret)
library(randomForest)
library(e1071)
library(class)

date<-Sys.Date()

### Set Working Directory
setwd("C:\\Users\\...\\MATH501")

### Stream in our data
orchids_tbl <- read.table("orchids.txt", header = TRUE, sep = "", quote = "\"'",dec = ".")

attach(orchids_tbl)

###################################################

## Part a)

# Present the data visually using bivariate scatter plots and colour-coding to distinguish between three locations of orchids. 
# Choose two of the three characteristcs X1, X2 and X3 that should be used as predictors for orchids and locations. Justify your choice using graphs. 
# Comment on the data in the context of the problem.

# Plot 1
# Petal length and leaf width 
ggplot (orchids_tbl)+ 
  geom_point(aes(x = X1, y = X2, col = loc)) +
  labs(x = "Petal Length (mm)", y = "Leaf Width (mm)",
       col = "Location") + 
  theme_wsj() + scale_color_wsj("colors6") +
  theme(legend.position = "bottom", plot.title = element_text(size = 10), legend.title = element_text(size = 8), axis.text = element_text (size = 8)) +
  ggtitle("Relationship between Petal Length and Leaf Width")

# Plot 2
# Petal length and petal width 
ggplot (orchids_tbl)+ 
  geom_point(aes(x = X1, y = X3, col = loc)) +
  labs(x = "Petal Length (mm)", y = "Petal Width (mm)",
       col = "Location") +
  theme_wsj() + scale_color_wsj("colors6") +
  theme(legend.position = "bottom", plot.title = element_text(size = 10), legend.title = element_text(size = 8), axis.text = element_text(size = 8)) +
  ggtitle("Relationship between Petal Width and Petal Length")

# Plot 3
# Leaf width and petal width 
ggplot (orchids_tbl)+ 
  geom_point(aes(x = X2, y = X3, col = loc)) +
  labs(x = "Leaf Width (mm)", y = "Petal Width (mm)",
       col = "Location")+
  theme_wsj() + scale_color_wsj("colors6") +
  theme(legend.position = "bottom", plot.title = element_text(size = 10), legend.title = element_text(size = 8), axis.text = element_text(size = 8)) +
  ggtitle("Relationship between Petal Width and Leaf Width")

######

## Part b)

# Create a training set consisting of 210 randomly chosen data points and a test set consisting of the remaining 60 data points.

set.seed(2) # to make the results reproducible
test <-sample(270,60)

train.orchids <- orchids_tbl[-test, ]
test.orchids <- orchids_tbl[test, ]

######

## Part c)

# Using the training data set we apply the K-nearest-neighbours method to construct a classifier to predict loc based on the two predictors 
# that we identified in (a).

train.X <- cbind(train.orchids$X1, train.orchids$X2)
cl <-as.factor(as.numeric(loc))
train.cl <- as.factor(as.numeric(train.orchids$loc))
df <- subset(train.orchids, select = -c(X3))

df <- subset(train.orchids, select = -c(X3))

train_control <- trainControl(method="LOOCV")

loocv.knn <- train(loc~.,
                   data=df,
                   trControl=train_control,
                   method="knn",
                   tuneGrid=data.frame(k=1:20))

# Display results:
loocv.knn

### Visualize the classification rule:

#plot(loocv.knn)

#k = 5 was found 

#Section needed to plot knn classifier
library(class)
k <- loocv.knn$bestTune$k
len <- 50 # we use a length of 50
#range(X1) #finds the range of values for X1
#range(X2) #finds the range of values for X2

xp <-seq(5.2, 20.7, length = len)
yp <-seq(14.2, 33.9, length = len)

xygrid <- expand.grid(X1 = xp, X2 = yp)
grid.knn <-knn( train = train.X, test = xygrid, cl = train.cl, k = k)

#sets colours based on LOC
col3 <-rep("lightblue", len*len)
for(i in 1:(len*len))
  if (grid.knn[i]=='1') {
    col3[i] <- "gold3"
  } else if(grid.knn[i]=='2') {
    col3[i] <- "orangered4"
  } else if(grid.knn[i]=='3') {
    col3[i] <- "darkgreen"
  }

# Plot the classification   
plot(xygrid,
     col = col3,
     main = "KNN classifier with K=5",
     xlab = "Petal Length (mm)",
     ylab = "Leaf Width (mm)")
contour(xp,
        yp,
        matrix(grid.knn, len),
        nlevels = 2,
        add = TRUE,
        lwd = 2)
points(X1, X2, pch = 20, col = loc)

######

### Part d)

# Using the training data set we apply the random forest (bagging) method to construct a classifier to predict loc based on 
# the two predictors that we identified in (a).

dff <- transform(df, loc = as.factor(as.numeric(loc)))
bag.tree <-randomForest(dff$loc~., 
                        data = dff,
                        subset = -test,
                        mtry = 2, 
                        importance = TRUE)

## Visualize the resulting classification rule in the scatter plot of the two predictors.

#plot scatter of bagging
grid.tree <- predict( bag.tree, xygrid, type = "class")
col3 <- rep("mediumseagreen", len*len) 
for (i in 1:(len*len)){
  if (grid.tree[i]== '1') col3[i] <- "gold3"
  else if (grid.tree[i]== '2') col3[i] <- "orangered4"
  else if (grid.tree[i]== '3') col3[i] <- "darkgreen"
}
plot(xygrid, col = col3, 
     main = "RandomForest Bagging Classification", 
     xlab = "Petal Length (mm)", 
     ylab = "Leaf Width (mm)")
contour(xp,
        yp,
        matrix(grid.tree, len),
        nlevels = 2,
        add = TRUE,
        lwd = 2)
points(X1, X2, pch = 20, col = loc)

######

### Part e)

# Using the training data set we apply the Support Vector Machines algorithm to construct a classifier to predict loc based on 
# the two predictors that we identified in (a).

### Decide which kernel is more suitable.
# The following section considers two kernels for Support Vector Machines to construct a classifier.

## Linear:
set.seed(1)
tune.out.lin = tune(svm,
                    as.factor(loc) ~ X1 + X2,
                    data = train.orchids,
                    kernel ="linear",
                    ranges = list(cost = c(0.001, 
                                           0.01, 
                                           0.1, 
                                           1, 
                                           5, 
                                           10, 
                                           100,
                                           1000,
                                           10000) ))

#?tune
summary(tune.out.lin)
bestmod.lin = tune.out.lin$best.model
#bestmod.lin

## Polynomial:
set.seed(1)
tune.out.ply =tune(svm,
                   as.factor(loc) ~ X1 + X2,
                   data = train.orchids,
                   kernel ="polynomial",
                   ranges =list(cost =seq(from = 0.01,
                                          to = 20,
                                          length = 20),
                                degree =seq(from = 1,
                                            to = 5, 
                                            length = 5) ))
summary(tune.out.ply)

bestmod.ply = tune.out.ply$best.model
#bestmod.ply

## Visualize the resulting classification rule in the scatter plot of the two predictors:

svm.fit <- svm( as.factor(loc) ~ X1 + X2,
                data = train.orchids,
                kernel = "linear",
                cost=10,
                scale = FALSE)

grid.svm = predict(svm.fit, xygrid, type = "class")

col3 <- rep("mediumseagreen", len*len) 
for (i in 1:(len*len)){
  if (grid.svm[i]== '1') col3[i] <- "gold3"
  else if (grid.svm[i]== '2') col3[i] <- "orangered4"
  else if (grid.svm[i]== '3') col3[i] <- "darkgreen"
}

plot(xygrid, col = col3, 
     main = "Simple Vector Machine Classification using Linear Kernel", 
     xlab = "Petal Length (mm)", 
     ylab = "Leaf Width (mm)")
contour(xp,
        yp,
        matrix(grid.svm, len),
        nlevels = 2,
        add = TRUE,
        lwd = 2)
points(X1, X2, pch = 20, col = loc)

#####

### Part f)

# We try to find the test error for the three classification rules constructed in (c), (d) and (e), respectively, 
# using the test set created in (b). 

## Calculating Test Error:
test.X <- cbind(test.orchids$X1, test.orchids$X2)
test.cl <- as.factor(test.orchids$loc)

# knn error
def.knn.k <-knn( train = train.X,
                 test = test.X,
                 cl = train.cl,
                 k = k)
tab.knn <-table(def.knn.k, test.cl)
error.knn <- (tab.knn[1,2]+tab.knn[2,1])/ sum(tab.knn)
#error.knn # 

# bagging

bag.pred <-predict(bag.tree, test.orchids, type = "class")
tab.bag <-table(bag.pred, test.orchids$loc)
error.bag <- (tab.bag[1,2]+tab.bag[2,1])/ sum(tab.bag)
#error.bag

# svm

# linear test error
ypred.lin = predict(bestmod.lin, test.orchids)
tab.svm.lin <- table(predict = ypred.lin, truth = test.cl ) 
error.svm.lin <- (sum(tab.svm.lin) - sum(diag(tab.svm.lin))) / sum(tab.svm.lin)
#error.svm.lin

# poly test error
ypred.ply = predict(bestmod.ply, test.orchids)
tab.svm.ply <- table(predict = ypred.ply, truth = test.cl ) 
error.svm.ply <- (sum(tab.svm.ply) - sum(diag(tab.svm.ply))) / sum(tab.svm.ply)
#error.svm.ply