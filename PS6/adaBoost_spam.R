library(rpart)
library(ggplot2)
library(adabag)
library(reshape2)

spambase <- read.csv("/home/max/Escritorio/Data Science/2nd term/15D012 Advanced Computational Methods/problemSets/spambase.data", sep=",", header=FALSE)

source("/home/max/Escritorio/Data Science/2nd term/15D012 Advanced Computational Methods/problemSets/adaBoost.R")

data <- spambase
data$V58 <- as.factor(data$V58)

nrow <- nrow(data)
ncol <- ncol(data)

train.ind <- sample(1:nrow, 3000)

train.data <- data[train.ind,]
test.data <- data.frame(data[-train.ind, -ncol])
colnames(test.data) <- colnames(train.data)[-ncol]

depths <- seq(1, 12)
my.train.error <- rep(NA, length(depths))
my.test.error <- rep(NA, length(depths))

package.train.error <- rep(NA, length(depths))
pkg.test.error <- rep(NA, length(depths))

for(k in depths){
  
  # Tree building, calling the function adaBoost
  my.adaBoost <- adaBoost(formula = V58 ~ . , data = train.data , depth = k ,
                         noTrees = 10 , type = "predict", test.data = test.data)
  
  my.train.error[k] <- 1 - mean(my.adaBoost$predLabels == train.data[, c("V58")])
  my.test.error[k] <- 1 - mean(my.adaBoost$testLabels == data[-train.ind, c("V58")])
  
  fit <- boosting(formula = V58 ~ ., data = train.data,
                  mfinal = 10, control=rpart.control(maxdepth=k))
  package.train <- as.factor(fit$class)
  predBST.test <- predict(fit, newdata = test.data)
  pkg.test <- as.factor(predBST.test$class)
  
  package.train.error[k] <- 1 - mean(package.train == train.data[, c("V58")])
  pkg.test.error[k] <- 1 - mean(pkg.test == data[-train.ind, c("V58")])
  
}

# minor modifications to the data to make it smoother and ErrorPlot building and saving
data2 <- data.frame(depth = depths, my.train.error, my.test.error, package.train.error, pkg.test.error)
data3 <- melt(data2, 1, variable.name = "type", value.name = "error")

ErrorPlot <- ggplot(data = data3, aes(x = depth, y = error, color = type)) + geom_line() + geom_point() +
  xlab("Tree Depth") + ylab("Misclassification Error") + theme_bw(base_size = 14, base_family = "Comic Sans")

cairo_pdf("cTree.pdf")
print(ErrorPlot) 
dev.off()
