spambase <- read.csv("/home/max/Descargas/spambase.data", header=FALSE)

data = spambase
nrow = nrow(data)

trainInd = sample(1:nrow, 3000)

trainData = data[trainInd,]
testData = data.frame(data[-trainInd,-ncol(data)])
colnames(testData) = colnames(trainData)[-ncol(data)]

library(compiler)
library(zoo)
library(rpart)
library(reshape2)

# we simply pass the function we want to use through cmpfun()
FindThreshold <- cmpfun(FindThreshold)

depths = seq(1,10)
MyTrainningError = rep(NA,length(depths))
MyTestError = rep(NA,length(depths))

PackageTrainError = rep(NA,length(depths))
PackageTestError = rep(NA,length(depths))

for(k in depths){
  
  MyTree = cTree(formula = V58 ~ ., data = trainData, depth = k, 
                  minPoints = 430, costFnc = Gini, type = "predict", testData = testData)
  
  MyTrainningError[k] = 1 - mean(MyTree$predLabels == trainData[, c("V58")])
  MyTestError[k] = 1 - mean(MyTree$testLabels == data[-trainInd, c("V58")])
  
  fit <- rpart(formula = V58 ~ ., data = trainData, method = "class",
               control=rpart.control(maxdepth = k, minsplit = 430))
  PackageTrain = predict(fit, type = "class")
  PackageTest = predict(fit, newdata = testData, type = "class")
  
  PackageTrainError[k] = 1 - mean(PackageTrain == trainData[,c("V58")])
  PackageTestError[k] = 1 - mean(PackageTest == data[-trainInd,c("V58")])
  
}

# reshaping the data a bit and plotting
DataFrame <- data.frame(depth = depths, MyTrainningError, MyTestError, PackageTrainError, PackageTestError)
MeltedDataFrame <- melt(DataFrame, 1, variable.name = "type", value.name = "error")

errorPlot <- ggplot(data = MeltedDataFrame, aes(x = depth, y = error, color = type)) + 
  geom_line() + 
  geom_point() +
  xlab("Tree Depth") +
  ylab("Misclassification Error") +
  theme_bw(base_size = 14, base_family = "Comic Sans")

# saving
cairo_pdf("cTree.pdf")
print(errorPlot) 
dev.off()
