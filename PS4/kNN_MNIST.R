#load the cvs files and define the inputs for the knn function
MNIST_training <- read.csv("/home/max/Escritorio/15D012_Adv_Computational_Methods/CSVfilesforPS3/MNIST_training.csv", header=FALSE)

MNIST_test <- read.csv("/home/max/Escritorio/15D012_Adv_Computational_Methods/CSVfilesforPS3/MNIST_test.csv", header=FALSE)

features <- as.matrix(MNIST_training[,-1])

labels <- as.numeric(MNIST_training[,1])

pixelstest <- as.matrix(MNIST_test)

#load the library needed
library(class)

predictions <- knn(features,pixelstest,labels,k=5)

# save the CSV predictions file
write.csv(predictions, file="MNIST_predictions.csv", row.names = FALSE)