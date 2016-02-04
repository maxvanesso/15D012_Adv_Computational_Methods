# Normalizing function to take into account different scales
norm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

kNN <- function(features, labels, memory = NULL, 
                k = 1, p = 2, type="train",norm = TRUE) {
  
  # Libraries needed
  library(assertthat)
  library(flexclust)
  library(ggplot2)
  
  
  # Different tests for different types
  not_empty(features); not_empty(labels); 
  is.string(type); assert_that(type %in% c("train", "predict"))
  if (type == "train") {
    assert_that(nrow(features) == length(labels))
  }else if (type == "predict") {
    assert_that(nrow(features) == length(labels) & not_empty(memory) & ncol(memory) == ncol(features) )
  }
  is.count(k); assert_that(k <= nrow(features))
  if(p != Inf)is.count(p);
  
  if(norm == TRUE) features <- as.data.frame(lapply(features, norm))
  
  # Calculate distance for any integer of p from 1 to Inf
  method <- ifelse(p==1, 'manhattan', ifelse(p==2,'euclidean',ifelse(p == Inf,'maximum','minkowski')))
  
  if (type == "train") {
    noObs <- nrow(features)
    distMatrix <- as.matrix(dist(features,method = method,p=p))
  } else if (type == "predict") {
    noObs <- nrow(memory)
    if(norm == TRUE) memory <- as.data.frame(lapply(memory, norm))
    distMatrix <- as.matrix(dist2(features,memory,method = method,p=p))
  }
  
  # Sort the distances in increasing numerical order and pick the first k elements
  neighbours <- apply(distMatrix, 2, order)
  
  # Compute and return the most frequent class in the k nearest neighbors
  predictedLabels <- rep(NA, noObs)
  probability <- rep(NA, noObs)
  for (obs in 1:noObs) {
    predictedLabels[obs] <- as.numeric(names(tail(sort(table(labels[neighbours[1:k, obs]])),1)))
    probability[obs] <- round(tail(sort(table(labels[neighbours[1:k, obs]])),1)/k,4)
  }
  
  # Accuracy and confusion matrix for errorCount available only if training
  if (type == "train") {
    errorCount <- table(predictedLabels, labels)
    accuracy <- mean(predictedLabels == labels)
  } else if (type == "predict") {
    errorCount <- NA
    accuracy <- NA
  }
  
  # Returning the results
  return(list(predictedLabels = predictedLabels, 
              probability = probability,
              accuracy = accuracy,
              errorCount = errorCount))
}

genStickSnake <- function(noStick=100, noSnake=100, 
                          min=0, max=100, 
                          gradient=1, amplitude=0.2, wavelength=500, 
                          saveData=TRUE, 
                          savePlot=TRUE) {
  
  #min, max and gradient are variables for both the stick and the snake
  #amplitude and wavelength are only for the snake
  #Be careful, because if you change the range, amplitude or wavelength,
  #the data sets can easily become ugly.
  
  #calculating the stick
  x <- runif(noStick, min, max)
  y <- gradient*x + rnorm(noStick)
  stick <- data.frame(x, y)
  
  #calculating the snake
  x <- runif(noSnake, min, max)
  y <- gradient*x + rnorm(noSnake) + (max-min)*amplitude*sin((max-min)/wavelength*x)
  snake <- data.frame(x, y)
  
  #joining the stick and the snake
  sas <- rbind(stick, snake)
  stick_or_snake <- c(rep("stick", noStick), rep("snake", noSnake))
  target <- c(rep(0, noStick), rep(1, noSnake))
  sas <- data.frame(sas, target, stick_or_snake)
  names(sas) <- c("x1", "x2", "y", "label")
  return(sas)
}

dataset <- genStickSnake()
features <- dataset[,c(1,2)]
labels <- dataset$y
result <- kNN(features,labels,k = 5, p = 2, type = "train")

predictions <- data.frame(cbind(dataset,predictedLabels = result$predictedLabels, probability = result$probability))
colnames(predictions) <- c("Dimension1","Dimension2","ClassLabel","PredictedLabel","Probability")
predictions$ClassLabel <- factor(predictions$ClassLabel)

# save the data in CSV format 
write.csv(predictions, file="predictions.csv", row.names = FALSE)

test <- data.frame(cbind(rep(seq(min(predictions$Dimension1), max(predictions$Dimension1), length.out=20),20)
        ,rep(seq(min(predictions$Dimension2), max(predictions$Dimension2), length.out=20),each=20)))

dataPred <- kNN(features,labels,test,k = 5, p = 2, type = "predict")

dataPred$probability[dataPred$predictedLabels == 1] <- dataPred$probability[dataPred$predictedLabels == 1]
dataPred$probability[dataPred$predictedLabels == 0] <- 1 - dataPred$probability[dataPred$predictedLabels == 0]

df<-data.frame(cbind(test,z = dataPred$probability))


dataPlot <- ggplot(predictions, aes(x=Dimension1, y=Dimension2) ) + 
  geom_point(size = 3, aes(colour = ClassLabel)) + 
  geom_contour(data=df, aes(x=X1, y=X2, z=z),breaks = 0.5)

dataPlot
# saving
cairo_pdf("dataPlot.pdf")
print(dataPlot) 
dev.off()

#############################################################################################
