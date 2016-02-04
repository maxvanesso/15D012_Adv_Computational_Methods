kNN <- function(features, labels, memory = NULL, 
                k = 1, p = 2, type="train", norm = TRUE) {
  
  library(assertthat)
  library(flexclust)
  library(ggplot2)
  
  # Different tests for different types
  not_empty(features); not_empty(labels); 
  is.string(type); assert_that(type %in% c("train", "predict"))
  if (type == "train") {
    assert_that(nrow(features) == length(labels))
  }else if (type == "predict") {
    assert_that(nrow(features) == length(labels) &
                  not_empty(memory) & 
                  ncol(memory) == ncol(features) )
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
  neighbors <- apply(distMatrix, 2, order)
  
  # Compute and return the most frequent class in the k nearest neighbors
  predLabels <- rep(NA, noObs)
  prob <- rep(NA, noObs)
  for (obs in 1:noObs) {
    predLabels[obs] <- as.numeric(names(tail(sort(table(labels[neighbors[1:k, obs]])),1)))
    prob[obs] <- round(tail(sort(table(labels[neighbors[1:k, obs]])),1)/k,4)
  }
  
  # Accuracy and confusion matrix for errorCount available only if training
  if (type == "train") {
    errorCount <- table(predLabels, labels)
    accuracy <- mean(predLabels == labels)
  } else if (type == "predict") {
    errorCount <- NA
    accuracy <- NA
  }
  
  # Returning the results
  return(list(predLabels = predLabels, 
              prob = prob,
              accuracy = accuracy,
              errorCount = errorCount))
}

norm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}