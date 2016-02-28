# First we write the cost functions
MissClassError <- function(probability) {
  MissError <- 1 - apply(probability,1,max)
  return(MissError) 
}

Entropy <- function(probability) {
  CrossEntropy <- - rowSums(probability*log(probability))
  return(CrossEntropy)
}

Gini <- function(probability) {
  Gini <- rowSums(probability*(1-probability))
  return(Gini)
}

# Then we define a function to find a threshold (the point where the data is going to be split)
FindThreshold <- function(x, y, costFnc) {
  
  minimumError <- 1
  BestThreshold <- NA
  labels <- NA
  y <- as.factor(y)
  
  if(length(unique(x)) > 1){  
    
    xroll <- rollmean(sort(unique(x)),2)
    numberPoints <- length(x)
    
    DataSplit <- lapply(xroll,function(z) {     
      potentialThreshold <- z
      
      predictedClasses <- rep(NA, 2)
      predictedClasses[1] <- as.numeric(names(tail(sort(table(y[x <= potentialThreshold])),1)))
      predictedClasses[2] <- as.numeric(names(tail(sort(table(y[x > potentialThreshold])),1)))
      
      probability <- matrix(NA,nrow = 2,ncol = length(unique(y)))
      probability[1,] <- table(y[x <= potentialThreshold])/length(y[x <= potentialThreshold])
      probability[2,] <- table(y[x > potentialThreshold])/length(y[x <= potentialThreshold])
     
      # We calculate the total cost of this split
      totalcost <- sum(costFnc(probability))
      
      return(list(error = totalcost, 
                  threshold = potentialThreshold, 
                  labels = predictedClasses))
    })
    
    DataSplit <- data.frame(t(simplify2array(DataSplit,higher = TRUE)))
    # We also find the min and the best threshold
    minimumError <- min(abs(unlist(DataSplit[[1]])))
    indicator <- which(DataSplit[[1]] %in% c(minimumError,-minimumError))[1]
    BestThreshold <- DataSplit[[indicator,2]]
    
    # Calculate the final labels of the split
    labels <- DataSplit[[indicator,3]]
  }
  return(list(err = minimumError,
              thres = BestThreshold, 
              labels = labels))
}

# We build the function for the tree using recursion
cTree <- function(formula , data, depth , minPoints , 
                  costFnc = Entropy, type = "train" , testData = NULL){
  
  library(assertthat)
  not_empty(data); not_empty(formula);
  is.count(depth); is.count(minPoints);
  is.string(type); assert_that(type %in% c("train", "predict"));
  
  ModelFrame <- model.frame(formula,data)
  Terms = attr(ModelFrame, "terms")
  Y <- model.response(ModelFrame)
  columnNames = attr(Terms, "term.labels")
  X <- subset(ModelFrame, select = columnNames)
  ncol = ncol(X)
  nrow = nrow(X)
  PredictedLabels = rep(NA,nrow)
  probability = rep(NA,nrow)
  
  if(nrow(X) >= minPoints & depth > 0 & length(unique(Y)) > 1){
    
    results <- apply(X,2,function(x) FindThreshold(x, Y,costFnc))
    
    DataSplit <- data.frame(t(simplify2array(results,higher = TRUE)))
    # We also find the min and the best threshold
    minimumError <- min(unlist(DataSplit[[1]]))
    
    
      
      columnindicator <- which(DataSplit[[1]]==minimumError)[1]
      BestThreshold <- DataSplit[[columnindicator,2]]
      
      # Calculate the final labels of the split
      labels <- DataSplit[[columnindicator,3]]
      
      row.indicator <- which(X[, columnindicator] <= BestThreshold)
      Xleft <- ModelFrame[row.indicator,]
      Xright <- ModelFrame[-row.indicator,]
      
      if(type == "predict"){
        
        RowTestIndicator <- which(testData[, columnindicator] <= BestThreshold)
        LeftTest <- data.frame(testData[RowTestIndicator,])
        colnames(LeftTest) = colnames(X)
        RightTest <- data.frame(testData[-RowTestIndicator,])
        colnames(RightTest) = colnames(X)
        
        left <- cTree(formula, data = Xleft, depth = depth -1, 
                      minPoints, costFnc, type, testData = LeftTest )
        right <- cTree(formula, data = Xright, depth = depth -1, 
                       minPoints, costFnc, type, testData = RightTest )
        
      }else{
        
        left <- cTree(formula, data = Xleft, depth = depth -1, minPoints, costFnc, type )
        right <- cTree(formula, data = Xright, depth = depth -1, minPoints, costFnc, type )
      }
      
      PredictedLabels[row.indicator] <- left$predLabels
      probability[row.indicator] <- left$prob
      PredictedLabels[-row.indicator] <- right$predLabels
      probability[-row.indicator] <- right$prob
      
      if(type == "predict"){
        
        testLabels <- rep(NA,nrow(testData))
        testLabels[RowTestIndicator] <- left$testLabels
        testLabels[-RowTestIndicator] <- right$testLabels
        
        return(list(predLabels = PredictedLabels, prob = probability , testLabels = testLabels))
        
      }else{
        
        return(list(predLabels = PredictedLabels, prob = probability))
        
      
    }
  }
  assignedClass <- as.numeric(names(tail(sort(table(Y)),1)))
  probability <- tail(sort(table(Y)),1)/length(Y)
  
  if(type == "predict"){
    return(list(predLabels = assignedClass, prob = probability, testLabels  = assignedClass))
  }else{
    return(list(predLabels = assignedClass, prob = probability))
  }
  
}