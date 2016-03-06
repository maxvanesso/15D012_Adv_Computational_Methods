tweaker <- function(Y,rev = F, levels = NULL){
  
  if(rev == F){
    Y <- as.numeric(as.character(Y))
    levels <- sort(unique(Y))
    Y[Y %in% levels[1]] <- -1
    Y[Y %in% levels[2]] <- 1
  }else{
    Y[Y == -1] <- levels[1]
    Y[Y == 1] <- levels[2]
    Y <- as.factor(Y)
  }
  
  return(Y)
  
}

dummie <- function(formula, data, w, depth, method = "class") {
  
  environment(formula) <- environment()
  rpart(formula, data, weights = w, method = method, control = rpart.control(maxdepth = depth))
}

adaBoost <- function(formula, data, depth, noTrees, type = "train", test.data = NULL) {
  
  require(assertthat)
  require(rpart)
  not_empty(data);not_empty(formula);
  is.count(depth); is.count(noTrees);
  is.string(type); assert_that(type %in% c("train", "predict"));
  
  # Extracting the data from the formula
  m.frame <- model.frame(formula,data)
  names <- colnames(m.frame)
  Terms <- attr(m.frame, "terms")
  Y <- model.response(m.frame)
  Xcoln <- attr(Terms, "term.labels")
  X <- subset(m.frame, select = Xcoln)
  nrow <- nrow(X)
  
  levels <- levels(Y)
  Y <- tweaker(Y)
  
  data <- data.frame(Y, X)
  colnames(data) <- names
  
  my.weights <- matrix(1/nrow,nrow = noTrees, ncol = nrow)
  predictions <- matrix(0, nrow = noTrees, ncol = nrow)
  alpha <- rep(0,noTrees)
  error <- rep(0, noTrees)
  
  if(type == "predict"){
    test.predictions <- matrix(0, nrow = noTrees, ncol = nrow(test.data))
  }
  
  for(iter in 1:noTrees){
    
    weights.iter <- my.weights[iter, ]
    
    fit.tree <- dummie(formula, data, weights.iter, depth)
    
    predictions[iter,] <- as.numeric(as.character(predict(fit.tree, type = "class")))
    
    if(type == "predict"){
      test.predictions[iter,] = as.numeric(as.character(predict(fit.tree, newdata = test.data, type = "class")))
    }
    
    indicator <- as.numeric(predictions[iter,] != Y)
    
    error[iter] <- t(weights.iter) %*% indicator / sum(weights.iter)
    
    alpha[iter] <- log((1 - error[iter]) / error[iter])
    
    if(iter < noTrees){
      my.weights[iter+1,] <- weights.iter * exp(alpha[iter] * indicator)
    }
  }
  
  predLabels <- sign(alpha %*% predictions)
  predLabels <- tweaker(predLabels, rev = T, levels)
  testLabels <- sign(alpha %*% test.predictions)
  testLabels <- tweaker(testLabels, rev = T, levels)
  
  acc <- mean(predLabels == model.response(m.frame))
  
  if(type == "predict"){
    return(list(predLabels = predLabels, testLabels = testLabels, acc = acc))
  }else{
    return(list(predLabels = predLabels, acc = acc)) 
  }
  
}
