hello <- function() {
  print("Hello, This is the function packages for Insik's data Mining!
        There are useful functions for data analysis.")
}



normalize <- function(x, class = FALSE, cl = ncol(x)){
  ds=x
  if(class == TRUE){
    ds = ds[, -cl]
  }
  nor = as.data.frame(lapply(ds, function(x){return((x- min(x))/(max(x) - min(x)))}))
  return(nor)
}



samplize <- function(x, is_valid = FALSE, p = c(4, 1)){
  set.seed(72170235)
  ds        <- x
  nds <- nrow(ds)
  partition <- c("train", "test")
  idx       <- c()
  rtn       <- c()
  test <- data.frame(); train <- data.frame(); valid <- data.frame();
  if(is_valid == FALSE){
    idx     <- sample(x = partition, size = nds, replace = TRUE, prob = p)
    test    <- ds[idx == "test", ]
    train   <- ds[idx == "train", ]
    rtn     <- list(train=train, test=test)
    return(rtn)
  }else{
    partition = c(partition, "valid")
    idx     <- sample(x = partition, size = nds, replace = TRUE, prob = p)
    test    <- ds[idx == "test", ]
    train   <- ds[idx == "train", ]
    valid   <- ds[idx == "valid", ]
    rtn     <- list(train=train, test=test, valid = valid)
    return(rtn)
  }
}



detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}



json<-function(data.frame,sink=FALSE) {
  require(RJSONIO)
  require(plyr)
  modified<-list(
    keys = colnames(data.frame),
    values = unname(alply(data.frame,1,identity))
  )
  if(sink!=FALSE) {
    sink(sink)
    cat(toJSON(modified,pretty=TRUE))
    sink()
  }
  if(sink==FALSE) {
    cat(toJSON(modified,pretty=TRUE))
  }
}



kfold.C50 <- function (ds,cl, fold=10, all.index = FALSE) {
  library(caret)   # for createFolds
  library(C50)          # for SVM
  set.seed(100)
  k.fold <-createFolds(as.vector(cl),k=fold)
  cl = factor(as.integer(cl))
  acc = c()              # classification result
  rcl = c() #recall
  pre = c() #precision
  for (i in 1:fold) {
    this.fold = k.fold[[i]]
    train.ds = ds[-c(this.fold),]
    train.cl = cl[-c(this.fold)]
    test.ds  = ds[c(this.fold),]
    test.cl  = cl[c(this.fold)]
    model = C5.0(train.ds,train.cl)
    result = predict(model, test.ds)
    acc[i] = mean(result==test.cl)
    rcl[i] = recall(result, test.cl) #
    pre[i] = precision(result, test.cl) #
  }
  if(all.index == FALSE){
    return(mean(acc))  # average accuracy of k=fold test
  }else if(all.index == TRUE){
    accuracy = mean(acc)
    recall = mean(rcl)
    precision = mean(pre)
    rtn = c(accuracy, recall, precision)
    names(rtn) = c("accuracy", "recall", "precision")
    return(rtn)
  }
}



kfold.knn <- function (ds,cl, fold=10, k.value=3, all.index = FALSE) {
  library(caret)   # for createFolds
  library(class)   # for knn
  set.seed(100)
  k.fold <-createFolds(as.vector(cl),k=fold)

  acc = c()              # classification result
  rcl = c() #recall
  pre = c() #precision
  for (i in 1:fold) {
    this.fold = k.fold[[i]]
    train.ds = ds[-c(this.fold),]
    train.cl = cl[-c(this.fold)]
    test.ds  = ds[c(this.fold),]
    test.cl  = cl[c(this.fold)]
    result = knn(train.ds, test.ds, train.cl, k=k.value)
    acc[i] = mean(result==test.cl)
    rcl[i] = recall(result, test.cl) #
    pre[i] = precision(result, test.cl) #

  }

  if(all.index == FALSE){
    return(mean(acc))  # average accuracy of k=fold test
  }else if(all.index == TRUE){
    accuracy = mean(acc)
    recall = mean(rcl)
    precision = mean(pre)
    rtn = c(accuracy, recall, precision)
    names(rtn) = c("accuracy", "recall", "precision")
    return(rtn)
  }

}



kfold.rf <- function (ds,cl, fold=10, all.index = FALSE) {
  library(caret)   # for createFolds
  library(randomForest)          # for rf
  set.seed(100)
  k.fold <-createFolds(as.vector(cl),k=fold)
  cl = factor(cl)
  acc = c()              # classification result
  rcl = c() #recall
  pre = c() #precision
  for (i in 1:fold) {
    this.fold = k.fold[[i]]
    train.ds = ds[-c(this.fold),]
    train.cl = cl[-c(this.fold)]
    test.ds  = ds[c(this.fold),]
    test.cl  = cl[c(this.fold)]
    model = randomForest(train.ds,train.cl)
    result = predict(model, test.ds)
    acc[i] = mean(result==test.cl)
    rcl[i] = recall(result, test.cl) #
    pre[i] = precision(result, test.cl) #
  }
  if(all.index == FALSE){
    return(mean(acc))  # average accuracy of k=fold test
  }else if(all.index == TRUE){
    accuracy = mean(acc)
    recall = mean(rcl)
    precision = mean(pre)
    rtn = c(accuracy, recall, precision)
    names(rtn) = c("accuracy", "recall", "precision")
    return(rtn)
  }

}



kfold.svm <- function (ds,cl, fold=10, ker = "radial", all.index = FALSE) {
  library(caret)   # for createFolds
  library("e1071")          # for SVM
  set.seed(100)
  k.fold <-createFolds(as.vector(cl),k=fold)
  cl = factor(cl)
  acc = c()              # classification result
  rcl = c() #recall
  pre = c() #precision
  for (i in 1:fold) {
    this.fold = k.fold[[i]]
    train.ds = ds[-c(this.fold),]
    train.cl = cl[-c(this.fold)]
    test.ds  = ds[c(this.fold),]
    test.cl  = cl[c(this.fold)]
    model = svm(train.ds,train.cl, kernel = ker)
    result = predict(model, test.ds)
    acc[i] = mean(result==test.cl)
    rcl[i] = recall(result, test.cl) #
    pre[i] = precision(result, test.cl) #

  }

  if(all.index == FALSE){
    return(mean(acc))  # average accuracy of k=fold test
  }else if(all.index == TRUE){
    accuracy = mean(acc)
    recall = mean(rcl)
    precision = mean(pre)
    rtn = c(accuracy, recall, precision)
    names(rtn) = c("accuracy", "recall", "precision")
    return(rtn)
  }


}



kfold.xgb <- function (ds,cl, fold=10, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", all.index = FALSE) {
  library(caret)   # for createFolds
  library(xgboost)
  set.seed(100)
  k.fold <-createFolds(as.vector(cl),k=fold)
  cl = factor(cl)
  acc = c()              # classification result
  for (i in 1:fold) {
    this.fold = k.fold[[i]]
    train.ds = ds[-c(this.fold),]
    train.cl = cl[-c(this.fold)]
    test.ds  = ds[c(this.fold),]
    test.cl  = cl[c(this.fold)]
    model = xgboost(as.matrix(train.ds),as.matrix(train.cl),
                    max_depth = max_depth, eta = eta, nthread = nthread,
                    nrounds = nrounds, objective = objective)
    pred = predict(model, test.ds)
    result = as.integer(pred > 0.5)
    acc[i] = mean(result==test.cl)
    rcl[i] = recall(result, test.cl) #
    pre[i] = precision(result, test.cl) #

  }

  if(all.index == FALSE){
    return(mean(acc))  # average accuracy of k=fold test
  }else if(all.index == TRUE){
    accuracy = mean(acc)
    recall = mean(rcl)
    precision = mean(pre)
    rtn = c(accuracy, recall, precision)
    names(rtn) = c("accuracy", "recall", "precision")
    return(rtn)
  }


}
